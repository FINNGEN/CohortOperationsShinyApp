#' import_cohort_atlas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets useSweetAlert
#' @importFrom reactable reactableOutput
mod_import_cohort_atlas_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    # uses :
    shinyWidgets::useSweetAlert(),
    #
    shiny::br(),
    # TOFIX: couldnt make to updateSelectInput with in the module, this is plan B
    shiny::uiOutput(ns("updated_selectinput")),
    # selectInput(ns("database_picker"), "Select CDM database:", choices = NULL),
    reactable::reactableOutput(ns("cohorts_reactable")),
    shiny::hr(),
    shiny::actionButton(ns("import_b"), "Import Selected")
  )
}

#' import_cohort_atlas Server Functions
#'
#' @noRd
#' @importFrom CDMTools changeDatabase getListCohortNamesIds getCohortStatus getCohortData
#' @importFrom reactable renderReactable reactable
#' @importFrom shinyWidgets sendSweetAlert confirmSweetAlert
#' @importFrom FinnGenTableTypes summarise_cohortData
mod_import_cohort_atlas_server <- function(id, r_connection, r_cohorts) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # reactive variables
    #
    r <- shiny::reactiveValues(
      atlas_cohorts_list = NULL,
      imported_cohortData = NULL,
      asked_intersect_names = NULL
    )

    #
    # creates picker in output$updated_selectinput
    #
    output$updated_selectinput <- shiny::renderUI({
      shiny::req(r_connection$cdm_webapi_conn)

      is_webAPI_up <- r_connection$cdm_webapi_conn$conn_status_tibble %>%
        dplyr::filter(step == "Connection to webAPI") %>%
        dplyr::pull(error) %>%
        !.

      if (!is_webAPI_up) {
        shiny::selectInput(ns("database_picker"), "Select CDM database:", choices = NULL)
      } else {
        shiny::selectInput(ns("database_picker"), "Select CDM database:",
          choices = r_connection$cdm_webapi_conn$CdmSources %>% dplyr::pull(sourceKey),
          selected = r_connection$cdm_webapi_conn$CdmSource$sourceKey
        )
      }
    })

    #
    # updates r_connection$cdm_webapi_conn with database_picker and refreshes r$atlas_cohorts_list
    #
    shiny::observeEvent(input$database_picker, {
      is_webAPI_up <- r_connection$cdm_webapi_conn$conn_status_tibble %>%
        dplyr::filter(step == "Connection to webAPI") %>%
        dplyr::pull(error) %>%
        !.

      shiny::req(is_webAPI_up)

      r_connection$cdm_webapi_conn <- CDMTools::changeDatabase(r_connection$cdm_webapi_conn, input$database_picker)

      r$atlas_cohorts_list <- CDMTools::getListCohortNamesIds(r_connection$cdm_webapi_conn) %>%
        dplyr::arrange(dplyr::desc(cohort_id))
    })

    #
    # updates table with r$atlas_cohorts_list change
    #
    output$cohorts_reactable <- reactable::renderReactable({
      shiny::validate(shiny::need(r$atlas_cohorts_list, "Couldn't connect to webAPI. Check Info tab for details."))
      r$atlas_cohorts_list %>%
        reactable::reactable(
          selectionId = ns("selected_index"), selection = "multiple", onClick = "select",
          searchable = TRUE
        )
    })

    #
    # button import selected: checks selected cohorts
    #
    observe({
      shinyjs::toggleState("import_b", condition = shiny::isTruthy(input$selected_index) )
    })

    shiny::observeEvent(input$import_b, {
      shiny::req(input$selected_index)

      selected_cohorts <- r$atlas_cohorts_list %>% dplyr::slice(input$selected_index)

      ## Check status of selected cohorts
      CohortOperationsShinyApp::sweetAlert_spiner("Checking cohorts' status")

      n_selected <- nrow(selected_cohorts)
      for (i in 1:n_selected) {
        selected_cohorts[i, "status"] <- CDMTools::getCohortStatus(r_connection$cdm_webapi_conn, selected_cohorts[[i, "cohort_id"]])
      }
      CohortOperationsShinyApp::remove_sweetAlert_spiner()

      # if any of the status is not COMPLETED error user
      not_compleated_cohorts <- selected_cohorts %>%
        dplyr::filter(status != "COMPLETE") %>%
        dplyr::pull(cohort_name)
      if (length(not_compleated_cohorts) != 0) {
        shinyWidgets::sendSweetAlert(
          session = session,
          type = "error",
          title = "Cohorts not COMPLETE",
          text = shiny::HTML(
            "The following cohorts are not COMPLETE:  <ul>",
            stringr::str_c(stringr::str_c("<li> ", not_compleated_cohorts, "</li>"), collapse = ""),
            "</ul> Please, go to Atlas and run them for database: ", input$database_picker
          ),
          html = TRUE
        )
        shiny::req(FALSE) # break reactivity
      }

      ## Import cohorts
      CohortOperationsShinyApp::sweetAlert_spiner("Importing cohorts")

      tmp_r_imported_cohortData <- CDMTools::getCohortData(r_connection$cdm_webapi_conn, selected_cohorts %>% dplyr::pull(cohort_id))
      if (CohortOperationsShinyApp::get_golem_config("enviroment") == "atlas-development") {
        tmp_r_imported_cohortData <- tmp_r_imported_cohortData %>%
          # TEMPFIX
          dplyr::mutate(
            BIRTH_DATE = pmin(BIRTH_DATE, COHORT_START_DATE),
            DEATH_DATE = pmax(DEATH_DATE, COHORT_END_DATE)
          )

        # TEMPFIX
      }
      r$imported_cohortData <- tmp_r_imported_cohortData
      # print(FinnGenTableTypes::is_cohortData(r$imported_cohortData, verbose = T))
      CohortOperationsShinyApp::remove_sweetAlert_spiner()

      # ask if existing cohorts should be replaced
      intersect_names <- dplyr::inner_join(
        r_cohorts$cohortData %>% dplyr::distinct(COHORT_SOURCE, COHORT_NAME),
        r$imported_cohortData %>% dplyr::distinct(COHORT_SOURCE, COHORT_NAME),
        by = c("COHORT_SOURCE", "COHORT_NAME")
      ) %>%
        dplyr::mutate(name = stringr::str_c(COHORT_NAME, ", From: ", COHORT_SOURCE)) %>%
        dplyr::pull(name)


      if (length(intersect_names) > 0) {
        shinyWidgets::confirmSweetAlert(
          session = session,
          inputId = "asked_intersect_names_alert",
          type = "question",
          title = "Some selected cohorts had been alredy imported:",
          text = shiny::HTML(
            "The following cohorts had been alredy imported: <ul>",
            stringr::str_c(stringr::str_c("<li> ", intersect_names, "</li>"), collapse = ""),
            "</ul> Should these be replaced or ignored."
          ),
          btn_labels = c("Not-import", "Replace"),
          html = TRUE
        )
      } else {
        r$asked_intersect_names <- TRUE
      }
    })

    # just pass the info to make it writable
    shiny::observe({
      r$asked_intersect_names <- input$asked_intersect_names_alert
    })

    #
    # confirmSweetAlert asked_intersect_names
    #
    shiny::observeEvent(r$asked_intersect_names, {
      # take names from main and imported
      intersect_names <- dplyr::inner_join(
        r_cohorts$cohortData %>% dplyr::distinct(COHORT_SOURCE, COHORT_NAME),
        r$imported_cohortData %>% dplyr::distinct(COHORT_SOURCE, COHORT_NAME),
        by = c("COHORT_SOURCE", "COHORT_NAME")
      )

      if (r$asked_intersect_names) {
        # remove from main
        r_cohorts$cohortData <- dplyr::anti_join(
          r_cohorts$cohortData,
          intersect_names,
          by = c("COHORT_SOURCE", "COHORT_NAME")
        )
      } else {
        r$imported_cohortData <- dplyr::anti_join(
          r$imported_cohortData,
          intersect_names,
          by = c("COHORT_SOURCE", "COHORT_NAME")
        )
      }

      r_cohorts$cohortData <- dplyr::bind_rows(
        r_cohorts$cohortData,
        r$imported_cohortData
      )
      # re calcualte all, to get the rigth years in dates
      r_cohorts$summaryCohortData <- FinnGenTableTypes::summarise_cohortData(r_cohorts$cohortData)

      .close_and_reset()
    })


    .close_and_reset <- function() {
      # r$imported_cohortData = NULL
      # r$atlas_cohorts_list <- NULL
      r$imported_cohortData <- NULL
      r$asked_intersect_names <- NULL
    }
  })
}
#
# #
# # # # # # no connection
# Sys.setenv(GOLEM_CONFIG_ACTIVE="dev_laptop_javier")
# r_connection <- reactiveValues(cdm_webapi_conn = configCDMTools())
#
#
# r_cohorts <- reactiveValues(
#   cohortData = test_cohortData,
#   summaryCohortData = FinnGenTableTypes::summarise_cohortData(test_cohortData)
# )
#
# shinyApp(
#   fluidPage(mod_import_cohort_atlas_ui("test")),
#   function(input,output,session){mod_import_cohort_atlas_server("test",r_connection, r_cohorts)}
# )
