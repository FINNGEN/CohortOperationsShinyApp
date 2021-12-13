#' import_cohort_atlas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList br uiOutput hr actionButton
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets useSweetAlert
#' @importFrom reactable reactableOutput
mod_import_cohort_atlas_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    use_mod_append_cohort_ui(),
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
#' @importFrom shiny moduleServer reactiveValues renderUI req selectInput observeEvent validate need HTML
#' @importFrom dplyr filter pull arrange desc slice mutate
#' @importFrom CDMTools changeDatabase getListCohortNamesIds getCohortStatus getCohortData
#' @importFrom reactable renderReactable reactable getReactableState updateReactable
#' @importFrom shinyjs toggleState
#' @importFrom shinyWidgets sendSweetAlert
#' @importFrom stringr str_c
mod_import_cohort_atlas_server <- function(id, r_connection, r_cohorts) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # reactive variables
    #
    r <- shiny::reactiveValues(
      atlas_cohorts_list = NULL
    )

    r_to_append <- shiny::reactiveValues(
      cohortData = NULL
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
          selection = "multiple", onClick = "select",
          searchable = TRUE
        )
    })
    # reactive function to get selected values
    r_selected_index <- reactive(reactable::getReactableState("cohorts_reactable", "selected", session))

    #
    # button import selected: checks selected cohorts
    #
    observe({
      shinyjs::toggleState("import_b", condition = !is.null(r_selected_index()) )
    })

    shiny::observeEvent(input$import_b, {
      shiny::req(r_selected_index())

      selected_cohorts <- r$atlas_cohorts_list %>% dplyr::slice(r_selected_index())

      ## Check status of selected cohorts
      CohortOperationsShinyApp::sweetAlert_spinner("Checking cohorts' status")

      n_selected <- nrow(selected_cohorts)
      for (i in 1:n_selected) {
        selected_cohorts[i, "status"] <- CDMTools::getCohortStatus(r_connection$cdm_webapi_conn, selected_cohorts[[i, "cohort_id"]])
      }
      CohortOperationsShinyApp::remove_sweetAlert_spinner()

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
      CohortOperationsShinyApp::sweetAlert_spinner("Importing cohorts")

      tmp_r_imported_cohortData <- CDMTools::getCohortData(r_connection$cdm_webapi_conn, selected_cohorts %>% dplyr::pull(cohort_id))
      if (get_golem_config("enviroment") == "atlas-development") {
        tmp_r_imported_cohortData <- tmp_r_imported_cohortData %>%
          # TEMPFIX
          dplyr::mutate(
            BIRTH_DATE = pmin(BIRTH_DATE, COHORT_START_DATE),
            DEATH_DATE = pmax(DEATH_DATE, COHORT_END_DATE)
          )

        # TEMPFIX
      }
      r_to_append$cohortData <- tmp_r_imported_cohortData
      # print(FinnGenTableTypes::is_cohortData(r$imported_cohortData, verbose = T))
      CohortOperationsShinyApp::remove_sweetAlert_spinner()

    })

    #
    # evaluate the cohorts to append; if accepted increase output to trigger closing acctions
    #
    r_append_accepted_counter <- mod_append_cohort_server("impor_atlas", r_cohorts, r_to_append )

    # close and reset
    shiny::observeEvent(r_append_accepted_counter(), {
      r_to_append$cohortData <- NULL
      reactable::updateReactable("cohorts_reactable", selected = NA, session = session )
    })



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
