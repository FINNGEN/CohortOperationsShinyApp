#' import_cohort_endpoints UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_import_cohort_endpoints_ui <- function(id){
  ns <- NS(id)
  tagList(
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

#' import_cohort_endpoints Server Functions
#'
#' @noRd
mod_import_cohort_endpoints_server <- function(id, r_connection, r_cohorts){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    r <- shiny::reactiveValues(
      endpoint_cohorts_list = NULL
    )

    r_to_append <- shiny::reactiveValues(
      cohortData = NULL
    )

    #
    # creates picker in output$updated_selectinput
    #
    output$updated_selectinput <- shiny::renderUI({
      shiny::req(r_connection$phewas_conn)

      df_endpoint_choices <- names(r_connection$phewas_conn)

      if (length(df_endpoint_choices)==0) {
        shiny::selectInput(ns("database_picker"), "Select CDM database:", choices = NULL)
      } else {
        shiny::selectInput(ns("database_picker"), "Select CDM database:",
                           choices = df_endpoint_choices,
                           selected = df_endpoint_choices[1]
        )
      }
    })


    #
    # updates  r$endpoint_cohorts_list with the list of endpoits
    #
    shiny::observe({
      shiny::req(input$database_picker)

      r$endpoint_cohorts_list <- FGpheWAS::getEndpointNames(r_connection$phewas_conn[[input$database_picker]]) %>%
        dplyr::mutate(data = list(tibble(type = c("Cases", "Excludes", "Controls")))) %>%
        tidyr::unnest(data)
    })


    #
    # updates  table with endpoints sns types list
    #
    output$cohorts_reactable = reactable::renderReactable({
      shiny::req(r$endpoint_cohorts_list)

      r$endpoint_cohorts_list %>% reactable(
        groupBy = "ENDPOINT",
        selection = "multiple",
        searchable = TRUE
      )

    })

    #
    # reactive function to get selected values
    #
    r_selected_endpoint_cohorts <- reactive({
      shiny::req(r$endpoint_cohorts_list)
      r$endpoint_cohorts_list %>% slice(
        reactable::getReactableState("cohorts_reactable", "selected", session)
      )
    })


    #
    #  button click
    #
    observe({
      selected_endpoint_cohorts <- reactable::getReactableState("cohorts_reactable", "selected", session)
      shinyjs::toggleState("import_b", condition = !is.null(selected_endpoint_cohorts) )
    })

    shiny::observeEvent(input$import_b, {
      shiny::req(r$endpoint_cohorts_list)
      shiny::req(r_selected_endpoint_cohorts())
      selected_endpoint_cohorts <- r_selected_endpoint_cohorts()

      cdm_schema <- r_connection$cdm_webapi_conn$CdmSource$cdmDatabaseSchema

      ## Import cohorts
      CohortOperationsShinyApp::sweetAlert_spinner("Importing cohorts")

      tmp_r_imported_cohortData <- selected_endpoint_cohorts %>%
        dplyr::mutate(type = case_when(type == "Cases" ~ "case", type == "Excludes" ~ "excl",type == "Controls" ~ "control")) %>%
        dplyr::group_by(ENDPOINT) %>% summarise(types= list(type )) %>%
        #
        dplyr::mutate(data = purrr::map2(ENDPOINT, types, ~FGpheWAS::getEndpointCohort(r_connection$phewas_conn[[input$database_picker]],cdm_schema, ..1, ..2) )) %>%
        dplyr::select(data) %>%
        tidyr::unnest(data)

      if (get_golem_config("enviroment") == "atlas-development") {
        tmp_r_imported_cohortData <- tmp_r_imported_cohortData %>%
          # TEMPFIX
          dplyr::mutate(
            BIRTH_DATE = pmin(BIRTH_DATE, COHORT_START_DATE),
            DEATH_DATE = pmax(DEATH_DATE, COHORT_END_DATE)
          )
        # END TEMPFIX
      }
      # print(FinnGenTableTypes::is_cohortData(r$imported_cohortData, verbose = T))
      CohortOperationsShinyApp::remove_sweetAlert_spinner()

      # ## Check if any cohort has 0 patients
      # no_patients_cohorts <- dplyr::setdiff(
      #   selected_endpoint_cohorts %>% dplyr::pull(cohort_name),
      #   tmp_r_imported_cohortData %>% dplyr::count(COHORT_NAME) %>% dplyr::pull(COHORT_NAME)
      # )
      # print(selected_cohorts)
      # print(tmp_r_imported_cohortData)
      # if (length(no_patients_cohorts) != 0) {
      #   shinyWidgets::sendSweetAlert(
      #     session = session,
      #     type = "error",
      #     title = "Cohorts have 0 patients",
      #     text = shiny::HTML(
      #       "The following cohorts are COMPLETE, but have 0 patiets:  <ul>",
      #       stringr::str_c(stringr::str_c("<li> ", no_patients_cohorts, "</li>"), collapse = ""),
      #       "</ul> They will not be imported."
      #     ),
      #     html = TRUE
      #   )
      # }

      # copy to reactible will trigger next step
      if(nrow(tmp_r_imported_cohortData)==0){
        shiny::req(FALSE) # break reactivity
      }
      r_to_append$cohortData <- tmp_r_imported_cohortData

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

## To be copied in the UI
# mod_import_cohort_endpoints_ui("import_cohort_endpoints_ui_1")

## To be copied in the server
# mod_import_cohort_endpoints_server("import_cohort_endpoints_ui_1")
