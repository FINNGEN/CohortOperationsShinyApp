#' import_cohorts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom reactable reactableOutput
#' @importFrom shiny actionButton
mod_import_cohorts_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    reactable::reactableOutput(ns("summaryCohortsData_reactable")) %>%
      CohortOperationsShinyApp::ui_load_spiner(),
    htmltools::hr(),
    #
    shiny::actionButton(ns("import_b"), "Import cohorts"), # calls modal_import_cohorts
    shiny::actionButton(ns("delete_b"), "Delete selected")
  )
}

#' import_cohorts Server Functions
#'
#' @noRd
#' @importFrom reactable renderReactable
#' @importFrom FinnGenTableTypes table_summarycohortData
#' @importFrom shinyWidgets confirmSweetAlert
mod_import_cohorts_server <- function(id, r_connection, r_cohorts) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # Updates summaryCohortsData_reactable
    #
    output$summaryCohortsData_reactable <- reactable::renderReactable({
      # FinnGenTableTypes::is_cohortData(r_cohorts$cohortData, verbose = TRUE)
      r_cohorts$summaryCohortData %>%
        FinnGenTableTypes::table_summarycohortData(
          selection = "multiple",
          selectionId = ns("cohortdata_check")
        )
    })

    #
    # button import_b: launch modal
    #
    shiny::observeEvent(input$import_b, {
      shiny::showModal(CohortOperationsShinyApp::modal_import_cohorts(ns))
    })
    # Module calls used in modal
    CohortOperationsShinyApp::mod_import_cohort_file_server("mod_import_cohort_file", r_cohorts)
    CohortOperationsShinyApp::mod_import_cohort_atlas_server("mod_import_cohort_atlas", r_connection, r_cohorts)

    #
    # button delete_b: launch confirmation alert
    #
    shiny::observeEvent(input$delete_b, {
      shiny::req(input$cohortdata_check)
      to_delete_names <- r_cohorts$summaryCohortData %>%
        dplyr::slice(input$cohortdata_check) %>%
        dplyr::pull(COHORT_NAME)
      shiny::req(to_delete_names)

      shinyWidgets::confirmSweetAlert(
        session = session,
        inputId = ns("ask_delete_alert"),
        type = "question",
        title = "Delete cohort ?",
        text = htmltools::HTML(
          "Are you sure you want to delete the following cohorts: <ul>",
          stringr::str_c(stringr::str_c("<li> ", to_delete_names, "</li>"), collapse = ""),
          "</ul>"
        ),
        btn_labels = c("Cancel", "Delete"),
        html = TRUE
      )
    })

    shiny::observeEvent(input$ask_delete_alert, {
      if (input$ask_delete_alert) {
        to_delete_names <- r_cohorts$summaryCohortData %>%
          dplyr::slice(input$cohortdata_check) %>%
          dplyr::pull(COHORT_NAME)

        r_cohorts$cohortData <- r_cohorts$cohortData %>% dplyr::filter(!(COHORT_NAME %in% to_delete_names))
        r_cohorts$summaryCohortData <- r_cohorts$summaryCohortData %>% dplyr::filter(!(COHORT_NAME %in% to_delete_names))
      }
    })
  })
}

# MODAL called by importcohorts_import_b
modal_import_cohorts <- function(ns) {
  shiny::modalDialog(
    size = "l",
    title = "Import cohorts",
    footer = NULL,
    easyClose = FALSE,
    #
    shiny::tabsetPanel(
      type = "tabs",
      # panel FILE
      shiny::tabPanel(
        "from File",
        htmltools::h2("This is possible if file in cohortTable format"),
        CohortOperationsShinyApp::mod_import_cohort_file_ui(ns("mod_import_cohort_file"))
      ),
      # panel ATLAS
      shiny::tabPanel(
        "from Atlas",
        CohortOperationsShinyApp::mod_import_cohort_atlas_ui(ns("mod_import_cohort_atlas"))
      ),
      # panel ENDPOINT
      shiny::tabPanel(
        "from Endploint",
        htmltools::h2("Possible if endpoint results in a BQ database in cohortTable format ")
      )
    )
  )
}


#
# # no connection
# Sys.setenv(GOLEM_CONFIG_ACTIVE="dev_no_connection")
# r_connection <- reactiveValues(cdm_webapi_conn = configCDMTools())
#
#
# r_cohorts <- reactiveValues(
#   cohortData = test_cohortData,
#   summaryCohortData = FinnGenTableTypes::summarise_cohortData(test_cohortData)
# )
# shinyApp(
#   fluidPage(mod_import_cohorts_ui("test")),
#   function(input,output,session){mod_import_cohorts_server("test", r_connection, r_cohorts)}
# )
