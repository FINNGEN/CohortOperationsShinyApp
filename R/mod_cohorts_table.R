#' cohorts_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS actionButton downloadButton
#' @importFrom htmltools tagList hr
#' @importFrom shinyjs useShinyjs
#' @importFrom reactable reactableOutput
mod_cohorts_table_ui <- function(id){
  ns <- shiny::NS(id)
  htmltools::tagList(
    shinyjs::useShinyjs(),
    #
    uiOutput(ns("ui_wrap"))
  )
}

#' cohorts_table Server Functions
#'
#' @noRd
#' @importFrom reactable renderReactable getReactableState
#' @importFrom FinnGenTableTypes table_summarycohortData
#' @importFrom shinyjs toggleState
#' @importFrom shiny observeEvent req downloadHandler
#' @importFrom dplyr slice pull filter
#' @importFrom shinyWidgets confirmSweetAlert
#' @importFrom htmltools HTML
#' @importFrom stringr str_c
#' @importFrom readr write_tsv
mod_cohorts_table_server <- function(id, r_cohorts,  table_editing=TRUE){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #
    # build ui
    #
    output$ui_wrap <- renderUI({
      if(table_editing){
        htmltools::tagList(
          reactable::reactableOutput(ns("summaryCohortsData_reactable")) %>%
            CohortOperationsShinyApp::ui_load_spinner(),
          htmltools::hr(),
          shiny::actionButton(ns("delete_b"), "Delete selected cohorts"),
          shiny::downloadButton(ns("download_db"), "Download selected cohorts")
        )
      }else{
        htmltools::tagList(
          reactable::reactableOutput(ns("summaryCohortsData_reactable")) %>%
            CohortOperationsShinyApp::ui_load_spinner()
        )
      }
    })

    #
    # Updates summaryCohortsData_reactable
    #
    output$summaryCohortsData_reactable <- reactable::renderReactable({
      # FinnGenTableTypes::is_cohortData(r_cohorts$cohortData, verbose = TRUE)
      if(table_editing){
        r_cohorts$summaryCohortData %>%
          FinnGenTableTypes::table_summarycohortData(
            selection = "multiple",
            onClick = "select"
          )
      }else{
        r_cohorts$summaryCohortData %>%
          FinnGenTableTypes::table_summarycohortData( )
      }
    })
    # reactive function to get selected values
    selected_cohorts <- reactive(reactable::getReactableState("summaryCohortsData_reactable", "selected", session))

    #
    # shinyjs enable disable buttons
    #
    observe({
      shinyjs::toggleState("delete_b", condition = !is.null(selected_cohorts()) )
      shinyjs::toggleState("download_db", condition = !is.null(selected_cohorts()) )
    })

    #
    # button delete_b: launch confirmation alert
    #
    shiny::observeEvent(input$delete_b, {
      shiny::req(selected_cohorts())
      selected_names <- r_cohorts$summaryCohortData %>%
        dplyr::slice(selected_cohorts()) %>%
        dplyr::pull(COHORT_NAME)
      shiny::req(selected_names)

      shinyWidgets::confirmSweetAlert(
        session = session,
        inputId = ns("ask_delete_alert"),
        type = "question",
        title = "Delete cohort ?",
        text = htmltools::HTML(
          "Are you sure you want to delete the following cohorts: <ul>",
          stringr::str_c(stringr::str_c("<li> ", selected_names, "</li>"), collapse = ""),
          "</ul>"
        ),
        btn_labels = c("Cancel", "Delete"),
        html = TRUE
      )
    })

    shiny::observeEvent(input$ask_delete_alert, {
      if (input$ask_delete_alert) {
        selected_names <- r_cohorts$summaryCohortData %>%
          dplyr::slice(selected_cohorts()) %>%
          dplyr::pull(COHORT_NAME)

        r_cohorts$cohortData <- r_cohorts$cohortData %>% dplyr::filter(!(COHORT_NAME %in% selected_names))
        r_cohorts$summaryCohortData <- r_cohorts$summaryCohortData %>% dplyr::filter(!(COHORT_NAME %in% selected_names))
      }
    })


    #
    # button download_b: download selected cohorts
    #
    output$download_db <- shiny::downloadHandler(
      filename = "cohorts_from_cohortOperations.tsv",
      content = function(file) {
        selected_names <- r_cohorts$summaryCohortData %>%
          dplyr::slice(selected_cohorts()) %>%
          dplyr::pull(COHORT_NAME)

        readr::write_tsv(
          file = file,
          x = r_cohorts$cohortData %>% dplyr::filter(COHORT_NAME %in% selected_names)
        )
      }
    )
  })
}

#
# r_cohorts <- reactiveValues(
#   cohortData = test_cohortData,
#   summaryCohortData = FinnGenTableTypes::summarise_cohortData(test_cohortData)
# )
# shinyApp(
#   fluidPage(mod_cohorts_table_ui("test")),
#   function(input,output,session){mod_cohorts_table_server("test", r_cohorts)}
# )
