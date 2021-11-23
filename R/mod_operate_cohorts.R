#' operate_cohorts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom reactable reactableOutput
#' @importFrom shiny uiOutput hr tags htmlOutput plotOutput downloadButton
mod_operate_cohorts_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    reactable::reactableOutput(ns("cohorts_reactable")) %>% CohortOperationsShinyApp::ui_load_spiner(),
    htmltools::hr(),
    # TEMP FIX: shinyjqui::updateOrderInput does not update when shinyjqui::orderInput is in as_source = TRUE mode. Lets build the whole thing on server
    shiny::uiOutput(ns("operation_expresion")),
    shiny::hr(),
    shiny::tags$b("Operation Expresion: "),
    shiny::htmlOutput(ns("entry_cohort_names_text")),
    shiny::hr(),
    shiny::plotOutput(ns("upset_plot")) %>% CohortOperationsShinyApp::ui_load_spiner(),
    shiny::hr(),
    reactable::reactableOutput(ns("cohort_output_reactable")),
    shiny::downloadButton(ns("save_db"), "Save cohorts")
  )
}

#' operate_cohorts Server Functions
#'
#' @noRd
#' @importFrom FinnGenTableTypes empty_cohortData table_summarycohortData cohortData_union plot_upset_cohortData summarise_cohortData
#' @importFrom reactable renderReactable
#' @importFrom shinyWidgets pickerInput
#' @importFrom shinyjqui orderInput
mod_operate_cohorts_server <- function(id, r_cohorts) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # reactive variables
    #
    r <- shiny::reactiveValues(
      result_cohortData = FinnGenTableTypes::empty_cohortData()
    )


    #
    # updates output$cohorts_reactable with given r_cohorts
    #
    output$cohorts_reactable <- reactable::renderReactable({
      r_cohorts$summaryCohortData %>%
        FinnGenTableTypes::table_summarycohortData()
    })


    #
    # creates UI for defining operation expression (TEMP FIX)
    #
    output$operation_expresion <- shiny::renderUI({
      # req(r_cohorts$cohortData)

      cohort_names <- r_cohorts$cohortData %>%
        dplyr::distinct(COHORT_NAME) %>%
        dplyr::pull(COHORT_NAME)

      htmltools::tagList(
        shinyWidgets::pickerInput(
          ns("entry_cohort_names_picker"),
          "Select one or more entry cohorts",
          choices = cohort_names,
          multiple = TRUE
        ),
        shinyjqui::orderInput(
          ns("source_boxes"),
          "Operation Elements",
          items = c("(", ")", "AND-IN", "OR-IN", "NOT-IN", cohort_names),
          as_source = TRUE, connect = ns("dest_boxes")
        ),
        shinyjqui::orderInput(
          ns("dest_boxes"),
          "Operate entry cohorts",
          items = NULL, placeholder = "Drag operation elements here..."
        )
      )
    })


    #
    # calculates total expresion
    #
    output$entry_cohort_names_text <- shiny::renderText({
      result_expresion <- ""
      if (!is.null(input$entry_cohort_names_picker)) {
        result_expresion <- stringr::str_c("[", stringr::str_c(input$entry_cohort_names_picker, collapse = " | "), "]")
      }
      if (!is.null(input$dest_boxes)) {
        if (nchar(result_expresion) != 0) {
          result_expresion <- stringr::str_c(result_expresion, "&")
        }
        result_expresion <- stringr::str_c(result_expresion, "(", stringr::str_c(input$dest_boxes, collapse = " "), ")")
      }

      if (nchar(result_expresion) == 0) {
        result_expresion <- "<font color=\"#AFAFAF\"> Select entry cohorts or operation elements </font>"
      }
      result_expresion
    })


    #
    # plots overlap
    #
    output$upset_plot <- shiny::renderPlot({
      shiny::req(nrow(r_cohorts$cohortData) != 0)

      op_exp <- NULL
      if (!is.null(input$dest_boxes)) {
        reparsed_input_dest_boxes <- input$dest_boxes %>%
          stringr::str_c("`", ., "`") %>%
          stringr::str_replace("^`AND-IN`$", "&") %>%
          stringr::str_replace("^`OR-IN`$", "|") %>%
          stringr::str_replace("^`NOT-IN`$", "!") %>%
          stringr::str_replace("^`\\(`$", "(") %>%
          stringr::str_replace("^`\\)`$", ")")
        op_exp <- stringr::str_c(reparsed_input_dest_boxes, collapse = "")
      }

      result_operation <- tryCatch(
        {
          FinnGenTableTypes::cohortData_union(
            r_cohorts$cohortData,
            entry_cohorts = input$entry_cohort_names_picker,
            operation_expresion = op_exp
          )
        },
        error = function(e) e$message
      )

      # if operation failed, error
      if (!tibble::is_tibble(result_operation)) {
        r$result_cohortData <- FinnGenTableTypes::empty_cohortData()
        shiny::validate(shiny::need(FALSE, "Operation expresion is malformed"))
      }

      # if correct update result_cohortData and upset plot
      r$result_cohortData <- result_operation
      result_cohort_name <- result_operation %>%
        dplyr::distinct(COHORT_NAME) %>%
        dplyr::pull(COHORT_NAME)
      if (length(result_cohort_name) == 0) {
        result_cohort_name <- ""
      }

      FinnGenTableTypes::plot_upset_cohortData(
        dplyr::bind_rows(r_cohorts$cohortData, result_operation),
        output_cohort = result_cohort_name
      )
    })


    #
    # updates output$cohorts_reactable with given r_cohorts
    #
    output$cohort_output_reactable <- reactable::renderReactable({
      r$result_cohortData %>%
        FinnGenTableTypes::summarise_cohortData() %>%
        FinnGenTableTypes::table_summarycohortData()
    })


    #
    # download save_db : download cohortData
    #
    output$save_db <- shiny::downloadHandler(
      filename = "cohorts_from_cohortOperations.tsv",
      content = function(file) {
        readr::write_tsv(
          file = file,
          x = dplyr::bind_rows(r_cohorts$cohortData, r$result_cohortData)
        )
      }
    )
  })
}

# r_cohorts <- reactiveValues(
#   cohortData = test_cohortData,
#   summaryCohortData = FinnGenTableTypes::summarise_cohortData(test_cohortData)
# )
#
# shinyApp(
#   fluidPage(mod_operate_cohorts_ui("test")),
#   function(input,output,session){mod_operate_cohorts_server("test", r_cohorts)}
# )
