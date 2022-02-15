#' operate_cohorts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS uiOutput hr tags htmlOutput plotOutput actionButton
#' @importFrom htmltools tagList
#' @importFrom shinyWidgets useSweetAlert
#' @importFrom shinyjs useShinyjs
#' @importFrom reactable reactableOutput
mod_operate_cohorts_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    use_mod_append_cohort_ui(),
    shinyWidgets::useSweetAlert(),
    shinyjs::useShinyjs(),
    # TEMP FIX: shinyjqui::updateOrderInput does not update when shinyjqui::orderInput is in as_source = TRUE mode. Lets build the whole thing on server
    shiny::uiOutput(ns("operation_expresion")),
    shiny::hr(),
    shiny::tags$b("Operation Expresion: "),
    shiny::htmlOutput(ns("entry_cohort_names_text")),
    shiny::hr(),
    shiny::plotOutput(ns("upset_plot")) %>%
      CohortOperationsShinyApp::ui_load_spinner(),
    shiny::hr(),
    shiny::tags$b("Result cohort: "),
    reactable::reactableOutput(ns("cohort_output_reactable")),
    shiny::actionButton(ns("copy_b"), "Copy result cohort to workbech")
  )
}

#' operate_cohorts Server Functions
#'
#' @noRd
#' @importFrom shiny moduleServer reactiveValues observe renderUI fluidRow column req renderText reactive renderPlot validate need observeEvent
#' @importFrom FinnGenTableTypes empty_cohortData summarise_cohortData cohortData_union plot_upset_cohortsOverlap table_summarycohortData
#' @importFrom dplyr filter distinct pull mutate
#' @importFrom htmltools tagList
#' @importFrom shinyWidgets pickerInput radioGroupButtons updatePickerInput
#' @importFrom shinyjqui orderInput
#' @importFrom stringr str_c str_replace str_replace_all
#' @importFrom reactable renderReactable
#' @importFrom shinyjs toggleState
mod_operate_cohorts_server <- function(id, r_cohorts) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # reactive variables
    #
    r_to_operate <- shiny::reactiveValues(
      cohortData = FinnGenTableTypes::empty_cohortData(),
      summaryCohortData = FinnGenTableTypes::empty_cohortData() %>% FinnGenTableTypes::summarise_cohortData()
    )

    r_to_append <- shiny::reactiveValues(
      cohortData = FinnGenTableTypes::empty_cohortData()
    )

    #
    # r_to_operate only the cohorts that are not from cohortOperations
    #
    shiny::observe({
      r_to_operate$cohortData <- r_cohorts$cohortData %>% dplyr::filter(COHORT_SOURCE!="CohortOperation")
      r_to_operate$summaryCohortData <- r_cohorts$summaryCohortData %>% dplyr::filter(COHORT_SOURCE!="CohortOperation")
    })

    #
    # creates UI for defining operation expression (TEMP FIX)
    #
    output$operation_expresion <- shiny::renderUI({
      cohort_names <- r_to_operate$cohortData %>%
        dplyr::distinct(COHORT_NAME) %>%
        dplyr::pull(COHORT_NAME)

      cohort_names_time <- r_to_operate$cohortData %>%
        dplyr::filter(!is.na(COHORT_START_DATE)) %>%
        dplyr::distinct(COHORT_NAME) %>%
        dplyr::pull(COHORT_NAME)

      htmltools::tagList(
        shiny::h5("Entry Cohorts:"),
        shiny::fluidRow(
          shiny::column(
            4,
            shinyWidgets::pickerInput(
              inputId = ns("entry_cohort_names_picker"),
              label = "Select entry cohorts",
              choices = cohort_names_time,
              multiple = TRUE
            )
          ),
          shiny::column(
            4,
            shinyWidgets::radioGroupButtons(
              inputId = ns("entry_cohort_start_rb"),
              label = "New cohorts starts with:",
              choices = c("first", "last"),
              selected = "first",
              status = "primary",
              checkIcon = list(
                yes = icon("ok", lib = "glyphicon"),
                no = icon("remove", lib = "glyphicon")
              )
            )
          ),
          shiny::column(
            4,
            shinyWidgets::radioGroupButtons(
              inputId = ns("entry_cohort_end_rb"),
              label = "New cohorts ends with: ",
              choices = c("first", "last"),
              selected = "last",
              status = "primary",
              checkIcon = list(
                yes = icon("ok", lib = "glyphicon"),
                no = icon("remove", lib = "glyphicon")
              )
            )
          )
        ),
        shiny::h5("Operation Cohorts:"),
        shinyjqui::orderInput(
          inputId = ns("source_boxes"),
          label = "Operation Elements",
          items = c("(", ")", "AND-IN", "OR-IN", "NOT-IN", cohort_names),
          as_source = TRUE, connect = ns("dest_boxes")
        ),
        shinyjqui::orderInput(
          inputId = ns("dest_boxes"),
          label = "Operate entry cohorts",
          items = NULL, placeholder = "Drag operation elements here..."
        )
      )
    })

    #
    # calculates total expresion
    #
    r_result_expresion <- reactive({
      shiny::req(input$entry_cohort_start_rb)

      result_expresion <- ""
      if (!is.null(input$entry_cohort_names_picker)) {
        result_expresion <- stringr::str_c("[", stringr::str_c(input$entry_cohort_names_picker, collapse = " OR-IN "), "]")
      }
      if (!is.null(input$dest_boxes)) {
        if (nchar(result_expresion) != 0) {
          result_expresion <- stringr::str_c(result_expresion, " AND-IN ")
        }
        result_expresion <- stringr::str_c(result_expresion, "(", stringr::str_c(input$dest_boxes, collapse = " "), ")")
      }

      if (nchar(result_expresion) == 0) {
        result_expresion <- "<font color=\"#AFAFAF\"> Select entry cohorts or operation elements </font>"
      }
      result_expresion

    })

    output$entry_cohort_names_text <- shiny::renderText({
      r_result_expresion()
    })


    #
    # Runs cohortData_union
    #
    r_result_operation <- shiny::reactive({
      shiny::req(input$entry_cohort_start_rb)
      shiny::req(nrow(r_to_operate$cohortData) != 0)

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
            r_to_operate$cohortData,
            entry_cohorts = input$entry_cohort_names_picker,
            operation_expresion = op_exp,
            entry_cohorts_start_date = input$entry_cohort_start_rb,
            entry_cohorts_end_date = input$entry_cohort_end_rb
          )
        },
        error = function(e){warning("mod_operate_cohorts: r_result_operation: trycatch: ",e$message);e$message}
      )

      if(!is.character(result_operation)){
        result_operation$new_cohortData <- result_operation$new_cohortData %>%
          dplyr::mutate(COHORT_NAME = r_result_expresion() %>% stringr::str_replace_all("[:blank:]","_"))
      }

      result_operation
    })


    #
    # plots upset overlap
    #
    output$upset_plot <- shiny::renderPlot({
      result_operation <- r_result_operation()
      # if operation failed, show error
      shiny::validate(shiny::need(!is.character(result_operation), "Operation expresion is malformed"))

      FinnGenTableTypes::plot_upset_cohortsOverlap(result_operation$cohortsOverlap)
    })

    #
    # plots new cohort table
    #
    output$cohort_output_reactable <- reactable::renderReactable({
      result_operation <- r_result_operation()

      if(is.character(result_operation)){
        FinnGenTableTypes::empty_cohortData() %>%
          FinnGenTableTypes::summarise_cohortData() %>%
          FinnGenTableTypes::table_summarycohortData()
      }else{
        result_operation$new_cohortData %>%
          FinnGenTableTypes::summarise_cohortData() %>%
          FinnGenTableTypes::table_summarycohortData()
      }
    })


    #
    # action button copy_b : append cohort
    #
    observe({
      result_operation <- r_result_operation()
      condition <- !is.character(result_operation)
      if(condition) condition <- (nrow(result_operation$new_cohortData) != 0)
      shinyjs::toggleState("copy_b", condition = condition )
    })

    observeEvent(input$copy_b, {
      result_operation <- r_result_operation()
      r_to_append$cohortData <- result_operation$new_cohortData
    })

    #
    # evaluate the cohorts to append; if accepted increase output to trigger closing actions
    #
    r_append_accepted_counter <- mod_append_cohort_server("operated_cohort", r_cohorts, r_to_append )

    # close and reset
    shiny::observeEvent(r_append_accepted_counter(), {
      shiny::req(r_append_accepted_counter()!=0)
      r_to_append$cohortData <- NULL

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "entry_cohort_names_picker",
        selected =  TRUE
      )

      .updateOrderInput(
        session = session,
        inputId = "dest_boxes",
        items = NULL
      )

      # shinyjs::reset("file_fi")
      # r_file$tmp_file <- NULL
      # r_file$imported_cohortData <- NULL
      # reactable::updateReactable("cohorts_reactable", selected = NA, session = session )
    })


  })
}


#FIX: this is a fix for shinyjqui::updateOrderInput
# as stated in the docummentation shinyjqui::updateOrderInput does not update items to NULL
# this is necessary to empty the operation expresion
.updateOrderInput <- function (session, inputId, label = NULL,
                               items = NULL, connect = NULL,
                               item_class = NULL) {
  item_class = match.arg(item_class,
                         c("default", "primary", "success",
                           "info", "warning", "danger"))
  # if(!is.null(items)) {
  items <- shinyjqui:::digestItems(items)
  #}
  if(!is.null(connect)){
    if(connect == FALSE) {
      connect <- "false"
    } else {
      connect <- paste0("#", connect, collapse = ", ")
    }
  }
  message <- list(label      = label,
                  items      = items,
                  connect    = connect,
                  item_class = item_class)
  message <- Filter(Negate(is.null), message)
  session$sendInputMessage(inputId, message)
}
