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
    #
    # TEMP FIX: shinyjqui::updateOrderInput does not update when shinyjqui::orderInput is in as_source = TRUE mode. Lets build the whole thing on server
    shiny::uiOutput(ns("operation_expresion")),
    shiny::hr(),
    shiny::tags$h4("New cohort's name "),
    #shiny::textInput(ns("new_cohort_name_text"), label = NULL, width="100%"),
    shiny::textOutput(ns("new_cohort_name_text")),
    shiny::br(),
    shiny::br(),
    shiny::actionButton(ns("create_new_cohort_b"), "Create New Cohort"),
    shiny::hr(),
    shiny::tags$h4("Result cohort: "),
    shiny::uiOutput(ns("cohort_result_ui"))
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

    r_results_operation <- shiny::reactiveValues(
      results=NULL
      )

    #
    # r_to_operate only the cohorts that are not from cohortOperations
    #
    shiny::observe({
      r_to_operate$cohortData <- r_cohorts$cohortData #%>% dplyr::filter(COHORT_SOURCE!="CohortOperation")
      r_to_operate$summaryCohortData <- r_cohorts$summaryCohortData# %>% dplyr::filter(COHORT_SOURCE!="CohortOperation")
      print(r_to_operate$summaryCohortData)
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
        shiny::h4("Select new cohort's entry and exit:"),
        shiny::p("The new cohort takes "),
        shinyWidgets::pickerInput(
          inputId = ns("entry_cohort_start_rb"),
          label = NULL,
          choices = list(`the earliest COHORT_START_DATE`="earliest",
                         `the latest COHORT_START_DATE`="latest",
                         `all the events`="all_events"),
          selected = "earliest"
        ),
        shiny::conditionalPanel(
          "input.entry_cohort_start_rb != 'all_events'",
          shiny::p(" and "),
          shinyWidgets::pickerInput(
            inputId = ns("entry_cohort_end_rb"),
            label = NULL,
            choices = list(`the earliest COHORT_END_DATE`="earliest",
                           `the latest COHORT_END_DATE`="latest"),
            selected = "latest"
          ),
          ns=ns
        ),
        shiny::p(" from  "),

        shinyWidgets::pickerInput(
          inputId = ns("entry_cohort_names_picker"),
          label = NULL,
          choices = cohort_names_time,
          multiple = TRUE,
          options = shinyWidgets::pickerOptions(
            multipleSeparator=" or ",
            noneSelectedText="Select one or more entry cohorts")
        ),



        shiny::h4("Select new cohort's patients:"),
        shiny::column(
          8,
          shiny::textOutput(ns("select_patients_start_text")),
          shinyjqui::orderInput(
            inputId = ns("dest_boxes"),
            label = NULL,
            items = NULL, placeholder = "Continue the sentence by draging here elements from the right -> "
          )
        ),
        shiny::column(
          4,
          shiny::p("Operation elements"),
          shinyjqui::orderInput(
            inputId = ns("source_boxes_cohorts"),
            label = "Cohorts",
            items = {a <- as.list(cohort_names %>% stringr::str_c("`",.,"`")); names(a) <- cohort_names; a},
            as_source = TRUE, connect = ns("dest_boxes")
          ),
          shinyjqui::orderInput(
            inputId = ns("source_boxes"),
            label = "Operations",
            items = c(`(`="(", `)`=")",
                      ` if they are also in `="&",
                      ` if they are not in `="&!",
                      ` or in ` = "|"),
            as_source = TRUE, connect = ns("dest_boxes")
          )
        ),

      )
    })

    #
    # calculates cohort entry expression
    #
    r_entry_cohort_expresion <- shiny::reactive({
      #shiny::req(input$entry_cohort_start_rb)

      entry_cohort_expresion <- ""
      if (!is.null(input$entry_cohort_names_picker)) {
        entry_cohort_expresion <- stringr::str_c(input$entry_cohort_names_picker, collapse = " or ")
      }

    })

    output$select_patients_start_text <- shiny::renderText({
      paste("The new cohort takes any of the patients in ", r_entry_cohort_expresion(), "...")
    })

    #
    # calculates cohort operation expression
    #
    r_operation_expresion <- shiny::reactive({

      operation_expresion<-NULL
      if (!is.null(input$dest_boxes)) {
        operation_expresion <- stringr::str_c(input$dest_boxes, collapse = "")
      }
      operation_expresion

    })

    #
    # calculates cohort default name expression
    #
    r_default_name <- shiny::reactive({

      r_results_operation$results <- NULL

      default_name <- ""
      if(shiny::isTruthy(r_entry_cohort_expresion())){
        entry_name <- ifelse(input$entry_cohort_start_rb=="all_events",
                             "all events from ",
                             paste(input$entry_cohort_start_rb, "entry and", input$entry_cohort_end_rb, "exit from"))
        default_name <- paste("[",entry_name,r_entry_cohort_expresion(),"] ")
      }
      if(shiny::isTruthy(r_operation_expresion())){
        default_name <- paste0(
          default_name,
          r_operation_expresion() %>%
            stringr::str_replace_all("\\&\\!`", " if they are not in ") %>%
            stringr::str_replace_all("\\&`", " if they are also in ")  %>%
            stringr::str_replace_all("\\|`", " or in ") %>%
            stringr::str_replace_all("`", "")
        )
      }
      print(default_name)
      default_name

    })

    # observe({
    #   shiny::updateTextInput(session, "new_cohort_name_text", value=r_default_name())
    # })
    output$new_cohort_name_text <- shiny::renderText({r_default_name()})


    #
    # Clicks create cohort
    #
    observeEvent(input$create_new_cohort_b, {
      default_name <- r_default_name()


      shiny::req(default_name)
      shiny::req(nrow(r_to_operate$cohortData) != 0)


      r_results_operation$results <- NULL



      op_exp <- r_operation_expresion()

      # TEMP FIX: cohortData_union automaticaly adds & before operation cohorts, it is removed here
      if(!is.null(op_exp)){
        if((op_exp %>% stringr::str_detect("^\\&"))){
          op_exp <- op_exp %>% stringr::str_remove("^\\&")
        }
        if((op_exp %>% stringr::str_detect("^\\|"))){
          r_results_operation$results <- "Cant or in here"
        }
      }
      print(op_exp)
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
          dplyr::mutate(COHORT_NAME = r_default_name())
      }

      r_results_operation$results <- result_operation
    })

    observe({
      condition <- !is.null(r_results_operation$results) | r_default_name()==""
      shinyjs::toggleState("create_new_cohort_b", condition = !condition )
    })


    #
    # creates UI for showing the new cohort results
    #
    output$cohort_result_ui <- shiny::renderUI({
      result_operation <- r_results_operation$results
      shiny::req(result_operation)

      # if operation failed, show error
      shiny::validate(shiny::need(!is.character(result_operation), "Operation expresion is malformed"))

      # otherwise build ui
      htmltools::tagList(
        reactable::reactableOutput(ns("cohort_output_reactable")),
        shiny::actionButton(ns("copy_b"), "Copy new cohort to workbech"),
        shiny::br(),
        shiny::br(),
        shiny::tags$h4("New cohort's patients provenace:"),
        shiny::tags$p("The following upset-plot helps you to visually validate the operation. It shows the patient overlap of all
                   the cohorts in Cohort Workbench, and highlighs in back the patients in the new cohort."),
        shiny::plotOutput(ns("upset_plot")) %>%
          CohortOperationsShinyApp::ui_load_spinner()
      )

    })

    #
    # plots new cohort table
    #
    output$cohort_output_reactable <- reactable::renderReactable({
      result_operation <- r_results_operation$results
      shiny::req(result_operation)

     # print(r_results_operation$results)

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
    # plots upset overlap
    #
    output$upset_plot <- shiny::renderPlot({
      result_operation <- r_results_operation$results
      shiny::req(result_operation)
      # if operation failed, show error
      shiny::validate(shiny::need(!is.character(result_operation), "Operation expresion is malformed"))

      FinnGenTableTypes::plot_upset_cohortsOverlap(result_operation$cohortsOverlap)
    })


    #
    # action button copy_b : append cohort
    #
    # observe({
    #   result_operation <- r_results_operation$results
    #   condition <- !is.character(result_operation)
    #   if(condition) condition <- (nrow(result_operation$new_cohortData) != 0)
    #   shinyjs::toggleState("copy_b", condition = condition )
    # })

    observeEvent(input$copy_b, {
      result_operation <- r_results_operation$results
      if(nrow(result_operation$new_cohortData)!=0){
        r_to_append$cohortData <- result_operation$new_cohortData
      }
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
