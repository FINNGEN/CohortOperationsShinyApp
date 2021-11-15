#' operate_cohorts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_operate_cohorts_ui <- function(id){
  ns <- NS(id)
  tagList(
    reactable::reactableOutput(ns("cohorts_reactable")),
    hr(),
    # TEMP FIX: shinyjqui::updateOrderInput does not update when shinyjqui::orderInput is in as_source = TRUE mode. Lets build the whole thing on server
    uiOutput(ns("operation_expresion")),
    hr(),
    tags$b("Operation Expresion: "),
    htmlOutput(ns("entry_cohort_names_text")),
    hr(),
    plotOutput(ns("upset_plot")),
    hr(),
    reactable::reactableOutput(ns("cohort_output_reactable"))
  )
}

#' operate_cohorts Server Functions
#'
#' @noRd
mod_operate_cohorts_server <- function(id, r_cohorts){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #
    # reactive variables
    #
    r <- reactiveValues(
      result_cohortData = FinnGenTableTypes::empty_cohortData()
    )


    #
    # updates output$cohorts_reactable with given r_cohorts
    #
    output$cohorts_reactable <- reactable::renderReactable({
      r_cohorts$cohortData %>%
        FinnGenTableTypes::summarise_cohortData() %>%
        FinnGenTableTypes::table_summarycohortData()
    })


    #
    # creates UI for defining operation expression (TEMP FIX)
    #
    output$operation_expresion <- renderUI({
      # req(r_cohorts$cohortData)

      cohort_names <- r_cohorts$cohortData %>%
        FinnGenTableTypes::summarise_cohortData() %>%
        pull(COHORT_NAME)

      tagList(
        shinyWidgets::pickerInput(
          ns("entry_cohort_names_picker"),
          "Select one or more entry cohorts",
          choices = cohort_names,
          multiple = TRUE
        ),
        shinyjqui::orderInput(
          ns("source_boxes"),
          'Operation Elements',
          items = c("(", ")", "&", "|", "!", cohort_names ),
          as_source = TRUE, connect = ns('dest_boxes')
        ),
        shinyjqui::orderInput(
          ns("dest_boxes"),
          'Operate entry cohorts',
          items = NULL, placeholder = 'Drag operation elements here...'
        )
      )

    })


    #
    # calculates total expresion
    #
    output$entry_cohort_names_text <-renderText({
      result_expresion <- ""
      if(!is.null(input$entry_cohort_names_picker)){
        result_expresion <- str_c("[",  str_c(input$entry_cohort_names_picker, collapse = " | "),"]")
      }
      if(!is.null(input$dest_boxes)){
        if(nchar(result_expresion)!=0){ result_expresion <- str_c(result_expresion, "&") }
        result_expresion <- str_c(result_expresion, "(",  str_c(input$dest_boxes, collapse = ""),")")
      }

      if(nchar(result_expresion)==0){
        result_expresion <- "<font color=\"#AFAFAF\"> Select entry cohorts or operation elements </font>"
      }
      result_expresion
    })


    #
    # plots overlap
    #
    output$upset_plot <- renderPlot({
      req(r_cohorts$cohortData)

      op_exp <- NULL
      if(!is.null(input$dest_boxes)){
        op_exp <- str_c(input$dest_boxes, collapse = "")
      }

      result_operation <- tryCatch({
        FinnGenTableTypes::cohortData_union(
          r_cohorts$cohortData,
          entry_cohorts = input$entry_cohort_names_picker,
          operation_expresion = op_exp
        )
      }, error = function(e) e$message)

      # if operation failed, error
      if(!is_tibble(result_operation)){
        r$result_cohortData <- FinnGenTableTypes::empty_cohortData()
        validate(need(FALSE , "Operation expresion is malformed"))
      }

      # if correct update result_cohortData and upset plot
      r$result_cohortData <- result_operation
      result_cohort_name <- result_operation %>% distinct(COHORT_NAME) %>% pull(COHORT_NAME)
      if(length(result_cohort_name)==0){result_cohort_name<-""}

      FinnGenTableTypes::plot_upset_cohortData(
        bind_rows(r_cohorts$cohortData, result_operation),
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




  })
}

## To be copied in the UI
# mod_operate_cohorts_ui("operate_cohorts_ui_1")
#
# ## To be copied in the server
# # mod_operate_cohorts_server("operate_cohorts_ui_1")
# r_cohorts <- reactiveValues(cohortData=read_tsv("data-raw/test_cohortData_3cohorts.tsv"))
# shinyApp(
#   fluidPage(mod_operate_cohorts_ui("test")),
#   function(input,output,session){mod_operate_cohorts_server("test", r_cohorts)}
# )






