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
    reactable::reactableOutput(ns("cohorts_reactable")) %>% ui_load_spiner(),
    hr(),
    # TEMP FIX: shinyjqui::updateOrderInput does not update when shinyjqui::orderInput is in as_source = TRUE mode. Lets build the whole thing on server
    shiny::uiOutput(ns("operation_expresion")),
    shiny::hr(),
    shiny::tags$b("Operation Expresion: "),
    shiny::htmlOutput(ns("entry_cohort_names_text")),
    shiny::hr(),
    shiny::plotOutput(ns("upset_plot")) %>% ui_load_spiner(),
    shiny::hr(),
    reactable::reactableOutput(ns("cohort_output_reactable")),
    shiny::downloadButton(ns("save_db"), "Save cohorts")
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
      r_cohorts$summaryCohortData %>%
        FinnGenTableTypes::table_summarycohortData()
    })


    #
    # creates UI for defining operation expression (TEMP FIX)
    #
    output$operation_expresion <- renderUI({
      # req(r_cohorts$cohortData)

      cohort_names <- r_cohorts$cohortData %>%
        distinct(COHORT_NAME) %>%
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
          items = c("(", ")", "AND-IN", "OR-IN", "NOT-IN", cohort_names ),
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
        result_expresion <- str_c(result_expresion, "(",  str_c(input$dest_boxes, collapse = " "),")")
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
      req(nrow(r_cohorts$cohortData)!=0)

      op_exp <- NULL
      if(!is.null(input$dest_boxes)){
        reparsed_input_dest_boxes <- input$dest_boxes %>%
          str_c("`", ., "`") %>%
          str_replace("^`AND-IN`$", "&") %>%
          str_replace("^`OR-IN`$", "|") %>%
          str_replace("^`NOT-IN`$", "!")%>%
          str_replace("^`\\(`$", "(")%>%
          str_replace("^`\\)`$", ")")
        op_exp <- str_c(reparsed_input_dest_boxes, collapse = "")
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


    #
    # download save_db : download cohortData
    #
    output$save_db <- downloadHandler(
      filename = "cohorts_from_cohortOperations.tsv",
      content = function(file) {
        write_tsv(
          file = file,
          x = bind_rows(r_cohorts$cohortData, r$result_cohortData)
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






