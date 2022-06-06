#' compare_cohorts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_compare_cohorts_ui <- function(id){
  ns <- NS(id)
  tagList(
    #
    shiny::tags$h4("Select cohorts to compare"),
    shinyWidgets::pickerInput(
      inputId = ns("cohorts_pi"),
      label = NULL,
      choices = "",
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE),
    ),
    shiny::hr(),
    #
    shiny::tags$h4("Patients overlap"),
    shiny::plotOutput(ns("upset_po"))  %>% ui_load_spinner(),
    shiny::hr(),
    #
    shiny::tags$h4("Patients counts"),
    shinyWidgets::pickerInput(
      inputId = ns("selectDate_pi"),
      label = "Show patients counts for:",
      choices = list(`Cohort Start Date` = "COHORT_START_DATE",
                     `Cohort End Date` = "COHORT_END_DATE",
                     `Birth Date` = "BIRTH_DATE",
                     `Death Date`= "DEATH_DATE"),
      selected = "COHORT_START_DATE"
    ),
    htmltools::withTags(
      table(
        style = "width: 100%",
        tr(
          td(
            valign = "bottom",
            shiny::checkboxGroupInput(
              inputId = ns("stratify_cbg"),
              label = "Stratify by",
              choices = c("Age", "Sex", "Calendar Year"),
              selected = c("Age", "Sex", "Calendar Year"),
              inline = TRUE
            )
          ),
          td(HTML("&nbsp;&nbsp;&nbsp;&nbsp;")),
          td(
            valign = "bottom",
            style = "text-align: right",
            shiny::checkboxInput(ns("y_scale_cb"), "Use same y-scale across cohorts")
          )
        )
      )
    ),
    plotly::plotlyOutput(
      outputId = ns("distribution_plotly"),
      width = "100%",
      height = "100%"
    ) %>% ui_load_spinner()
  )
}

#' compare_cohorts Server Functions
#'
#' @noRd
mod_compare_cohorts_server <- function(id, r_cohorts){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shiny::observe({
      shiny::req(r_cohorts$cohortData)
      all_cohorts <- r_cohorts$cohortData %>% dplyr::distinct(COHORT_NAME) %>%  dplyr::pull(COHORT_NAME)
      shinyWidgets::updatePickerInput(session, "cohorts_pi", choices = all_cohorts, selected = all_cohorts )
     })



    output$upset_po <- shiny::renderPlot({
      shiny::req(r_cohorts$cohortData)
      shiny::req(input$cohorts_pi)

      selected_cohortData <- r_cohorts$cohortData %>% dplyr::filter(COHORT_NAME %in% input$cohorts_pi)

    FinnGenTableTypes::cohortData_union(selected_cohortData)$cohortsOverlap %>%
        FinnGenTableTypes::plot_upset_cohortsOverlap()

    })


    output$distribution_plotly <- plotly::renderPlotly({
      shiny::req(r_cohorts$cohortData)
      shiny::req(input$cohorts_pi)
      shiny::req(input$stratify_cbg)
      shiny::req(input$selectDate_pi)

      selected_cohortData <- r_cohorts$cohortData %>% dplyr::filter(COHORT_NAME %in% input$cohorts_pi)

      FinnGenTableTypes::plot_cohortData_comparison(
        selected_cohortData,
        dateColumn = input$selectDate_pi,
        stratifyByAgeGroup = "Age" %in% input$stratify_cbg,
        stratifyByGender = "Sex" %in% input$stratify_cbg,
        stratifyByCalendarYear = "Calendar Year" %in% input$stratify_cbg,
        yscaleFixed = input$y_scale_cb
      )

    })

  })
}

## To be copied in the UI
# mod_compare_cohorts_ui("compare_cohorts_ui_1")

## To be copied in the server
# mod_compare_cohorts_server("compare_cohorts_ui_1")
