#' gwas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_gwas_ui <- function(id){
  ns <- NS(id)
  htmltools::tagList(
    shinyjs::useShinyjs(),
    shinyWidgets::useSweetAlert(),
    #
    shiny::tags$h4("Cohorts to compare"),
    shiny::fluidRow(
      shiny::column(6,shiny::tags$h5("Cases-cohort"),
        shinyWidgets::pickerInput(
        inputId = ns("cases_pi"),
        label = NULL,
        choices = "",
        options = list(title = "Select cases-cohort")
        )),
      shiny::column(6,shiny::tags$h5("Controls-cohort"),
                    shinyWidgets::pickerInput(
        inputId = ns("controls_pi"),
        label = NULL,
        choices = "",
        options = list(title = "Select controls-cohort")
      ))
    ),
    shiny::htmlOutput(outputId = ns("cohrots_info_to")),
    shiny::hr(),
    reactable::reactableOutput(ns("endpoints_overlap_rt")),
    shiny::hr(),
    shiny::downloadButton(ns("rungwas"), "Run gwas analysis")
  )
}

#' gwas Server Functions
#'
#' @noRd
mod_gwas_server <- function(id, r_connection, r_cohorts){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    r_phewas <- shiny::reactiveValues(
      cohorts_settings = NULL,
      analysis_settings = NULL
    )

    shiny::observe({
      shiny::req(r_cohorts$summaryCohortData)

      shinyWidgets::updatePickerInput(session, "cases_pi", choices = r_cohorts$summaryCohortData %>% dplyr::pull(COHORT_NAME))
      shinyWidgets::updatePickerInput(session, "controls_pi", choices = r_cohorts$summaryCohortData %>% dplyr::pull(COHORT_NAME))
    })

    shiny::observe({
      shiny::req(input$cases_pi)
      shiny::req(input$controls_pi)


      CohortOperationsShinyApp::sweetAlert_spinner("Assessing patients in cases and controls")

      cases_cohort <- r_cohorts$cohortData %>% dplyr::filter(COHORT_NAME == input$cases_pi)
      controls_cohort <- r_cohorts$cohortData %>% dplyr::filter(COHORT_NAME == input$controls_pi)

      r_phewas$cohorts_settings <- FGpheWAS::createCohortsSettings(
        connection_settings = r_connection$phewas_conn,
        cases_cohort_source = cases_cohort %>% dplyr::distinct(COHORT_SOURCE) %>% dplyr::pull(COHORT_SOURCE),
        cases_cohort_name = cases_cohort %>% dplyr::distinct(COHORT_NAME) %>% dplyr::pull(COHORT_NAME),
        cases_id_list = cases_cohort %>% dplyr::pull(FINNGENID),
        controls_cohort_source = controls_cohort %>% dplyr::distinct(COHORT_SOURCE) %>% dplyr::pull(COHORT_SOURCE),
        controls_cohort_name = controls_cohort %>% dplyr::distinct(COHORT_NAME) %>% dplyr::pull(COHORT_NAME),
        controls_id_list = controls_cohort %>% dplyr::pull(FINNGENID)
      )

      CohortOperationsShinyApp::remove_sweetAlert_spinner()

      CohortOperationsShinyApp::sweetAlert_spinner("Checking ovelap with endpoints")

      closests_endpoints <- FGpheWAS::findClosestEndpoints(
        r_connection$phewas_conn,
        r_phewas$cohorts_settings
      )

      output$endpoints_overlap_rt  <- FGpheWAS::tableClosestEndpoints(closests_endpoints) %>%
        reactable::renderReactable()

      CohortOperationsShinyApp::remove_sweetAlert_spinner()

    })



    output$cohrots_info_to <- shiny::renderUI({
      shiny::req(r_phewas$cohorts_settings)

      n_cases         <- r_phewas$cohorts_settings$status_finngenids %>% dplyr::filter(test=="cases") %>% pull(n_ids)
      n_controls      <- r_phewas$cohorts_settings$status_finngenids %>% dplyr::filter(test=="controls") %>% pull(n_ids)
      n_valid_cases   <- r_phewas$cohorts_settings$status_finngenids %>% dplyr::filter(test=="valid cases") %>% pull(n_ids)
      n_valid_controls<- r_phewas$cohorts_settings$status_finngenids %>% dplyr::filter(test=="valid controls") %>% pull(n_ids)
      n_overlap       <- r_phewas$cohorts_settings$status_finngenids %>% dplyr::filter(test=="overlap") %>% pull(n_ids)

      em_overlap <- .emogi(min(c(n_cases, n_controls)), min(c(n_cases, n_controls)) - n_overlap)

      shiny::HTML(stringr::str_glue(
        "Cases-cohort has {n_cases} patients, from which {n_valid_cases} patients have phenotypic information {.emogi(n_cases, n_valid_cases)}.",
        "Controls-cohort has {n_controls} patients, from which {n_valid_controls} patients have phenotypic information {.emogi(n_controls, n_valid_controls)}.",
        "There is {n_overlap} patients that belong to both cohorts {em_overlap}.",
        .sep="<br>"))

    })



  })
}



.emogi <- function(n,n_valid){
  if(n_valid < n*0.8){
    return("&#x274C;")# warning ❌
  }
  if(n_valid < n ){
    return("&#x1F7E8;")# warning 🟨
  }
  if(n_valid == n){
    return("&#x2705;")# warning ✅
  }
}

## To be copied in the UI
# mod_gwas_ui("gwas_ui_1")

## To be copied in the server
# mod_gwas_server("gwas_ui_1")
