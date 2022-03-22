#' phewas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_phewas_ui <- function(id){
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
    #
    shiny::tags$h4("Analysis Settings"),
    shiny::tags$h5("Codes will be taken from the following registers: "),
    shinyWidgets::pickerInput(
      inputId = ns("sources_pi"),
      label = NULL,
      choices = FGpheWAS::valid_longdata_values$SOURCE,
      selected = FGpheWAS::valid_longdata_values$SOURCE,
      options = list(
        `actions-box` = TRUE),
      multiple = TRUE,
      width = "80%"
    ),
    shiny::tags$h5("Only codes in the following vocabularies will be compared: "),
    shinyWidgets::pickerInput(
      inputId = ns("vocabulary_pi"),
      label = NULL,
      choices = FGpheWAS::valid_longdata_values$vocabulary_id,
      selected = FGpheWAS::valid_longdata_values$vocabulary_id,
      options = list(
        `actions-box` = TRUE),
      multiple = TRUE,
      width = "80%"
    ),
    #
    shinyWidgets::prettySwitch(ns("include_endpoints_ps"), "Compare also endpoints", value = TRUE, status = "primary"),
    #
    shiny::fluidRow(
      shiny::column(6,shiny::tags$h5("Codes will be excuded from the comparison if they appear in less than ")),
      shiny::column(1,shiny::numericInput(ns("min_count_s"), NULL, 5, min = 0, max = 100, width = "80px")),
      shiny::column(1,shiny::tags$h5("patients."))
    ),
    #
    shinyWidgets::prettyToggle(
      inputId = "advance",
      label_on = "Advance options", label_off = "Advance vocabulary options",
      status_on = "primary", status_off = "primary",
      outline = TRUE,
      plain = TRUE,
      icon_on = icon("caret-down"),
      icon_off = icon("caret-right")
    ),
    shiny::conditionalPanel(
      condition = "input.advance",
      # idc10
      shiny::fluidRow(
        shiny::column(2, shiny::tags$h5("ICD10fi precision")),
        shiny::column(3, shinyWidgets::sliderTextInput(inputId = ns("ICD10fi_precision_s"),label = NULL, choices = 1:5, selected = 5)),
        shiny::column(2,shiny::tags$h5("ICD10fi codes")),
        shiny::column(5,shinyWidgets::checkboxGroupButtons(
              inputId = ns("ICD10fi_keep_s"),
              label = NULL,
              choices = c("FG_CODE1", "FG_CODE2"), selected = c("FG_CODE1", "FG_CODE2"),
              status = "primary",
              checkIcon = list( yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
            )
        )
      ),
      shiny::fluidRow(
        shiny::column(2, shiny::tags$h5("ICD9fi precision")),
        shiny::column(3, shinyWidgets::sliderTextInput(inputId = ns("ICD9fi_precision_s"),label = NULL, choices = 1:5, selected = 5))
      ),
      shiny::fluidRow(
        shiny::column(2, shiny::tags$h5("ICD9fi precision")),
        shiny::column(3, shinyWidgets::sliderTextInput(inputId = ns("ICD8fi_precision_s"),label = NULL, choices = 1:5, selected = 5))
      ),
      shiny::fluidRow(
        shiny::column(2, shiny::tags$h5("ATC precision")),
        shiny::column(3, shinyWidgets::sliderTextInput(inputId = ns("ATC_precision_s"),label = NULL, choices = 1:7, selected = 7))
      ),
      shiny::fluidRow(
        shiny::column(2, shiny::tags$h5("NOMESCOfi precision")),
        shiny::column(3, shinyWidgets::sliderTextInput(inputId = ns("NOMESCOfi_precision_s"),label = NULL, choices = 1:5, selected = 5))
      ),
      shiny::fluidRow(
        shiny::column(2,shiny::tags$h5("ICDO3 codes")),
        shiny::column(5,shinyWidgets::checkboxGroupButtons(
          inputId = ns("ICDO3_keep_s"),
          label = NULL,
          choices = c("FG_CODE1", "FG_CODE2", "FG_CODE3"), selected = c("FG_CODE1", "FG_CODE2", "FG_CODE3"),
          status = "primary",
          checkIcon = list( yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
        ))
      )
    ),
    shiny::hr(),
    shiny::downloadButton(ns("runphewas"), "Run PheWAS analysis & download results")
  )
}

#' phewas Server Functions
#'
#' @noRd
mod_phewas_server <- function(id, r_connection, r_cohorts){
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


    shiny::observe({
      shiny::req(input$cases_pi)
      shiny::req(input$controls_pi)
      #
      shiny::req(input$sources_pi)
      shiny::req(input$vocabulary_pi)
      shiny::req(input$ICD10fi_keep_s)
      shiny::req(input$ICD10fi_precision_s)
      shiny::req(input$ICD9fi_precision_s)
      shiny::req(input$ICD8fi_precision_s)
      shiny::req(input$ATC_precision_s)
      shiny::req(input$NOMESCOfi_precision_s)
      shiny::req(input$ICDO3_keep_s)
      shiny::req(input$include_endpoints_ps)
      shiny::req(input$min_count_s)



      r_phewas$analysis_settings <- FGpheWAS::createAnalysisSettings(
        longdata_sources = input$sources_pi,
        longdata_vocabulary_ids = input$vocabulary_pi,
        #
        keep_ICD10fi_codes = input$ICD10fi_keep_s,
        ICD10fi_precision = input$ICD10fi_precision_s,
        ICD9fi_precision = input$ICD9fi_precision_s,
        ICD8fi_precision = input$ICD8fi_precision_s,
        ATC_precision = input$ATC_precision_s,
        NOMESCOfi_precision = input$NOMESCOfi_precision_s,
        keep_ICDO3_codes = input$ICDO3_keep_s,
        include_endpoints = input$include_endpoints_ps,
        min_n_counts = input$min_count_s
      )

    })


    shiny::observe({
      condition <- shiny::isTruthy(r_phewas$analysis_settings) & shiny::isTruthy(r_phewas$cohorts_settings)
      shinyjs::toggleState("runphewas", condition = condition )
    })


    output$runphewas <- downloadHandler(
      filename = function() {
        paste0("Phewas_analysis_",
              FGpheWAS::test_phewas_results$cohorts_settings$cases_cohort$name,
              "_",
              FGpheWAS::test_phewas_results$cohorts_settings$controls_cohort$name,
              ".html")
      },
      content = function(file) {

        shiny::req(r_phewas$analysis_settings)
        shiny::req(r_phewas$cohorts_settings)

        #
        CohortOperationsShinyApp::sweetAlert_spinner("Fetching cohort counts")
        case_controls_counts <- FGpheWAS::getCodeCounts(
          connection_settings = r_connection$phewas_conn,
          cohorts_settings = r_phewas$cohorts_settings,
          analysis_settings = r_phewas$analysis_settings
        )
        CohortOperationsShinyApp::remove_sweetAlert_spinner()

        #
        CohortOperationsShinyApp::sweetAlert_spinner("Calculating Fisher-tests")
        case_controls_counts_fisher <- FGpheWAS::addFisherTestToCodeCounts(case_controls_counts)
        CohortOperationsShinyApp::remove_sweetAlert_spinner()

        #
        CohortOperationsShinyApp::sweetAlert_spinner("Building resutls dashboard")

        cohorts_settings <- r_phewas$cohorts_settings
        cohorts_settings$cases_cohort$validated_ids <- NULL
        cohorts_settings$controls_cohort$validated_ids <- NULL

        print(cohorts_settings)

        analysis_settings <- r_phewas$analysis_settings

        FGpheWAS::buildReport(
          phewas_results =list(
            cohorts_settings = cohorts_settings,
            analysis_settings = analysis_settings,
            case_controls_counts_fisher = case_controls_counts_fisher
          ) ,
          output_file_html = file)

        CohortOperationsShinyApp::remove_sweetAlert_spinner()

      }
    )


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
# mod_phewas_ui("phewas_ui_1")

## To be copied in the server
# mod_phewas_server("phewas_ui_1")
