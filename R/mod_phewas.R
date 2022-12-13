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
    shiny::tags$h4("Select Data Freeze"),
    # TOFIX: couldnt make to updateSelectInput with in the module, this is plan B
    shiny::uiOutput(ns("updated_selectinput")),
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
    #
    #
    shiny::fluidRow(
      shiny::column(3,shiny::checkboxInput(ns("match_cb"), "Used matched controls.", value = FALSE)),
      shiny::column(4,shiny::tags$h5(" Match by sex and birth year with ratio 1:")),
      shiny::column(2,shiny::numericInput(ns("n_match_ni"), NULL, 10, min = 0, max = 100, width = "80px"))
    ),
    #
    shiny::htmlOutput(outputId = ns("cohrots_info_to"))  %>%
      CohortOperationsShinyApp::ui_load_spinner(),
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
    shiny::checkboxInput(ns("include_endpoints_ci"), "Compare also endpoints", value = TRUE),
    #
    shiny::fluidRow(
      shiny::column(6,shiny::tags$h5("Codes will be excuded from the comparison if they appear in less than ")),
      shiny::column(1,shiny::numericInput(ns("min_count_s"), NULL,0, min = 0, max = 100, width = "80px")),
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

    #
    # creates picker in output$updated_selectinput
    #
    output$updated_selectinput <- shiny::renderUI({
      shiny::req(r_connection$phewas_conn)

      df_choices <- names(r_connection$phewas_conn)

      if (length(df_choices)==0) {
        shiny::selectInput(ns("database_picker"), "Select CDM database:", choices = NULL)
      } else {
        shiny::selectInput(ns("database_picker"), "Select CDM database:",
                           choices = df_choices,
                           selected = df_choices[1]
        )
      }
    })



    shiny::observe({
      shiny::req(r_cohorts$cohortData)

      shinyWidgets::updatePickerInput(session, "cases_pi", choices = r_cohorts$cohortData %>% dplyr::distinct(COHORT_NAME) %>%  dplyr::pull(COHORT_NAME))
      shinyWidgets::updatePickerInput(session, "controls_pi", choices = r_cohorts$cohortData %>% dplyr::distinct(COHORT_NAME) %>% dplyr::pull(COHORT_NAME))
    })

    shiny::observe({
      shiny::req(input$cases_pi)
      shiny::req(input$controls_pi)

      cases_cohort <- r_cohorts$cohortData %>% dplyr::filter(COHORT_NAME == input$cases_pi)
      controls_cohort <- r_cohorts$cohortData %>% dplyr::filter(COHORT_NAME == input$controls_pi)

      if(nrow(cases_cohort)!=0 & nrow(controls_cohort)!=0){

        cases_id_list <- cases_cohort %>% dplyr::pull(FINNGENID)
        controls_id_list <- controls_cohort %>% dplyr::pull(FINNGENID)
        controls_cohort_name <- controls_cohort %>% dplyr::distinct(COHORT_NAME) %>% dplyr::pull(COHORT_NAME)

        per_maped <- NULL
        if(input$match_cb == TRUE){

          CohortOperationsShinyApp::sweetAlert_spinner("Finding matched controls")

          res <- fct_cohortMatch(
            cdm_webapi_conn = r_connection$cdm_webapi_conn,
            cases_ids = cases_id_list,
            controls_ids = controls_id_list,
            n_match = input$n_match_ni
          )

          controls_id_list <- res$mapped_control_id
          controls_cohort_name <- paste0("Matched 1:", input$n_match_ni, " to ", controls_cohort_name)
          per_maped <- res$per_maped

          CohortOperationsShinyApp::remove_sweetAlert_spinner()
        }


        CohortOperationsShinyApp::sweetAlert_spinner("Assessing patients in cases and controls")

        cohorts_settings <- FGpheWAS::createCohortsSettings(
          connection_settings = r_connection$phewas_conn[[input$database_picker]],
          cases_cohort_source = cases_cohort %>% dplyr::distinct(COHORT_SOURCE) %>% dplyr::pull(COHORT_SOURCE),
          cases_cohort_name = cases_cohort %>% dplyr::distinct(COHORT_NAME) %>% dplyr::pull(COHORT_NAME),
          cases_id_list = cases_id_list,
          controls_cohort_source = controls_cohort %>% dplyr::distinct(COHORT_SOURCE) %>% dplyr::pull(COHORT_SOURCE),
          controls_cohort_name = controls_cohort_name,
          controls_id_list = controls_id_list
        )

        cohorts_settings$per_maped <- per_maped

        CohortOperationsShinyApp::remove_sweetAlert_spinner()

        r_phewas$cohorts_settings <- cohorts_settings

      }else{
        r_phewas$cohorts_settings <- NULL
      }


    })

    output$cohrots_info_to <- shiny::renderUI({
      if(shiny::isTruthy(r_phewas$cohorts_settings)){

      n_cases         <- r_phewas$cohorts_settings$status_finngenids %>% dplyr::filter(test=="cases") %>% pull(n_ids)
      n_controls      <- r_phewas$cohorts_settings$status_finngenids %>% dplyr::filter(test=="controls") %>% pull(n_ids)
      n_valid_cases   <- r_phewas$cohorts_settings$status_finngenids %>% dplyr::filter(test=="valid cases") %>% pull(n_ids)
      n_valid_controls<- r_phewas$cohorts_settings$status_finngenids %>% dplyr::filter(test=="valid controls") %>% pull(n_ids)
      n_overlap       <- r_phewas$cohorts_settings$status_finngenids %>% dplyr::filter(test=="overlap") %>% pull(n_ids)

      em_overlap <- .emogi(min(c(n_cases, n_controls)), min(c(n_cases, n_controls)) - n_overlap)

      print(r_phewas$cohorts_settings)

      matching_info <- ""
      if(shiny::isTruthy(r_phewas$cohorts_settings$per_maped)){
        matching_info <- paste0("Matching: ", {scales::percent(r_phewas$cohorts_settings$per_maped)},
                                " of controls found all matches",
                                .emogi(1, r_phewas$cohorts_settings$per_maped), ".")

      }

      print(matching_info)

      shiny::HTML(stringr::str_glue(
        "Cases-cohort has {n_cases} patients, from which {n_valid_cases} patients have phenotypic information {.emogi(n_cases, n_valid_cases)}.",
        "Controls-cohort has {n_controls} patients, from which {n_valid_controls} patients have phenotypic information {.emogi(n_controls, n_valid_controls)}.",
        "There is {n_overlap} patients that belong to both cohorts {em_overlap}.",
        matching_info,
        .sep="<br>"))

      }else{
        ""
      }

    })


    shiny::observe({
      shiny::req(input$cases_pi)
      shiny::req(input$controls_pi)
      #
      shiny::req(input$sources_pi)
      shiny::req(input$vocabulary_pi)
      #shiny::req(input$ICD10fi_keep_s)
      shiny::req(input$ICD10fi_precision_s)
      shiny::req(input$ICD9fi_precision_s)
      shiny::req(input$ICD8fi_precision_s)
      shiny::req(input$ATC_precision_s)
      shiny::req(input$NOMESCOfi_precision_s)
      #shiny::req(input$ICDO3_keep_s)
      #shiny::req(input$include_endpoints_ci)
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
        include_endpoints = input$include_endpoints_ci,
        min_n_counts = input$min_count_s
      )

      print(r_phewas$analysis_settings)

    })


    shiny::observe({
      condition <- shiny::isTruthy(r_phewas$analysis_settings) & shiny::isTruthy(r_phewas$cohorts_settings)
      shinyjs::toggleState("runphewas", condition = condition )
    })


    output$runphewas <- downloadHandler(
      filename = function() {
        paste0("Phewas_analysis_",
               r_phewas$cohorts_settings$cases_cohort$name,
               "_",
               r_phewas$cohorts_settings$controls_cohort$name,
               ".html")
      },
      content = function(file) {

        shiny::req(r_phewas$analysis_settings)
        shiny::req(r_phewas$cohorts_settings)

        #
        CohortOperationsShinyApp::sweetAlert_spinner("Fetching cohort counts")
        case_controls_counts <- FGpheWAS::getCodeCounts(
          connection_settings = r_connection$phewas_conn[[input$database_picker]],
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
