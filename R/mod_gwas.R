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
    shinyFeedback::useShinyFeedback(),
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
    shiny::htmlOutput(outputId = ns("cohrots_info_to")),
    shiny::hr(),
    reactable::reactableOutput(ns("endpoints_overlap_rt")),
    shiny::hr(),
    shiny::textInput(ns("gwas_name"),label = "Analysis name"),
    shiny::textInput(ns("gwas_desc"),label = "Analysis description"),
    shiny::textInput(ns("gwas_desc_cases"),label = "Cases description"),
    shiny::textInput(ns("gwas_desc_controls"),label = "Controls description"),
    shiny::textInput(ns("gwas_email"),label = "Notification email"),
    shiny::hr(),
    shiny::actionButton(ns("rungwas"), "Run gwas analysis")
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
      shiny::req(r_cohorts$summaryCohortData)

      shinyWidgets::updatePickerInput(session, "cases_pi", choices = r_cohorts$summaryCohortData %>% dplyr::pull(COHORT_NAME))
      shinyWidgets::updatePickerInput(session, "controls_pi", choices = r_cohorts$summaryCohortData %>% dplyr::pull(COHORT_NAME))
    })

    shiny::observe({
      shiny::req(input$cases_pi)
      shiny::req(input$controls_pi)

      cases_cohort <- r_cohorts$cohortData %>% dplyr::filter(COHORT_NAME == input$cases_pi)
      controls_cohort <- r_cohorts$cohortData %>% dplyr::filter(COHORT_NAME == input$controls_pi)

      if(nrow(cases_cohort)!=0 & nrow(controls_cohort)!=0){

        CohortOperationsShinyApp::sweetAlert_spinner("Assessing patients in cases and controls")


        cohorts_settings <- FGpheWAS::createCohortsSettings(
          connection_settings = r_connection$phewas_conn[[input$database_picker]],
          cases_cohort_source = cases_cohort %>% dplyr::distinct(COHORT_SOURCE) %>% dplyr::pull(COHORT_SOURCE),
          cases_cohort_name = cases_cohort %>% dplyr::distinct(COHORT_NAME) %>% dplyr::pull(COHORT_NAME),
          cases_id_list = cases_cohort %>% dplyr::pull(FINNGENID),
          controls_cohort_source = controls_cohort %>% dplyr::distinct(COHORT_SOURCE) %>% dplyr::pull(COHORT_SOURCE),
          controls_cohort_name = controls_cohort %>% dplyr::distinct(COHORT_NAME) %>% dplyr::pull(COHORT_NAME),
          controls_id_list = controls_cohort %>% dplyr::pull(FINNGENID)
        )

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

        shiny::HTML(stringr::str_glue(
          "Cases-cohort has {n_cases} patients, from which {n_valid_cases} patients have phenotypic information {.emogi(n_cases, n_valid_cases)}.",
          "Controls-cohort has {n_controls} patients, from which {n_valid_controls} patients have phenotypic information {.emogi(n_controls, n_valid_controls)}.",
          "There is {n_overlap} patients that belong to both cohorts {em_overlap}.",
          .sep="<br>"))

      }else{
        ""
      }

    })


    output$endpoints_overlap_rt  <- reactable::renderReactable({

      if(shiny::isTruthy(r_phewas$cohorts_settings)){

        CohortOperationsShinyApp::sweetAlert_spinner("Checking ovelap with endpoints")

        closests_endpoints <- FGpheWAS::findClosestEndpoints(
          r_connection$phewas_conn[[input$database_picker]],
          r_phewas$cohorts_settings
        )

        endpoints_overlap_rt <- FGpheWAS::tableClosestEndpoints(closests_endpoints)

        CohortOperationsShinyApp::remove_sweetAlert_spinner()

        endpoints_overlap_rt
      }else{
        NULL
      }

    })


    shiny::observe({
      if(shiny::isTruthy(r_phewas$cohorts_settings)){
        # default names
        phenotype_name <- paste(r_phewas$cohorts_settings$cases_cohort$name, "vs", r_phewas$cohorts_settings$controls_cohort$name)
        phenotype_name <- phenotype_name %>%  stringr::str_replace_all("[^[:alnum:]]", "") %>%  stringr::str_to_upper()
        cases_description <- paste("Cases-cohort:", r_phewas$cohorts_settings$cases_cohort$name, "from", r_phewas$cohorts_settings$cases_cohort$source)
        controls_description <- paste("Controls-cohort:", r_phewas$cohorts_settings$controls_cohort$name, "from", r_phewas$cohorts_settings$controls_cohort$source)
        analysis_description <- paste(cases_description, "; ", controls_description)
        email <- r_connection$connection_sandboxAPI$notification_email
      }else{
        phenotype_name <- ""
        cases_description <- ""
        controls_description <- ""
        analysis_description <- ""
        email <- ""
      }

      shiny::updateTextInput(session, "gwas_name", value = phenotype_name )
      shiny::updateTextInput(session, "gwas_desc", value = analysis_description )
      shiny::updateTextInput(session, "gwas_desc_cases", value = cases_description )
      shiny::updateTextInput(session, "gwas_desc_controls", value = controls_description )
      shiny::updateTextInput(session, "gwas_email", value = email )
    })


    shiny::observe({
      # name
      shinyFeedback::feedbackWarning(
        "gwas_name",
        stringr::str_detect(input$gwas_name, "[^[:alnum:]]|[:lower:]"),
        "Name must use only upper case characters or numbers"
      )
      #email
      shinyFeedback::feedbackWarning(
        "gwas_email",
        !stringr::str_detect(input$gwas_email, "^[[:alnum:].-_]+@[[:alnum:].-]+$"),
        "Not a valid email"
      )
    })


    shiny::observeEvent(input$rungwas, {
      req(r_phewas$cohorts_settings)
      req(input$gwas_name)
      req(input$gwas_desc)
      req(input$gwas_desc_cases)
      req(input$gwas_desc_controls)
      req(input$gwas_email)
      #
      req(input$database_picker)

      release <- "Regine9"
      if(input$database_picker=="sandbox_tools_r10"){release <- "Regine10"}


      CohortOperationsShinyApp::sweetAlert_spinner("Sending data to GWAS pipeline")

      res_gwas <- FGpheWAS::runGWASAnalysis(
        r_connection$connection_sandboxAPI,
        r_phewas$cohorts_settings,
        input$gwas_name,
        description = input$gwas_desc,
        cases_description = input$gwas_desc_cases,
        controls_description = input$gwas_desc_controls,
        notification_email = input$gwas_email,
        release = release
      )

      CohortOperationsShinyApp::remove_sweetAlert_spinner()


      shinyWidgets:: sendSweetAlert(
        session = session,
        title = ifelse(res_gwas$status==TRUE, "GWAS analysis sent", "Error sending GWAS analysis"),
        text = res_gwas$content,
        type = ifelse(res_gwas$status==TRUE, "success", "error")
      )

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
