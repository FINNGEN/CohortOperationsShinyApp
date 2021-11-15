#' import_cohorts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_import_cohorts_ui <- function(id){
  ns <- NS(id)
  tagList(
    reactable::reactableOutput(ns("summaryCohortsData_reactable"))%>%
      ui_load_spiner(),
    hr(),
    #
    shiny::actionButton(ns("import_b"), "Import cohorts"), # calls modal_import_cohorts
    shiny::actionButton(ns("delete_b"), "Delete selected")
  )
}

#' import_cohorts Server Functions
#'
#' @noRd
mod_import_cohorts_server <- function(id, r_connection, r_cohorts){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #
    # Updates summaryCohortsData_reactable
    #
    output$summaryCohortsData_reactable <- reactable::renderReactable({
      #FinnGenTableTypes::is_cohortData(r_cohorts$cohortData, verbose = TRUE)
      r_cohorts$summaryCohortData %>%
        FinnGenTableTypes::table_summarycohortData(
          selection = "multiple",
          selectionId = ns("cohortdata_check")
        )
    })

    #
    # button import_b: launch modal
    #
    observeEvent(input$import_b, {
      showModal( modal_import_cohorts(ns) )
    })
    # Module calls used in modal
    mod_import_cohort_file_server("mod_import_cohort_file", r_cohorts)
    mod_import_cohort_atlas_server("mod_import_cohort_atlas", r_connection, r_cohorts)

    #
    # button delete_b: launch confirmation alert
    #
    observeEvent(input$delete_b, {
      req(input$cohortdata_check)
      to_delete_names <- r_cohorts$summaryCohortData %>% slice(input$cohortdata_check) %>%  pull(COHORT_NAME)
      req(to_delete_names)

      shinyWidgets::confirmSweetAlert(
        session = session,
        inputId = ns("ask_delete_alert"),
        type = "question",
        title = "Delete cohort ?",
        text = HTML("Are you sure you want to delete the following cohorts: <ul>",
                    str_c(str_c("<li> ", to_delete_names, "</li>"), collapse = ""),
                    "</ul>"),
        btn_labels = c("Cancel", "Delete"),
        html = TRUE
      )
    })

    observeEvent(input$ask_delete_alert, {
      if(input$ask_delete_alert){
        to_delete_names <- r_cohorts$summaryCohortData %>% slice(input$cohortdata_check) %>%  pull(COHORT_NAME)

        r_cohorts$cohortData <- r_cohorts$cohortData %>% filter(!(COHORT_NAME  %in% to_delete_names))
        r_cohorts$summaryCohortData <- r_cohorts$summaryCohortData %>% filter(!(COHORT_NAME  %in% to_delete_names))

      }
    })


  })
}

# MODAL called by importcohorts_import_b
modal_import_cohorts <- function(ns){modalDialog(
  size = "l",
  title = "Import cohorts",
  footer = NULL,
  easyClose = FALSE,
  #
  tabsetPanel(
    type = "tabs",
    # panel FILE
    tabPanel("from File",
             h2("This is possible if file in cohortTable format"),
             mod_import_cohort_file_ui(ns("mod_import_cohort_file"))
    ),
    # panel ATLAS
    tabPanel("from Atlas",
             mod_import_cohort_atlas_ui(ns("mod_import_cohort_atlas"))
    ),
    # panel ENDPOINT
    tabPanel("from Endploint",
             h2("Possible if endpoint results in a BQ database in cohortTable format ")
    )
  )
)}



# no connection
#Sys.setenv(GOLEM_CONFIG_ACTIVE="dev_no_connection")
# r_connection <- reactiveValues(cdm_webapi_conn = configCDMTools())
#
#
# r_cohorts <- reactiveValues(
#   cohortData = test_cohortData,
#   summaryCohortData = FinnGenTableTypes::summarise_cohortData(test_cohortData)
# )
# shinyApp(
#   fluidPage(mod_import_cohorts_ui("test")),
#   function(input,output,session){mod_import_cohorts_server("test", r_connection, r_cohorts)}
# )
