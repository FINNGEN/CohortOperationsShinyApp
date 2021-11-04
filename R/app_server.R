#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # INFO connection tab ---------------------------------------------
  r_conn <- reactiveValues(cdm_webapi_conn = configCDMTools())
  r_cohorts <- reactiveValues(
    cohortData = FinnGenTableTypes::empty_cohortData()
  )

  output$info_connstatus_dg <- toastui::renderDatagrid({
    r_conn$cdm_webapi_conn$conn_status_tibble  %>%
      select(error, step, message) %>%
      mutate(step = str_replace(step, "Test c", "C")) %>%
      toastui::datagrid(
        sortable = FALSE,
        colnames = c("Status", "Connection", "Error Message"),
        colwidths = c(100,350,NULL)
      )%>%
      toastui::grid_format(
        "error", function(value) {
          lapply(
            X = value,
            FUN = function(x) {
              if (x)
                fluidPage(tags$i(class = "fa fa-times-circle", style = "color: rgb(166,0,90)"),shiny::icon("fa-times-circle"))
              else
                fluidPage(tags$i(class = "fa fa-check-circle", style = "color: rgb(0,166,90)"),shiny::icon("fa-check-circle"))
            }
          )
        }
      )
  })

  shiny::observeEvent(input$info_connstatus_refresh_b,{
    r_conn$cdm_webapi_conn <- configCDMTools()
  })


  # IMPORT tab ---------------------------------------------

  mod_import_cohort_file_server("in_modal_import_file", r_cohorts)

  mod_import_cohort_atlas_server("in_modal_import_atlas", r_conn, r_cohorts)

  output$importcohorts_cohorts_dg <- toastui::renderDatagrid({


    FinnGenTableTypes::is_cohortData(r_cohorts$cohortData, verbose = TRUE)

     r_cohorts$cohortData %>%
    #r_cohorts$cohortData %>%
      FinnGenTableTypes::summarise_cohortData() %>%
      FinnGenTableTypes::table_summarycohortData(display_mode = "duration" ) %>%
      toastui::grid_selection_row(
        inputId = "sel_check",
        type = "checkbox",
        return = "index"
      )
  })





  # importcohorts_import_b modal
  observeEvent(input$importcohorts_import_b, {
    showModal( modal_import_cohorts() )
  })



  # INFO modules ---------------------------------------------
  #mod_info_box_server("includedConceptsInfo", "test", "info_test.md")



}


