#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # INFO connection tab ---------------------------------------------
  r <- reactiveValues(cdm_webapi_conn = configCDMTools())

  output$info_connstatus_dg <- toastui::renderDatagrid({
    r$cdm_webapi_conn$conn_status_tibble  %>%
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
    r$cdm_webapi_conn <- configCDMTools()
  })


  # IMPORT tab ---------------------------------------------
  r2 <- reactiveValues(
    cohortData = FinnGenTableTypes::empty_cohortData()
    )


  rf <-  mod_import_cohort_file_server("in_modal_import_cohorts")

  output$importcohorts_cohorts_dg <- toastui::renderDatagrid({
   rf() %>%
    #r2$cohortData %>%
      FinnGenTableTypes::summarise_cohortData() %>%
      FinnGenTableTypes::table_summarycohortData(display_mode = "duration" )
  })





  # importcohorts_import_b modal
  observeEvent(input$importcohorts_import_b, {
    showModal( modal_import_cohorts() )
  })



  # INFO modules ---------------------------------------------
  #mod_info_box_server("includedConceptsInfo", "test", "info_test.md")



}


