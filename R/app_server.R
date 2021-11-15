#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # INFO connection tab ---------------------------------------------
  r_connection <- reactiveValues(cdm_webapi_conn = configCDMTools())
  r_cohorts <- reactiveValues(
    cohortData = FinnGenTableTypes::empty_cohortData(),
    summaryCohortData = FinnGenTableTypes::empty_cohortData() %>% FinnGenTableTypes::summarise_cohortData()
  )






  # INFO modules ---------------------------------------------
  mod_connection_to_db_server("mod_connection_to_db", r_connection)

  mod_import_cohorts_server("mod_import_cohorts",r_connection, r_cohorts)

  mod_operate_cohorts_server("mod_operate_cohorts", r_cohorts)



}


