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


  mod_connection_to_db_server("mod_connection_to_db", r_connection)


  # IMPORT tab ---------------------------------------------











  # INFO modules ---------------------------------------------
  #mod_info_box_server("includedConceptsInfo", "test", "info_test.md")
  mod_operate_cohorts_server("mod_operate_cohorts", r_cohorts)



}


