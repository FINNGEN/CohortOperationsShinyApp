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

  mod_cohorts_table_server("mod_cohorts_table_import", r_cohorts)
  mod_import_cohort_file_server("mod_import_cohort_file", r_cohorts)
  mod_import_cohort_atlas_server("mod_import_cohort_atlas", r_connection, r_cohorts)

  mod_cohorts_table_server("mod_cohorts_table_operate", r_cohorts)
  mod_operate_cohorts_server("mod_operate_cohorts", r_cohorts)

  mod_cohorts_table_server("mod_cohorts_table_phewas", r_cohorts)
}
