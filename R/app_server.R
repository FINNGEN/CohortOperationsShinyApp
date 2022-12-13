#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # INFO connection tab ---------------------------------------------
  r_connection <- reactiveValues(
    cdm_webapi_conn = configCDMTools(),
    phewas_conn = configFGpheWAS(),
    connection_sandboxAPI = configGWAS())
  r_cohorts <- reactiveValues(
    cohortData = FinnGenTableTypes::empty_cohortData(),
    summaryCohortData = FinnGenTableTypes::empty_cohortData() %>% FinnGenTableTypes::summarise_cohortData()
  )






  # modules ---------------------------------------------
  mod_connection_to_db_server("mod_connection_to_db", r_connection)

  mod_cohorts_table_server("mod_cohorts_table_import", r_cohorts)
  mod_import_cohort_file_server("mod_import_cohort_file", r_cohorts)
  mod_import_cohort_atlas_server("mod_import_cohort_atlas", r_connection, r_cohorts)
  mod_import_cohort_endpoints_server("mod_import_cohort_endpoint", r_connection, r_cohorts)

   mod_cohorts_table_server("mod_cohorts_table_operate", r_cohorts)
   mod_operate_cohorts_server("mod_operate_cohorts", r_cohorts)

   mod_cohorts_table_server("mod_cohorts_table_compare", r_cohorts, table_editing = FALSE)
   mod_compare_cohorts_server("mod_compare_cohorts", r_cohorts)

   mod_cohorts_table_server("mod_cohorts_table_phewas", r_cohorts, table_editing = FALSE)
   mod_phewas_server("mod_phewas", r_connection, r_cohorts)
   mod_cohorts_table_server("mod_cohorts_table_gwas", r_cohorts, table_editing = FALSE)
   mod_gwas_server("mod_gwas", r_connection, r_cohorts)

  #
  # info bubbles ---------------------------------------------
  mod_info_box_server("info_importcohorts", "Import Cohorts", "info_importcohorts.md")
  mod_info_box_server("info_operatecohorts", "Operate Cohorts", "info_operatecohorts.md")
  mod_info_box_server("info_comparecohorts", "Compare Cohorts", "info_comparecohorts.md")
  mod_info_box_server("info_phewas", "PheWAS", "info_phewas.md")



}
