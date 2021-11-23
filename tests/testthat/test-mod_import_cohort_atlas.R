library(shiny)
library(dplyr)
library(tibble)
library(stringr)
library(purrr)
library(readr)



test_that("mod_import_cohort_atlas shows no connection error", {
  # no connection
  Sys.setenv(GOLEM_CONFIG_ACTIVE = "dev_no_connection")
  r_connection <- reactiveValues(cdm_webapi_conn = configCDMTools())


  r_cohorts <- reactiveValues(
    cohortData = FinnGenTableTypes::empty_cohortData(),
    summaryCohortData = FinnGenTableTypes::summarise_cohortData(
      FinnGenTableTypes::empty_cohortData()
    )
  )

  testServer(mod_import_cohort_atlas_server, args = list(r_connection = r_connection, r_cohorts = r_cohorts), {

    # select first cohort
    output$cohorts_reactable %>%
      expect_error("Couldn't connect to webAPI. Check Info tab for details.")
  })
})


test_that("mod_import_cohort_atlas pick database changes cdm_webapi_conn", {
  # no connection
  Sys.setenv(GOLEM_CONFIG_ACTIVE = "dev_laptop_javier")
  r_connection <- reactiveValues(cdm_webapi_conn = configCDMTools())


  r_cohorts <- reactiveValues(
    cohortData = FinnGenTableTypes::empty_cohortData(),
    summaryCohortData = FinnGenTableTypes::summarise_cohortData(
      FinnGenTableTypes::empty_cohortData()
    )
  )

  testServer(mod_import_cohort_atlas_server, args = list(r_connection = r_connection, r_cohorts = r_cohorts), {

    # select first cohort
    session$setInputs(database_picker = "dummy_df6v2_50k_13_finngen_omop_bq")

    r_connection$cdm_webapi_conn$CdmSource$sourceKey %>%
      expect_equal("dummy_df6v2_50k_13_finngen_omop_bq")
  })
})
