library(shiny)
library(dplyr)
library(tibble)
library(stringr)
library(purrr)
library(readr)
library(testthat)


cdm_webapi_conn = configCDMTools()

# authenticate
bigrquery::bq_auth(path = Sys.getenv("GCP_SERVICE_KEY"))

testing_phewas_schema <- "phewas_dummy_1k"

connection_details <- DatabaseConnector::createConnectionDetails(
  dbms = "bigquery-dbi",
  bq_dbi_project = Sys.getenv("GCP_PROJECT_ID"),
  bq_dbi_billing = Sys.getenv("GCP_BILLING_PROJECT_ID")
)

connection_settings <- FGpheWAS::createConnectionSettings(
  connection_details = connection_details,
  phewas_schema =testing_phewas_schema
)


cases_ids <- test_cohortData  %>% filter(COHORT_NAME=="A") %>% head(100) %>%  distinct(FINNGENID) %>%  pull(FINNGENID)
controls_ids <- test_cohortData  %>% filter(COHORT_NAME %in% c("B", "C")) %>% distinct(FINNGENID) %>%  pull(FINNGENID)

cohorts_settings <- FGpheWAS::createCohortsSettings(
  connection_settings = connection_settings,
  cases_cohort_source = "", cases_cohort_name = "test cases", cases_id_list = cases_ids,
  controls_cohort_source = "", controls_cohort_name = "test controls",  controls_id_list = controls_ids
)



test_that("fct_cohortMatch works", {

  r <- fct_cohortMatch(cdm_webapi_conn, cohorts_settings)

  expect_equal(r$per_maped, 0.99)

})
