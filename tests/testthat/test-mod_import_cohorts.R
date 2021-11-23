library(shiny)
library(dplyr)
library(tibble)
library(purrr)

# no connection
Sys.setenv(GOLEM_CONFIG_ACTIVE = "dev_no_connection")
r_connection <- reactiveValues(cdm_webapi_conn = configCDMTools())


r_cohorts <- reactiveValues(
  cohortData = test_cohortData,
  summaryCohortData = FinnGenTableTypes::summarise_cohortData(test_cohortData)
)


test_that("mod_import_cohorts deletes selected cohort", {
  testServer(mod_import_cohorts_server, args = list(r_connection = r_connection, r_cohorts = r_cohorts), {

    # select first cohort
    session$setInputs(cohortdata_check = c(1, 2))
    # delete button
    session$setInputs(delete_b = 1)
    # delete accept
    session$setInputs(ask_delete_alert = TRUE)

    session$flushReact()

    r_cohorts$cohortData %>%
      expect_equal(
        test_cohortData %>% filter(COHORT_NAME == "C")
      )
    r_cohorts$summaryCohortData %>%
      pull(COHORT_NAME) %>%
      expect_equal("C")
  })
})
