library(shiny)
library(dplyr)
library(tibble)
library(stringr)
library(purrr)
library(testthat)

# no connection
Sys.setenv(GOLEM_CONFIG_ACTIVE = "dev_no_connection")
r_connection <- reactiveValues(cdm_webapi_conn = configCDMTools(), phewas_conn = configFGpheWAS())


test_that("mod_connection_to_db dont error", {
  testServer(mod_connection_to_db_server, args = list(r_connection = r_connection), {
    session$flushReact()

    tmp <- output$connection_status_reactable

    session$setInputs(connection_status_b = 1)

    output$connection_status_reactable %>% expect_equal(tmp)
  })
})
