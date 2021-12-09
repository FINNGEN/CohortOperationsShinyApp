library(shiny)
library(dplyr)
library(tibble)
library(stringr)
library(purrr)
library(readr)


test_that("mod_import_cohort_file_server updates output$cohorts_reactable", {
  r_cohorts <- reactiveValues(
    cohortData = FinnGenTableTypes::empty_cohortData(),
    summaryCohortData = FinnGenTableTypes::empty_cohortData() %>% FinnGenTableTypes::summarise_cohortData()
  )

  testServer(mod_import_cohort_file_server, args = list(r_cohorts = r_cohorts), {

    session$setInputs(file_fi = list(datapath = "../../data-raw/test_cohortData_no_tsv.csv"))
    # i dont understand
    r_file$tmp_file <- input$file_fi ; session$flushReact()
    #
    output$cohorts_reactable %>%
       expect_error("Uploaded file is not a tabu")

    session$setInputs(file_fi = list(datapath = "../../data-raw/test_cohortData_missing_name.tsv"))
    # i dont understand
    r_file$tmp_file <- input$file_fi ; session$flushReact()
    #
    output$cohorts_reactable %>%
      expect_error("Uploaded tsv file is not in cohortData format.\n These are the reason")

    session$setInputs(file_fi = list(datapath = "../../data-raw/test_cohortData_3cohorts.tsv"))
    # i dont understand
    r_file$tmp_file <- input$file_fi ; session$flushReact()
    #
    # loaded is same as in test data
    r_file$imported_cohortData %>% expect_equal(test_cohortData)

    output$cohorts_reactable
  })
})

test_that("mod_import_cohort_file_server TRMPORL HACK convert genobrowser works", {
  r_cohorts <- reactiveValues(
    cohortData = FinnGenTableTypes::empty_cohortData(),
    summaryCohortData = FinnGenTableTypes::empty_cohortData() %>% FinnGenTableTypes::summarise_cohortData()
  )

  testServer(mod_import_cohort_file_server, args = list(r_cohorts = r_cohorts), {
    session$setInputs(file_fi = list(datapath = "../../data-raw/test_genobrowser_output.tsv"))
    r_file$tmp_file <- input$file_fi ; session$flushReact()
    # loaded is same as in test data
    tmp <- test_cohortData %>%
      transmute(
        FINNGENID = FINNGENID,
        variant = "16r3839507os",
        gt = case_when(
          COHORT_NAME == "A" ~ "1|1",
          COHORT_NAME == "B" ~ "0|1",
          COHORT_NAME == "C" ~ "0|0"
        )
      ) %>%
      dplyr::mutate(
        COHORT_SOURCE = "Genobrowser[DF6]",
        COHORT_NAME = paste0(variant, "-", gt)
      ) %>%
      FinnGenTableTypes::as_cohortData()

    r_file$imported_cohortData %>% expect_equal(tmp)

    output$cohorts_reactable
  })
})
