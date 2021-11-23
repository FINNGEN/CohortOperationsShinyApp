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
    output$cohorts_reactable %>%
      expect_error("Uploaded file is not a tabu")

    session$setInputs(file_fi = list(datapath = "../../data-raw/test_cohortData_missing_name.tsv"))
    output$cohorts_reactable %>%
      expect_error("Uploaded tsv file is not in cohortData format.\n These are the reason")

    session$setInputs(file_fi = list(datapath = "../../data-raw/test_cohortData_3cohorts.tsv"))
    # loaded is same as in test data
    r$imported_cohortData %>% expect_equal(test_cohortData)

    output$cohorts_reactable
  })
})


test_that("mod_import_cohort_file_server appends selected", {
  r_cohorts <- reactiveValues(
    cohortData = FinnGenTableTypes::empty_cohortData(),
    summaryCohortData = FinnGenTableTypes::empty_cohortData() %>% FinnGenTableTypes::summarise_cohortData()
  )

  testServer(mod_import_cohort_file_server, args = list(r_cohorts = r_cohorts), {
    # import file
    session$setInputs(file_fi = list(datapath = "../../data-raw/test_cohortData_3cohorts.tsv"))
    r$imported_cohortData %>% expect_equal(test_cohortData)
    # select 1 and 2
    session$setInputs(selected_index = c(1, 2))
    # click import
    session$setInputs(import_b = 1)

    r_cohorts$cohortData %>%
      expect_equal(test_cohortData %>% filter(COHORT_NAME %in% c("A", "B")))

    r_cohorts$summaryCohortData %>%
      expect_equal(test_cohortData %>% filter(COHORT_NAME %in% c("A", "B")) %>% FinnGenTableTypes::summarise_cohortData())

    r$tmp_file %>% expect_equal(NULL)
    r$imported_cohortData %>% expect_equal(NULL)
    r$selected_cohortData %>% expect_equal(NULL)
    r$asked_intersect_names %>% expect_equal(NULL)
  })
})


test_that("mod_import_cohort_file_server overlaping cohots replaced", {
  r_cohorts <- reactiveValues(
    cohortData = test_cohortData,
    summaryCohortData = test_cohortData %>% FinnGenTableTypes::summarise_cohortData()
  )

  testServer(mod_import_cohort_file_server, args = list(r_cohorts = r_cohorts), {

    # import file
    session$setInputs(file_fi = list(datapath = "../../data-raw/test_cohortData_3cohorts.tsv"))
    r$imported_cohortData %>% expect_equal(test_cohortData)

    # select 1 and 2
    session$setInputs(selected_index = c(1))
    # click import
    session$setInputs(import_b = 1)
    # click yes
    session$setInputs(asked_intersect_names_alert = TRUE)

    r_cohorts$cohortData %>% # distinct(COHORT_NAME) %>%
      expect_equal(
        bind_rows(
          test_cohortData %>% filter(COHORT_NAME %in% c("B", "C")),
          test_cohortData %>% filter(COHORT_NAME %in% c("A"))
        ) # %>% distinct(COHORT_NAME)
      )

    r_cohorts$summaryCohortData %>%
      pull(COHORT_NAME) %>%
      expect_equal(c("A", "B", "C"))

    r$tmp_file %>% expect_equal(NULL)
    r$imported_cohortData %>% expect_equal(NULL)
    r$selected_cohortData %>% expect_equal(NULL)
    r$asked_intersect_names %>% expect_equal(NULL)
  })
})



test_that("mod_import_cohort_file_server overlaping cohots NOT replaced", {
  r_cohorts <- reactiveValues(
    cohortData = test_cohortData,
    summaryCohortData = test_cohortData %>% FinnGenTableTypes::summarise_cohortData()
  )

  testServer(mod_import_cohort_file_server, args = list(r_cohorts = r_cohorts), {

    # import file
    session$setInputs(file_fi = list(datapath = "../../data-raw/test_cohortData_3cohorts.tsv"))
    r$imported_cohortData %>% expect_equal(test_cohortData)

    # select 1 and 2
    session$setInputs(selected_index = c(1))
    # click import
    session$setInputs(import_b = 1)
    # click yes
    session$setInputs(asked_intersect_names_alert = FALSE)

    r_cohorts$cohortData %>% # distinct(COHORT_NAME) %>%
      expect_equal(test_cohortData)

    r_cohorts$summaryCohortData %>%
      expect_equal(test_cohortData %>% FinnGenTableTypes::summarise_cohortData())
    #
    # r$tmp_file %>% expect_equal(NULL)
    # r$imported_cohortData %>% expect_equal(NULL)
    # r$selected_cohortData %>% expect_equal(NULL)
    # r$asked_intersect_names %>% expect_equal(NULL)
  })
})


test_that("mod_import_cohort_file_server TRMPORL HACK convert genobrowser works", {
  r_cohorts <- reactiveValues(
    cohortData = FinnGenTableTypes::empty_cohortData(),
    summaryCohortData = FinnGenTableTypes::empty_cohortData() %>% FinnGenTableTypes::summarise_cohortData()
  )

  testServer(mod_import_cohort_file_server, args = list(r_cohorts = r_cohorts), {
    session$setInputs(file_fi = list(datapath = "../../data-raw/test_genobrowser_output.tsv"))
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

    r$imported_cohortData %>% expect_equal(tmp)

    output$cohorts_reactable
  })
})
