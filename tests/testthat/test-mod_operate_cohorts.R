library(shiny)
library(dplyr)
library(tibble)
library(stringr)
library(purrr)
library(readr)


test_that("mod_operate_cohorts creates cohort A", {
  r_cohorts <- reactiveValues(
    cohortData = test_cohortData,
    summaryCohortData = test_cohortData %>% FinnGenTableTypes::summarise_cohortData()
  )

  testServer(mod_operate_cohorts_server, args = list(r_cohorts = r_cohorts), {


    session$setInputs(entry_cohort_start_rb = c("earliest"))
    session$setInputs(entry_cohort_end_rb = c("latest"))
    session$setInputs(entry_cohort_names_picker = c("A"))

    output$new_cohort_name_text %>% expect_equal("[ earliest entry and latest exit from A ]")

    session$setInputs(create_new_cohort_b = 1)

    r_results_operation$results$new_cohortData %>% nrow() %>%
      expect_equal(500)

  })
})

test_that("mod_operate_cohorts creates entry cohort A or B", {
  r_cohorts <- reactiveValues(
    cohortData = test_cohortData,
    summaryCohortData = test_cohortData %>% FinnGenTableTypes::summarise_cohortData()
  )

  testServer(mod_operate_cohorts_server, args = list(r_cohorts = r_cohorts), {


    session$setInputs(entry_cohort_start_rb = c("earliest"))
    session$setInputs(entry_cohort_end_rb = c("latest"))
    session$setInputs(entry_cohort_names_picker = c("A", "B"))

    output$new_cohort_name_text %>% expect_equal("[ earliest entry and latest exit from A or B ]")

    session$setInputs(create_new_cohort_b = 1)

    r_results_operation$results$new_cohortData %>% nrow() %>%
      expect_equal(750)

  })
})

test_that("mod_operate_cohorts creates operation cohort A", {
  r_cohorts <- reactiveValues(
    cohortData = test_cohortData,
    summaryCohortData = test_cohortData %>% FinnGenTableTypes::summarise_cohortData()
  )

  testServer(mod_operate_cohorts_server, args = list(r_cohorts = r_cohorts), {

    session$setInputs(entry_cohort_start_rb = c("earliest"))
    session$setInputs(entry_cohort_end_rb = c("latest"))
    session$setInputs(entry_cohort_names_picker = c("A", "B"))

    session$setInputs(dest_boxes = c("&!", "`C`"))

    output$new_cohort_name_text %>% expect_equal("[ earliest entry and latest exit from A or B ]  if they are not in C")

    session$setInputs(create_new_cohort_b = 1)

    r_results_operation$results$new_cohortData %>% nrow() %>%
      expect_equal(500)

  })
})


test_that("mod_operate_cohorts warning with [] or in C", {
  r_cohorts <- reactiveValues(
    cohortData = test_cohortData,
    summaryCohortData = test_cohortData %>% FinnGenTableTypes::summarise_cohortData()
  )

  testServer(mod_operate_cohorts_server, args = list(r_cohorts = r_cohorts), {

    session$setInputs(entry_cohort_start_rb = c("earliest"))
    session$setInputs(entry_cohort_end_rb = c("latest"))
    session$setInputs(entry_cohort_names_picker = c("A", "B"))

    session$setInputs(dest_boxes = c("|", "`C`"))

    output$new_cohort_name_text %>% expect_equal("[ earliest entry and latest exit from A or B ]  or in C")

    session$setInputs(create_new_cohort_b = 1) %>%
      expect_warning()

    is.character(r_results_operation$results) %>%
      expect_true()

  })
})





# PUTA MIERDA EL SERVER TEST
