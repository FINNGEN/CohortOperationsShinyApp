library(shiny)
library(dplyr)
library(tibble)
library(stringr)
library(purrr)
library(readr)


test_that("mod_append_cohort_server no input does nothing", {
  r_cohorts <- reactiveValues(
    cohortData = test_cohortData,
    summaryCohortData = test_cohortData %>% FinnGenTableTypes::summarise_cohortData()
  )

  r_append_cohort <- reactiveValues(
    cohortData = NULL
  )

  testServer(mod_append_cohort_server, args = list(r_cohorts = r_cohorts,r_append_cohort = r_append_cohort), {
    session$flushReact()
    r$append_accepted_counter %>% expect_equal(0)
  })
})


test_that("mod_append_cohort_server overlaping COHORT_NAME replace r_cohorts", {
  # testing values
  initial_cohortData <- test_cohortData %>% filter(COHORT_NAME %in% c("A", "B"))
  append_cohortData <- test_cohortData %>% filter((COHORT_NAME=="A" & SEX=="male"))
  result_cohortData <- bind_rows(initial_cohortData %>% filter(COHORT_NAME!="A"), append_cohortData)

  # input params
  r_cohorts <- reactiveValues(
    cohortData = initial_cohortData,
    summaryCohortData = initial_cohortData %>% FinnGenTableTypes::summarise_cohortData()
  )

  r_append_cohort <- reactiveValues(
    cohortData = NULL
  )

  # run server
  testServer(mod_append_cohort_server, args = list(r_cohorts = r_cohorts,r_append_cohort = r_append_cohort), {
    ## set cohort to append
    r_append_cohort$cohortData <- append_cohortData
    ## click : "Replace" current cohort with new cohort
    session$setInputs(asked_intersect_names_alert = TRUE)

    ## test that cohort was replaced
    r_cohorts$cohortData %>% expect_equal(result_cohortData)
    ## test that counter flag is increase
    session$getReturned()() %>% expect_equal(1)

  })
})


test_that("mod_append_cohort_server overlaping COHORT_NAME replace in append_cohorts", {
  # testing values
  initial_cohortData <- test_cohortData %>% filter(COHORT_NAME %in% c("A", "B"))
  append_cohortData <- test_cohortData %>% filter(COHORT_NAME %in% c("B", "C")) %>% filter(SEX=="male")
  result_cohortData <- bind_rows(initial_cohortData, append_cohortData %>% filter(COHORT_NAME=="C"))

  # input params
  r_cohorts <- reactiveValues(
    cohortData = initial_cohortData,
    summaryCohortData = initial_cohortData %>% FinnGenTableTypes::summarise_cohortData()
  )

  r_append_cohort <- reactiveValues(
    cohortData = NULL
  )

  # run server
  testServer(mod_append_cohort_server, args = list(r_cohorts = r_cohorts,r_append_cohort = r_append_cohort), {
    ## set cohort to append
    r_append_cohort$cohortData <- append_cohortData
    ## click : "Not-import" current cohort with new cohort
    session$setInputs(asked_intersect_names_alert = FALSE)

    ## test that cohort was replaced
    r_cohorts$cohortData %>% expect_equal(result_cohortData)
    ## test that counter flag is increase
    session$getReturned()() %>% expect_equal(1)

  })
})


test_that("mod_append_cohort_server non overlaping COHORT_NAME ", {
  # testing values
  initial_cohortData <- test_cohortData %>% filter(COHORT_NAME %in% c("A", "B"))
  append_cohortData <- test_cohortData %>% filter(COHORT_NAME %in% c("C"))
  result_cohortData <- bind_rows(initial_cohortData, append_cohortData)

  # input params
  r_cohorts <- reactiveValues(
    cohortData = initial_cohortData,
    summaryCohortData = initial_cohortData %>% FinnGenTableTypes::summarise_cohortData()
  )

  r_append_cohort <- reactiveValues(
    cohortData = NULL
  )

  # run server
  testServer(mod_append_cohort_server, args = list(r_cohorts = r_cohorts,r_append_cohort = r_append_cohort), {
    ## set cohort to append
    r_append_cohort$cohortData <- append_cohortData
    session$flushReact()
    ## force change, QUESTION: is this the way ??
    r$asked_intersect_names <- TRUE
    session$flushReact()

    ## test that cohort was replaced
    r_cohorts$cohortData %>% nrow() %>% expect_equal(result_cohortData%>% nrow() )
    ## test that counter flag is increase
    session$getReturned()()  %>% expect_equal(1)

  })
})
