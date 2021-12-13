# library(shiny)
# library(dplyr)
# library(tibble)
# library(stringr)
# library(purrr)
# library(readr)
#
#
# test_that("mod_operate_cohorts updates output$cohorts_reactable", {
#   r_cohorts <- reactiveValues(
#     cohortData = test_cohortData,
#     summaryCohortData = test_cohortData %>% FinnGenTableTypes::summarise_cohortData()
#   )
#
#   testServer(mod_operate_cohorts_server, args = list(r_cohorts = r_cohorts), {
#
#
#
#    #  print(r_cohorts$summaryCohortData)
#    #  session$flushReact()
#    #
#    #  print(r_to_operate$summaryCohortData )
#    #
#    #  print(input$entry_cohort_start_rb)
#    #
#    #  # session$setInputs(entry_cohort_names_picker = c("A", "B"))
#    #  #
#    #  # session$setInputs(entry_cohort_start_rb = "firts")
#    #  #
#    #  #
#    #  # #
#    #  # #print(input$entry_cohort_start_rb)
#    #  #
#    #  # r_result_expresion() %>% print()
#    #  #
#    #
#    #
#    # # session$setInputs(entry_cohort_names_picker = c("A", "B"))
#    #
#
#   })
# })


# PUTA MIERDA EL SERVER TEST
