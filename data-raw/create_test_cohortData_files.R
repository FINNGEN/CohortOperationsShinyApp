library(tidyverse)




test_cohortData <- bind_rows(
  FinnGenTableTypes::test_1k_dummy_cohortData %>% slice(1:500) %>%
    mutate(COHORT_NAME = "A"),
  FinnGenTableTypes::test_1k_dummy_cohortData %>% slice(251:750) %>%
    mutate(COHORT_NAME = "B"),
  FinnGenTableTypes::test_1k_dummy_cohortData %>% slice(501:1000) %>%
    mutate(COHORT_NAME = "C")
)

test_cohortData %>% write_tsv("data-raw/test_cohortData_3cohorts.tsv")

test_cohortData %>%
  select(-COHORT_NAME) %>%
  write_tsv("data-raw/test_cohortData_missing_name.tsv")

test_cohortData %>% write_tsv("data-raw/test_cohortData_no_tsv.csv")

# temporal hack
test_cohortData %>%
  transmute(
    FINNGENID = FINNGENID,
    variant = "16r3839507os",
    gt = case_when(
      COHORT_NAME == "A" ~ "1|1",
      COHORT_NAME == "B" ~ "0|1",
      COHORT_NAME == "C" ~ "0|0"
    )
  ) %>%
  write_tsv("data-raw/test_genobrowser_output.tsv")

usethis::use_data(test_cohortData, overwrite = TRUE)
