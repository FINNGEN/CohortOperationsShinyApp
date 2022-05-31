#' cohortMatch
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


fct_cohortMatch <- function(cdm_webapi_conn, cases_ids, controls_ids,
                            n_match = 10, match_sex = TRUE, match_year = TRUE) {


  # get cohort data from cases and controls
  cases_cohortData <- CDMTools::getCohortDataFromFinnGenIds(cdm_webapi_conn, cases_ids)
  controls_cohortData <- CDMTools::getCohortDataFromFinnGenIds(cdm_webapi_conn, controls_ids)


  # prepare matching data
  matching_rules <- cases_cohortData %>% dplyr::transmute(
    gender = gender,
    birth_year = lubridate::year(birth_date)
  ) %>% dplyr::count(gender, birth_year)

  to_match <- controls_cohortData %>% dplyr::transmute(
    finngenid = finngenid,
    gender = gender,
    birth_year = lubridate::year(birth_date)
  )

  # match
  mapped <- dplyr::left_join(matching_rules, to_match, by = c("gender", "birth_year")) %>%
    tidyr::nest(data=c("finngenid")) %>%
    dplyr::mutate(
      n = n*n_match,
      matched_ids = purrr::map2(data, n, ~.safe_sample_n(.x,.y)),
      n_unmaped = n - purrr::map_int(matched_ids, nrow)
    ) %>%
    dplyr::select(matched_ids, n, n_unmaped)


  per_maped <- (sum(mapped$n)-sum(mapped$n_unmaped))/sum(mapped$n)
  mapped_control_id <- mapped %>% tidyr::unnest(matched_ids) %>% dplyr::pull(finngenid)


  return(
    list(
      mapped_control_id = mapped_control_id,
      per_maped = per_maped
    )
  )

}


.safe_sample_n <- function(data, n) {
  if(nrow(data)<n){
    data
  }else{
    data %>% sample_n(n)
  }
}
