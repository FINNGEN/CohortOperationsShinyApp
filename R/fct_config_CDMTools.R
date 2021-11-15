#' configCDMTools
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


configCDMTools <- function() {


  # testing
  if (get_golem_config("enviroment") == "no_connection") {
    return(
      list(conn_status_tibble = tibble(
        step = "Connection to webAPI",
        error = TRUE,
        message = "This is procued by CohortOperations"
      ))

    )
  }


  # in development laptop
  if (get_golem_config("enviroment") == "atlas-development") {
    # authenticate
    bigrquery::bq_auth(path = get_golem_config("GCP_SERVICE_KEY"))
  }
  # in development sandbox
  if (get_golem_config("enviroment") == "sandbox") {
    # authenticate
    bigrquery::bq_auth(scopes = "https://www.googleapis.com/auth/bigquery.readonly")
    # desactivate https
    httr::set_config(httr::config(ssl_verifypeer = FALSE))
  }


  # CDMTools config
  connection_details <- DatabaseConnector::createConnectionDetails(
    dbms = get_golem_config("CDMTOOLS_dbms"),
    bq_dbi_project = get_golem_config("GCP_PROJECT_ID"),
    bq_dbi_billing = get_golem_config("GCP_BILLING_PROJECT_ID")
  )

  cdm_webapi_conn <- CDMTools::createCDMWebAPIconn(
    webapi_url = get_golem_config("CDMTOOLS_webapi_url"),
    CDM_source_key = get_golem_config("CDMTOOLS_CDM_source_key_test"),
    connection_details = connection_details
  )

  #print(cdm_webapi_conn)
  return(cdm_webapi_conn)
}
