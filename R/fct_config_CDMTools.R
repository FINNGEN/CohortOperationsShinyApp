#' configCDMTools
#'
#' @description buils cdm_webapi_conn based on the info un golem yalm
#'
#' @return returns a cdm_webapi_conn list with the info set in get_golem_config
#'
#'
#' @importFrom dplyr tibble
#' @importFrom bigrquery bq_auth
#' @importFrom httr set_config config
#' @importFrom DatabaseConnector createConnectionDetails
#' @importFrom CDMTools createCDMWebAPIconn
configCDMTools <- function() {


  # testing
  if (get_golem_config("enviroment") == "no_connection") {
    return(
      list(conn_status_tibble = dplyr::tibble(
        step = "Connection to webAPI",
        error = TRUE,
        message = "This is procured by CohortOperations"
      ))
    )
  }

  # in development laptop
  if (get_golem_config("enviroment") == "atlas-development") {
    # billing project from goem-config.yalm
    bq_dbi_billing = get_golem_config("GCP_BILLING_PROJECT_ID")
    # authenticate
    bigrquery::bq_auth(path = get_golem_config("GCP_SERVICE_KEY"))
  }

  # in development sandbox
  if (get_golem_config("enviroment") == "sandbox") {
    # billing project from envar
    bq_dbi_billing = Sys.getenv("BUCKET_SANDBOX_IVM") %>% stringr::str_remove("-red$")
    # authenticate
    options(gargle_oauth_cache=FALSE) #to avoid the question that freezes the app
    bigrquery::bq_auth(scopes = "https://www.googleapis.com/auth/bigquery")
    # desactivate https
    httr::set_config(httr::config(ssl_verifypeer = FALSE))
  }


  # CDMTools config
  connection_details <- DatabaseConnector::createConnectionDetails(
    dbms = get_golem_config("CDMTOOLS_dbms"),
    bq_dbi_project = get_golem_config("GCP_PROJECT_ID"),
    bq_dbi_billing = bq_dbi_billing
  )

  cdm_webapi_conn <- CDMTools::createCDMWebAPIconn(
    webapi_url = get_golem_config("CDMTOOLS_webapi_url"),
    CDM_source_key = get_golem_config("CDMTOOLS_CDM_source_key_test"),
    connection_details = connection_details
  )

  # default to latest database
  last_sourceKey <- cdm_webapi_conn$CdmSources %>% dplyr::arrange(desc(sourceId)) %>% dplyr::slice(1) %>%  dplyr::pull(sourceKey)
  cdm_webapi_conn <-  CDMTools::changeDatabase(cdm_webapi_conn, last_sourceKey)

  # print(cdm_webapi_conn)
  return(cdm_webapi_conn)
}
