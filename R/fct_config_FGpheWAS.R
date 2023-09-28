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
configFGpheWAS <- function() {




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

    # CDMTools config
    connection_details <- DatabaseConnector::createConnectionDetails(
      dbms = get_golem_config("CDMTOOLS_dbms"),
      bq_dbi_project = get_golem_config("GCP_PROJECT_ID"),
      bq_dbi_billing = bq_dbi_billing
    )

    connection_settings_n <- list(
      #
      sandbox_tools_r12 = FGpheWAS::createConnectionSettings(
        name = "r12",
        connection_details = connection_details,
        phewas_schema = "sandbox_tools_r12",
        endpoint_cohorts_table = "endpoint_cohorts_r12_v1",
        code_counts_table = "code_counts_r12_v1",
        df9_flag = FALSE,
        codes_info_schema = "medical_codes",
        fg_codes_info_table = "fg_codes_info_v3"
      ),
      #
      sandbox_tools_r11 = FGpheWAS::createConnectionSettings(
        name = "r11",
        connection_details = connection_details,
        phewas_schema = "sandbox_tools_r11",
        endpoint_cohorts_table = "endpoint_cohorts_r11_v1",
        code_counts_table = "code_counts_r11_v1",
        df9_flag = FALSE,
        codes_info_schema = "medical_codes",
        fg_codes_info_table = "fg_codes_info_v3"
      ),
      #
      sandbox_tools_r10 = FGpheWAS::createConnectionSettings(
        name = "r10",
        connection_details = connection_details,
        phewas_schema = "sandbox_tools_r10",
        endpoint_cohorts_table = "endpoint_cohorts_r10_v1",
        code_counts_table = "code_counts_r10_v1",
        df9_flag = FALSE,
        codes_info_schema = "medical_codes",
        fg_codes_info_table = "fg_codes_info_v1"
      ),
      #
      sandbox_tools_r6 = FGpheWAS::createConnectionSettings(
        name = "r6",
        connection_details = connection_details,
        phewas_schema = "sandbox_tools_r6",
        endpoint_cohorts_table = "endpoint_cohorts_dummy50k_v1",
        code_counts_table = "code_counts_dummy50k_v1",
        df9_flag = FALSE,
        codes_info_schema = "medical_codes",
        fg_codes_info_table = "fg_codes_info_v1"
      )
    )

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

    # CDMTools config
    connection_details <- DatabaseConnector::createConnectionDetails(
      dbms = get_golem_config("CDMTOOLS_dbms"),
      bq_dbi_project = get_golem_config("GCP_PROJECT_ID"),
      bq_dbi_billing = bq_dbi_billing
    )

    connection_settings_n <- list(
      #
      sandbox_tools_r11 = FGpheWAS::createConnectionSettings(
        name = "r11",
        connection_details = connection_details,
        phewas_schema = "sandbox_tools_r11",
        endpoint_cohorts_table = "endpoint_cohorts_r11_v1",
        code_counts_table = "code_counts_r11_v1",
        df9_flag = FALSE,
        codes_info_schema = "medical_codes",
        fg_codes_info_table = "fg_codes_info_v3"
      ),
      #
      sandbox_tools_r10 = FGpheWAS::createConnectionSettings(
        name = "r10",
        connection_details = connection_details,
        phewas_schema = "sandbox_tools_r10",
        endpoint_cohorts_table = "endpoint_cohorts_r10_v1",
        code_counts_table = "code_counts_r10_v1",
        df9_flag = FALSE,
        codes_info_schema = "medical_codes",
        fg_codes_info_table = "fg_codes_info_v1"
      ),
      #
      sandbox_tools_r9 = FGpheWAS::createConnectionSettings(
        name = "r9",
        connection_details = connection_details,
        phewas_schema = "sandbox_tools_r9",
        endpoint_cohorts_table = "endpoint_cohorts",
        code_counts_table = "code_counts",
        df9_flag = TRUE,
        codes_info_schema = "sandbox_tools_r9",
        fg_codes_info_table = "fg_codes_info"
      )
      #
    )
  }



  # print(cdm_webapi_conn)
  return(connection_settings_n)
}
