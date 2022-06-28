
#' Title
#'
#' @return
#' @export
#'
#' @examples
configGWAS <- function() {

  url = "https://"

  .sandbox_token = Sys.getenv("SANDBOX_TOKEN")

  connection_sandboxAPI <- FGpheWAS::createSandboxAPIConnection(url, .sandbox_token)

  return(connection_sandboxAPI)
}
