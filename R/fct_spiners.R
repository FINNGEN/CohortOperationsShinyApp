


#' ui_load_spinner
#'
#' Adds dna spinner to a ui_element
#'
#' @param ui_element ui_element to add spinner to
#' @param ... other parameters to shinycustomloader::withLoader
#'
#' @return
#' @export
#'
#' @importFrom shinycustomloader withLoader
ui_load_spinner <- function(ui_element, ...) {
  shinycustomloader::withLoader(
    ui_element,
    type = "html",
    loader = "dnaspin",
    ...
  )
}



#' sweetAlert_spinner
#'
#' A sweet alert with a dna spinner
#'
#' @param message  message to show in the alert
#' @param wait_time_sec if not NULL shows a progress bar that loads in the given second
#' @param ... additional options for shinyWidgets::show_alert
#'
#' @return
#' @export
#'
#'
#' @importFrom shinyWidgets show_alert
sweetAlert_spinner <- function(message, wait_time_sec = NULL, ...) {
  shinyWidgets::show_alert(
    title = NULL,
    text = tags$div(
      message,
      ui_load_spinner(plotOutput(outputId = "plot", width = "100px", height = "100px"), proxy.height = "90px")
      # attendantBar("progress-bar", hidden = TRUE, max=1000)
    ),
    html = TRUE,
    type = NULL,
    btn_labels = NA,
    closeOnClickOutside = FALSE,
    showCloseButton = FALSE,
    width = "250px",
    ...
  )

  # if(!is.null(wait_time_sec)){
  #   att <- Attendant$new("progress-bar")
  #   att$auto(ms= wait_time_sec)
  #   att$set(1)
  # }
}

#' remove_sweetAlert_spinner
#'
#' Closes a sweetAlert_spinner
#'
#' @return
#' @export
#'
#' @importFrom shinyWidgets closeSweetAlert
remove_sweetAlert_spinner <- function() {
  shinyWidgets::closeSweetAlert()
}
