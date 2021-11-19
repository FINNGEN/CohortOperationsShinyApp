


ui_load_spiner <- function(ui_element, ...) {

  shinycustomloader::withLoader(
    ui_element,
    type = "html",
    loader = "dnaspin",
    ...)

}



sweetAlert_spiner <- function(test, wait_time_sec = NULL, ...) {

  shinyWidgets::show_alert(
    title = NULL,
    text = tags$div(
      test,
      ui_load_spiner(plotOutput(outputId = "plot", width = "100px", height = "100px"), proxy.height = "90px")
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

remove_sweetAlert_spiner <- function(){
  shinyWidgets::closeSweetAlert()
}
