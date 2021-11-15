


ui_load_spiner <- function(ui_element, ...) {

  shinycustomloader::withLoader(
    ui_element,
    type = "html",
    loader = "dnaspin",
    ...)

}


sweetAlert_spiner <- function(test, ...) {

 shinyWidgets::show_alert(
    title = NULL,
    text = tags$div(
      test,
      ui_load_spiner(plotOutput(outputId = "plot", width = "100px", height = "100px"), proxy.height = "90px")
    ),
    html = TRUE,
    type = NULL,
    btn_labels = NA,
    closeOnClickOutside = FALSE,
    showCloseButton = FALSE,
    width = "250px",
    ...
  )

}

remove_sweetAlert_spiner <- function(){
  shinyWidgets::closeSweetAlert()
}
