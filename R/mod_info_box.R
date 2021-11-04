#' info_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_info_box_ui <- function(id, item) {
  ns <- NS(id)

  infoTag <- tags$small(
    class = "badge pull-right action-button",
    style = "padding: 1px 6px 2px 6px; background-color: steelblue;",
    type = "button",
    id = ns("info_icon_link"),
    "i"
  )
  item$children[[1]]$children <-
    append(item$children[[1]]$children, list(infoTag))
  return(item)
}

#' info_box Server Functions
#'
#' @noRd
mod_info_box_server <- function(id, title, info_md_FileName) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    info_md_FileName <- paste0("app/info_md/", info_md_FileName)

    shiny::observeEvent(input$info_icon_link, {
      shiny::showModal(shiny::modalDialog(
        title = title,
        easyClose = TRUE,
        footer = NULL,
        size = "l",
        includeMarkdown(app_sys(info_md_FileName))
      ))
    })
  })
}

## To be copied in the UI
# mod_info_box_ui("info_box_ui_1")

## To be copied in the server
# mod_info_box_server("info_box_ui_1")
