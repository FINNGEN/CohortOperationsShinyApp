#' connection_to_db UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_connection_to_db_ui <- function(id){
  ns <- NS(id)
  tagList(
    reactable::reactableOutput(ns("connection_status_reactable")) %>% ui_load_spiner(),
    shiny::actionButton(ns("connection_status_b"), "Reconnect")
  )
}

#' connection_to_db Server Functions
#'
#' @noRd
mod_connection_to_db_server <- function(id, r_connection){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$connection_status_reactable <- reactable::renderReactable({

      r_connection$cdm_webapi_conn$conn_status_tibble  %>%
        mutate(step = str_replace(step, "Test c", "C")) %>%
        transmute(Status = error, Connection = step, `Error message` = message) %>%
        reactable::reactable(
          columns = list(
            Status = reactable::colDef(cell = function(value) {
              # Render as an X mark or check mark
              if (value) "\u274c" else "\u2714\ufe0f"
            })
          )
        )


    })

    shiny::observeEvent(input$connection_status_b,{
      r_connection$cdm_webapi_conn <- NULL
      r_connection$cdm_webapi_conn <- configCDMTools()
    })


  })
}

## To be copied in the UI
# mod_connection_to_db_ui("connection_to_db_ui_1")

## To be copied in the server
# mod_connection_to_db_server("connection_to_db_ui_1")

#r_connection <- reactiveValues(cdm_webapi_conn = configCDMTools())
#
# shinyApp(
#   fluidPage(mod_connection_to_db_ui("test")),
#   function(input,output,session){mod_connection_to_db_server("test", r_connection)}
# )
