#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Your application UI logic
    shinydashboard::dashboardPage(

      # TITLE
      shinydashboard::dashboardHeader(title = "Cohort Operations"),

      ## SIDEBAR
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Info", tabName = "info", icon = icon("info")),
          h4("Settings"),
          shinydashboard::menuItem("Import Cohorts", tabName = "importcohorts", icon = icon("address-card-o")),
          shinydashboard::menuItem("Operate Cohorts", tabName = "operatecohorts", icon = icon("sliders"))
        )
      ),

      ## BODY
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          ## info
          shinydashboard::tabItem(
            tabName = "info",
            includeMarkdown(app_sys("app/info_md/info_main_page.md")),
            tags$b("Connection status:"),
            mod_connection_to_db_ui("mod_connection_to_db")
          ),
          ## Import Cohorts
          shinydashboard::tabItem(
            tabName = "importcohorts",
            mod_import_cohorts_ui("mod_import_cohorts"),
          ),
          ## Operate Cohorts
          shinydashboard::tabItem(
            tabName = "operatecohorts",
            mod_operate_cohorts_ui("mod_operate_cohorts")
          )
        )
      )
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "CohortOperationsShinyApp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
