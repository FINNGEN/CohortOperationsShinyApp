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

          # mod_info_box_ui(
          #   id = "includedConceptsInfo",
          #   item = shinydashboard::menuItem(text = "test", tabName = "test", icon = icon("info"))
          # ),

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
            h6("Connection status:"),
            toastui::datagridOutput("info_connstatus_dg"),
            shiny::actionButton("info_connstatus_refresh_b", "Reconnect")
          ),
          ## Import Cohorts
          shinydashboard::tabItem(tabName = "importcohorts",
                  toastui::datagridOutput(outputId = "importcohorts_cohorts_dg"),
                  #
                  shiny::actionButton("importcohorts_import_b", "Import"), # calls modal_import_cohorts
                  shiny::actionButton("importcohorts_delete_b", "Delete"),
                  shiny::actionButton("importcohorts_save_b", "Save Cohorts"),
          )
        )
      )
    )
  )
}


# MODAL called by importcohorts_import_b
modal_import_cohorts <- function(){modalDialog(
  size = "l",
  title = "Import cohorts",
  footer = NULL,
  easyClose = FALSE,
  #
  tabsetPanel(
    type = "tabs",
    # panel FILE
    tabPanel("from File",
             h2("This is possible if file in cohortTable format"),
             mod_import_cohort_file_ui("in_modal_import_file")
    ),
    # panel ATLAS
    tabPanel("from Atlas",
             mod_import_cohort_atlas_ui("in_modal_import_atlas")
    ),
    # panel ENDPOINT
    tabPanel("from Endploint",
             h2("Possible if endpoint results in a BQ database in cohortTable format ")
    )
  )
)}

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
