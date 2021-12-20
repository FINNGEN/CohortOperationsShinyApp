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
          h5(" Cohort Creation"),
          shinydashboard::menuItem("Import Cohorts", tabName = "importcohorts", icon = icon("address-card")),
          shinydashboard::menuItem("Operate Cohorts", tabName = "operatecohorts", icon = icon("sliders-h"))#,
          #h5(" Cohort Charaterisation"),
          #shinydashboard::menuItem("PheWAS", tabName = "phewas", icon = icon("briefcase-medical"))
        )
      ),

      ## BODY
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          ## info
          shinydashboard::tabItem(
            tabName = "info",
            shiny::includeMarkdown(app_sys("app/info_md/info_main_page.md")),
            tags$b("Connection status:"),
            mod_connection_to_db_ui("mod_connection_to_db"),
            h6("Runing in enviroment: ", get_golem_config("enviroment"))
          ),
          ## Import Cohorts
          shinydashboard::tabItem(
            tabName = "importcohorts",
            ### Cohorts workbech
            shinydashboard::box(
              title = "Cohorts workbech", status = "primary", solidHeader = TRUE, width = 12,
              mod_cohorts_table_ui("mod_cohorts_table_import")
            ),
            ### Import Cohorts
            shinydashboard::tabBox(
              title = tagList(shiny::icon("upload"), "Import Cohorts:"),
              id = "import_files", width = 12, side="right",
              selected = "from File",
              # #### panel ENDPOINT
              # shiny::tabPanel(
              #   "from Endploint",
              #   htmltools::h2("Possible if endpoint results in a BQ database in cohortTable format ")
              # ),
              #### panel ATLAS
              shiny::tabPanel(
                "from Atlas",
                mod_import_cohort_atlas_ui("mod_import_cohort_atlas")
              ),
              #### panel FILE
              shiny::tabPanel(
                "from File",
                #htmltools::h2("This is possible if file in cohortTable format"),
                mod_import_cohort_file_ui("mod_import_cohort_file")
              )
            )
          ),
          ## Operate Cohorts
          shinydashboard::tabItem(
            tabName = "operatecohorts",
            ### Cohorts workbech
            shinydashboard::box(
              title = "Cohorts workbech", status = "primary", solidHeader = TRUE, width = 12,
              mod_cohorts_table_ui("mod_cohorts_table_operate")
            ),
            ### Operate Cohorts
            shinydashboard::box(
              title = tagList(shiny::icon("clone"), "Operate Cohorts:"),
              status = "primary", solidHeader = FALSE, width = 12,
              mod_operate_cohorts_ui("mod_operate_cohorts")
            )
          )#,
          # ## PheWAS
          # shinydashboard::tabItem(
          #   tabName = "phewas",
          #   ### Cohorts workbech
          #   shinydashboard::box(
          #     title = "Cohorts workbech", status = "primary", solidHeader = TRUE, width = 12,
          #     mod_cohorts_table_ui("mod_cohorts_table_phewas")
          #   ),
          #   ### phewas config
          #   shinydashboard::box(
          #     title = tagList(shiny::icon("clone"), "PheWAS settings:"),
          #     status = "primary", solidHeader = FALSE, width = 12,
          #     h6("select cases from workbench"),
          #     h6("select controls from workbench"),
          #     h6("aditional config"),
          #     shiny::actionButton("runphewas", "Run PheWAS")
          #   ),
          #   shinydashboard::box(
          #     title = tagList(shiny::icon("clone"), "PheWAS results:"),
          #     status = "primary", solidHeader = FALSE, width = 12,
          #     shiny::radioButtons("runphewas", "Show",
          #                         choices = c("Table", "Manhatan Plot", "Case vs Controls Plot"),
          #                         inline = TRUE)
          #   )
          # )
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
