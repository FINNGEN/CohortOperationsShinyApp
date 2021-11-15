#' import_cohort_atlas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_import_cohort_atlas_ui <- function(id){
  ns <- NS(id)

  tagList(
    # uses :
    shinyWidgets::useSweetAlert(),
    #
    br(),
    # TOFIX: couldnt make to updateSelectInput with in the module, this is plan B
    uiOutput(ns("updated_selectinput")),
    #selectInput(ns("database_picker"), "Select CDM database:", choices = NULL),
    reactable::reactableOutput(ns("cohorts_reactable")),
    hr(),
    actionButton(ns("import_b"), "Import Selected"),
    actionButton(ns("cancel_b"), "Cancel")
  )
}

#' import_cohort_atlas Server Functions
#'
#' @noRd
mod_import_cohort_atlas_server <- function(id, r_connection, r_cohorts){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #
    # reactive variables
    #
    r <- reactiveValues(
      atlas_cohorts_list = NULL,
      imported_cohortData = NULL,
      asked_intersect_names = NULL
    )

    #
    # creates picker in output$updated_selectinput
    #
    output$updated_selectinput <- renderUI({
      req(r_connection$cdm_webapi_conn)

      is_webAPI_up <- r_connection$cdm_webapi_conn$conn_status_tibble %>%
        filter(step=="Connection to webAPI") %>% pull(error) %>% !.

      if(!is_webAPI_up){
        selectInput(ns("database_picker"), "Select CDM database:", choices = NULL)
      }else{
        selectInput(ns("database_picker"), "Select CDM database:",
                    choices = r_connection$cdm_webapi_conn$CdmSources %>% pull(sourceKey),
                    selected = r_connection$cdm_webapi_conn$CdmSource$sourceKey)
      }
    })

    #
    # updates r_connection$cdm_webapi_conn with database_picker and refreshes r$atlas_cohorts_list
    #
    observeEvent(input$database_picker, {
      is_webAPI_up <- r_connection$cdm_webapi_conn$conn_status_tibble %>%
        filter(step=="Connection to webAPI") %>% pull(error) %>% !.

      req(is_webAPI_up)

      r_connection$cdm_webapi_conn <- CDMTools::changeDatabase(r_connection$cdm_webapi_conn, input$database_picker)

      r$atlas_cohorts_list <- CDMTools::getListCohortNamesIds(r_connection$cdm_webapi_conn) %>%
            arrange(desc(cohort_id))

    })

    #
    # updates table with r$atlas_cohorts_list change
    #
    output$cohorts_reactable <- reactable::renderReactable({
      validate(need(r$atlas_cohorts_list, "Couldn't connect to webAPI. Check Info tab for details."))
      r$atlas_cohorts_list %>%
        reactable::reactable(
          selectionId = ns("selected_index"), selection = "multiple", onClick = "select",
          searchable = TRUE)
    })

    #
    # button import selected: checks selected cohorts
    #
    observeEvent(input$import_b, {

      req(input$selected_index)

      selected_cohorts <- r$atlas_cohorts_list %>%  slice(input$selected_index)

      ## Check status of selected cohorts
      sweetAlert_spiner("Checking cohorts' status")

      n_selected <- nrow(selected_cohorts)
      for (i in 1:n_selected) {
        selected_cohorts[i,"status"] <- CDMTools::getCohortStatus(r_connection$cdm_webapi_conn, selected_cohorts[[i,"cohort_id"]])
      }
      remove_sweetAlert_spiner()

      # if any of the status is not COMPLETED error user
      not_compleated_cohorts <- selected_cohorts %>% filter(status!="COMPLETE") %>% pull(cohort_name)
      if(length(not_compleated_cohorts)!=0){
        shinyWidgets::sendSweetAlert(
          session = session,
          type = "error",
          title = "Cohorts not COMPLETE",
          text = HTML("The following cohorts are not COMPLETE:  <ul>",
                      str_c(str_c("<li> ",not_compleated_cohorts, "</li>"), collapse = ""),
                      "</ul> Please, go to Atlas and run them for database: ", input$database_picker),
          html = TRUE
        )
        req(FALSE)# break reactivity
      }

      ## Import cohorts
      sweetAlert_spiner("Importing cohorts")

      r$imported_cohortData <- CDMTools::getCohortData(r_connection$cdm_webapi_conn, selected_cohorts %>% pull(cohort_id)) %>%
        #TEMPFIX
        mutate(
          BIRTH_DATE = pmin(BIRTH_DATE, COHORT_START_DATE),
          DEATH_DATE = pmax(DEATH_DATE, COHORT_END_DATE)
        )
      #print(FinnGenTableTypes::is_cohortData(r$imported_cohortData, verbose = T))
      #TEMPFIX
      remove_sweetAlert_spiner()

      # ask if existing cohorts should be replaced
      intersect_names <- inner_join(
        r_cohorts$cohortData %>% distinct(COHORT_SOURCE, COHORT_NAME),
        r$imported_cohortData %>% distinct(COHORT_SOURCE, COHORT_NAME),
        by = c("COHORT_SOURCE", "COHORT_NAME")
      ) %>%
        mutate(name = str_c(COHORT_NAME, ", From: ", COHORT_SOURCE)) %>%
        pull(name)


      if(length(intersect_names)>0){
        shinyWidgets::confirmSweetAlert(
          session = session,
          inputId = "asked_intersect_names_alert",
          type = "question",
          title = "Some selected cohorts had been alredy imported:",
          text = HTML("The following cohorts had been alredy imported: <ul>",
                      str_c(str_c("<li> ", intersect_names, "</li>"), collapse = ""),
                      "</ul> Should these be replaced or ignored."),
          btn_labels = c("Not-import", "Replace"),
          html = TRUE
        )
      }else{
        r$asked_intersect_names <- TRUE
      }

    })

    # just pass the info to make it writable
    observe({r$asked_intersect_names <- input$asked_intersect_names_alert})

    #
    # confirmSweetAlert asked_intersect_names
    #
    observeEvent(r$asked_intersect_names, {
      # take names from main and imported
      intersect_names <- inner_join(
        r_cohorts$cohortData %>% distinct(COHORT_SOURCE, COHORT_NAME),
        r$imported_cohortData %>% distinct(COHORT_SOURCE, COHORT_NAME),
        by = c("COHORT_SOURCE", "COHORT_NAME")
      )

      if(r$asked_intersect_names){
        # remove from main
        r_cohorts$cohortData <- anti_join(
          r_cohorts$cohortData,
          intersect_names,
          by = c("COHORT_SOURCE", "COHORT_NAME")
        )
      }else{
        r$imported_cohortData <- anti_join(
          r$imported_cohortData,
          intersect_names,
          by = c("COHORT_SOURCE", "COHORT_NAME")
        )
      }

      r_cohorts$cohortData <- bind_rows(
        r_cohorts$cohortData,
        r$imported_cohortData
      )
      # re calcualte all, to get the rigth years in dates
      r_cohorts$summaryCohortData <- FinnGenTableTypes::summarise_cohortData(r_cohorts$cohortData)

      .close_and_reset()
    })

    #
    # button cancel selected: close modal
    #
    observeEvent(input$cancel_b, {
      .close_and_reset()
    })


    .close_and_reset <- function(){
      #r$imported_cohortData = NULL
      #r$atlas_cohorts_list <- NULL
      r$imported_cohortData <-  NULL
      r$asked_intersect_names <- NULL
      removeModal()
    }

  })
}
#
# #
# # # # # # no connection
# Sys.setenv(GOLEM_CONFIG_ACTIVE="dev_laptop_javier")
# r_connection <- reactiveValues(cdm_webapi_conn = configCDMTools())
#
#
# r_cohorts <- reactiveValues(
#   cohortData = test_cohortData,
#   summaryCohortData = FinnGenTableTypes::summarise_cohortData(test_cohortData)
# )
#
# shinyApp(
#   fluidPage(mod_import_cohort_atlas_ui("test")),
#   function(input,output,session){mod_import_cohort_atlas_server("test",r_connection, r_cohorts)}
# )
