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
    uiOutput(ns("upadated_selectinput")), #selectInput(ns("database_picker"), "Select CDM database:", c("A")),
    reactable::reactableOutput(ns("cohorts_reactable")),
    hr(),
    actionButton(ns("import_b"), "Import Selected"),
    actionButton(ns("cancel_b"), "Cancel")
  )
}

#' import_cohort_atlas Server Functions
#'
#' @noRd
mod_import_cohort_atlas_server <- function(id, r_conn, r_cohorts){
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
    # updates r$atlas_cohorts_list with connection change
    #
    observeEvent(r_conn$cdm_webapi_conn, {

      is_webAPI_up <- r_conn$cdm_webapi_conn$conn_status_tibble %>%
        filter(step=="Connection to webAPI") %>% pull(error) %>% !.

      req(is_webAPI_up)

      output$upadated_selectinput <- renderUI({
        selectInput(
          ns("database_picker"), "Select CDM database:",
          choices = as.list(r_conn$cdm_webapi_conn$CdmSources %>% pull(sourceName)) %>%
            setNames(r_conn$cdm_webapi_conn$CdmSources %>% pull(sourceKey))
        )
      })

      r$atlas_cohorts_list <- CDMTools::getListCohortNamesIds(r_conn$cdm_webapi_conn)

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
    # button import selected: checks selected cohots
    #
    observeEvent(input$import_b, {

      req(input$selected_index)

      selected_cohorts <- r$atlas_cohorts_list %>%  slice(input$selected_index)

      # spiner alert reading status
      # shinyWidgets::progressSweetAlert(
      #   session = session, id = "progress_read_status",
      #   title = "Reading status of cohort:",
      #   display_pct = TRUE, value = 0
      # )
      shinybusy::show_modal_spinner(
        spin = "trinity-rings",
        color = "#112446",
        text = "Reading status of seelcted cohorts.",
      )

      n_selected <- nrow(selected_cohorts)
      for (i in 1:n_selected) {
        shinyWidgets::updateProgressBar(
          session = session, id = "progress_read_status",
          title = str_c("Checking status of cohort: \n", selected_cohorts[[i,"cohort_name"]]),
          value = i*round(100/n_selected-1)
        )
        selected_cohorts[i,"status"] <- CDMTools::getCohortStatus(r_conn$cdm_webapi_conn, selected_cohorts[[i,"cohort_id"]])
      }
      shinybusy::remove_modal_spinner()

      # if any is not COMPLETED error user
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

      # spiner alert importing
      shinybusy::show_modal_spinner(
        spin = "trinity-rings",
        color = "#112446",
        text = "Importing from atlas cohort:",
      )

      print(selected_cohorts %>% pull(cohort_id))
      r$imported_cohortData <- CDMTools::getCohortData(r_conn$cdm_webapi_conn, selected_cohorts %>% pull(cohort_id)) %>%
        #TEMPFIX
        transmute(
          COHORT_DATAFREEZE="DF0",
          COHORT_SOURCE = "Atlas",
          COHORT_NAME = cohort_name,
          FINNGENID = finngenid,
          COHORT_START_DATE = cohort_start_date,
          COHORT_END_DATE = cohort_end_date,
          SEX = gender,
          BIRTH_DATE = lubridate::as_date(NA)
        )
        #TEMPFIX
      shinybusy::remove_modal_spinner()

      # ask if existing cohorts should be replaced
      intersect_names <- inner_join(
        r_cohorts$cohortData %>% distinct(COHORT_DATAFREEZE, COHORT_SOURCE, COHORT_NAME),
        r$imported_cohortData %>% distinct(COHORT_DATAFREEZE, COHORT_SOURCE, COHORT_NAME),
        by = c("COHORT_DATAFREEZE", "COHORT_SOURCE", "COHORT_NAME")
      ) %>%
        mutate(name = str_c(COHORT_DATAFREEZE, COHORT_SOURCE, COHORT_NAME, sep = " ")) %>%
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
        r_cohorts$cohortData %>% distinct(COHORT_DATAFREEZE, COHORT_SOURCE, COHORT_NAME),
        r$imported_cohortData %>% distinct(COHORT_DATAFREEZE, COHORT_SOURCE, COHORT_NAME),
        by = c("COHORT_DATAFREEZE", "COHORT_SOURCE", "COHORT_NAME")
      )

      print(r$imported_cohortData %>%  FinnGenTableTypes::summarise_cohortData())

      if(r$asked_intersect_names){
        r_cohorts$cohortData <- anti_join(
          r_cohorts$cohortData,
          intersect_names,
          by = c("COHORT_DATAFREEZE", "COHORT_SOURCE", "COHORT_NAME")
        )
      }else{
        r$imported_cohortData <- anti_join(
          r$imported_cohortData,
          intersect_names,
          by = c("COHORT_DATAFREEZE", "COHORT_SOURCE", "COHORT_NAME")
        )
      }

      r_cohorts$cohortData <- bind_rows(
          r_cohorts$cohortData,
          r$imported_cohortData
        )
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




## To be copied in the UI
# mod_import_cohort_atlas_ui("import_cohort_atlas_ui_1")

## To be copied in the server
# mod_import_cohort_atlas_server("import_cohort_atlas_ui_1")
