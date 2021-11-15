#' import_cohort_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import readr
mod_import_cohort_file_ui <- function(id){
  ns <- NS(id)
  tagList(
    #
    fileInput(ns("file_fi"), "Choose tsv file using cohortData format:",
              multiple = FALSE,
              accept = c("text/tsv","text/tabular-separated-values,text/plain",".tsv")
    ),
    reactable::reactableOutput(ns("cohorts_reactable")) %>% ui_load_spiner(),
    hr(),
    actionButton(ns("import_b"), "Import Selected"),
    actionButton(ns("cancel_b"), "Cancel")
  )
}

#' import_cohort_file Server Functions
#'
#' @noRd
mod_import_cohort_file_server <- function(id, r_cohorts){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #
    # reactive variables
    #
    r <- reactiveValues(
      tmp_file = NULL,
      imported_cohortData = NULL,
      selected_cohortData = NULL,
      asked_intersect_names = NULL
    )

    #
    #just pass the info to make it writable
    #
    observe({r$tmp_file <- input$file_fi})

    #
    # updates output$cohorts_reactable with uploaded file
    #
    output$cohorts_reactable <- reactable::renderReactable({
      req(r$tmp_file)
      ext <- tools::file_ext(r$tmp_file$datapath)

      validate(need(ext == "tsv", "Uploaded file is not a tabular-separated-values (.tsv) file. Please upload a tsv file."))

      tmp_imported_file <- read_tsv(r$tmp_file$datapath, show_col_types = FALSE)

      is_cohortData_message <- tryCatch(
        {as.character(FinnGenTableTypes::is_cohortData(tmp_imported_file, verbose = TRUE))}, message = function(m){m$message}
      )

      validate(
        need(is_cohortData_message=="TRUE",
             str_c("Uploaded tsv file is not in cohortData format.\n These are the reasons: \n", is_cohortData_message))
      )

      r$imported_cohortData <- tmp_imported_file

      # output reactable
      r$imported_cohortData %>%
        distinct(COHORT_NAME, COHORT_SOURCE) %>%
        select(COHORT_NAME, COHORT_SOURCE) %>%
        reactable::reactable(
          selectionId = ns("selected_index"), selection = "multiple", onClick = "select",
          searchable = TRUE)
    })

    #
    # button import selected: checks selected cohorts
    #
    observeEvent(input$import_b, {

      req(input$selected_index)

      r$selected_cohortData <- semi_join(
        r$imported_cohortData,
        r$imported_cohortData %>%
          distinct(COHORT_NAME, COHORT_SOURCE) %>%
          slice(input$selected_index),
        by = c("COHORT_SOURCE", "COHORT_NAME")
      )

      # ask if existing cohorts should be replaced
      intersect_names <- inner_join(
        r_cohorts$cohortData %>% distinct(COHORT_SOURCE, COHORT_NAME),
        r$selected_cohortData %>% distinct(COHORT_SOURCE, COHORT_NAME),
        by = c("COHORT_SOURCE", "COHORT_NAME")
      ) %>%
        mutate(name = str_c(COHORT_SOURCE, COHORT_NAME, sep = " ")) %>%
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
        r$selected_cohortData %>% distinct( COHORT_SOURCE, COHORT_NAME),
        by = c("COHORT_SOURCE", "COHORT_NAME")
      )

      if(r$asked_intersect_names){
        # remove from main
        r_cohorts$cohortData <- anti_join(
          r_cohorts$cohortData,
          intersect_names,
          by = c("COHORT_SOURCE", "COHORT_NAME")
        )
        r_cohorts$summaryCohortData <- anti_join(
          r_cohorts$summaryCohortData,
          intersect_names,
          by = c("COHORT_SOURCE", "COHORT_NAME")
        )
      }else{
        r$selected_cohortData <- anti_join(
          r$selected_cohortData,
          intersect_names,
          by = c("COHORT_SOURCE", "COHORT_NAME")
        )
      }

      r_cohorts$cohortData <- bind_rows(
        r_cohorts$cohortData,
        r$selected_cohortData
      )
      r_cohorts$summaryCohortData <- bind_rows(
        r_cohorts$summaryCohortData,
        FinnGenTableTypes::summarise_cohortData(r$selected_cohortData)
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
      r$tmp_file <- NULL
      r$imported_cohortData <-  NULL
      r$selected_cohortData <-  NULL
      r$asked_intersect_names <- NULL
      removeModal()
    }

  })
}

# r_cohorts <- reactiveValues(
#   cohortData = FinnGenTableTypes::empty_cohortData(),
#   summaryCohortData = FinnGenTableTypes::summarise_cohortData(FinnGenTableTypes::empty_cohortData())
# )
#
# shinyApp(
#   fluidPage(mod_import_cohort_file_ui("test")),
#   function(input,output,session){mod_import_cohort_file_server("test", r_cohorts)}
# )
