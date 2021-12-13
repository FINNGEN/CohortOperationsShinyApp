#' import_cohort_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS fileInput actionButton
#' @importFrom htmltools tagList hr
#' @importFrom shinyjs useShinyjs
#' @importFrom reactable reactableOutput
mod_import_cohort_file_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
   use_mod_append_cohort_ui(),
    shinyjs::useShinyjs(),
    #
    shiny::fileInput(ns("file_fi"), "Choose tsv file using cohortData format:",
      multiple = FALSE,
      accept = c("text/tsv", "text/tabular-separated-values,text/plain", ".tsv")
    ),
    htmltools::hr(),
    reactable::reactableOutput(ns("cohorts_reactable")), # %>% ui_load_spiner(),
    htmltools::hr(),
    shiny::actionButton(ns("import_b"), "Import Selected")
  )
}

#' import_cohort_file Server Functions
#'
#' @noRd
#' @importFrom shiny moduleServer reactiveValues observe req validate need observeEvent
#' @importFrom reactable renderReactable reactable getReactableState updateReactable
#' @importFrom tools file_ext
#' @importFrom readr read_tsv
#' @importFrom utils hasName
#' @importFrom dplyr mutate distinct semi_join slice
#' @importFrom FinnGenTableTypes is_cohortData as_cohortData
#' @importFrom stringr str_c
#' @importFrom shinyjs toggleState reset
mod_import_cohort_file_server <- function(id, r_cohorts) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    #
    # reactive variables
    #
    r_file <- shiny::reactiveValues(
      tmp_file = NULL,
      imported_cohortData = NULL
    )

    r_to_append <- shiny::reactiveValues(
      cohortData = NULL
    )

    #
    # just pass the info to make it writable
    #
    shiny::observe({
      r_file$tmp_file <- input$file_fi
    })

    #
    # updates output$cohorts_reactable with uploaded file, or with error
    #
    output$cohorts_reactable <- reactable::renderReactable({
      shiny::req(r_file$tmp_file)
      ext <- tools::file_ext(r_file$tmp_file$datapath)

      shiny::validate(shiny::need(ext == "tsv", "Uploaded file is not a tabular-separated-values (.tsv) file. Please upload a tsv file."))

      tmp_imported_file <- readr::read_tsv(r_file$tmp_file$datapath, show_col_types = FALSE)

      # TEMP HACK
      # if it has columns variant and gt
      if (tmp_imported_file %>% utils::hasName(c("variant", "gt")) %>% sum() == 2) {
        tmp_imported_file <- tmp_imported_file %>%
          dplyr::mutate(
            COHORT_SOURCE = "Genobrowser[DF6]",
            COHORT_NAME = paste0(variant, "-", gt)
          )
        message("TEMP HACK: genobrowser table converter to cohortData")
      }
      # END TEMP HACK

      is_cohortData_message <- tryCatch(
        {
          as.character(FinnGenTableTypes::is_cohortData(tmp_imported_file, extrict = FALSE, verbose = TRUE))
        },
        message = function(m) {
          m$message
        }
      )

      shiny::validate(
        shiny::need(
          is_cohortData_message == "TRUE",
          stringr::str_c("Uploaded tsv file is not in cohortData format.\n These are the reasons: \n", is_cohortData_message)
        )
      )

      r_file$imported_cohortData <- FinnGenTableTypes::as_cohortData(tmp_imported_file)

      shiny::req(r_file$imported_cohortData)
      # output reactable
      r_file$imported_cohortData %>%
        dplyr::distinct(COHORT_NAME, COHORT_SOURCE) %>%
        reactable::reactable(
          selection = "multiple", onClick = "select", defaultSelected = NULL,
          searchable = TRUE
        )
    })
    # reactive function to get selected values
    r_selected_index <- reactive(reactable::getReactableState("cohorts_reactable", "selected", session))

    #
    # button import selected: checks selected cohorts
    #
    observe({
      shinyjs::toggleState("import_b", condition = !is.null(r_selected_index()) )
    })

    shiny::observeEvent(input$import_b, {
      shiny::req(r_selected_index())
      ## copy selected to
      r_to_append$cohortData <- dplyr::semi_join(
        r_file$imported_cohortData,
        r_file$imported_cohortData %>%
          dplyr::distinct(COHORT_NAME, COHORT_SOURCE) %>%
          dplyr::slice(r_selected_index()),
        by = c("COHORT_SOURCE", "COHORT_NAME")
      )
    })

    #
    # evaluate the cohorts to append; if accepted increase output to trigger closing acctions
    #
    r_append_accepted_counter <- mod_append_cohort_server("impor_file", r_cohorts, r_to_append )

    # close and reset
    shiny::observeEvent(r_append_accepted_counter(), {
      shinyjs::reset("file_fi")
      r_file$tmp_file <- NULL
      r_file$imported_cohortData <- NULL
      r_to_append$cohortData <- NULL
      reactable::updateReactable("cohorts_reactable", selected = NA, session = session )
    })

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

