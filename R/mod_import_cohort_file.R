#' import_cohort_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_import_cohort_file_ui <- function(id){
  ns <- NS(id)
  tagList(
    #
    fileInput(ns("file_fi"), "Choose tsv file using cohortData format:",
              multiple = FALSE,
              accept = c("text/tsv","text/tabular-separated-values,text/plain",".tsv")
    ),
    reactable::reactableOutput(ns("cohorts_reactable")),
    hr(),
    actionButton(ns("import_b"), "Import Selected"),
    actionButton(ns("cancel_b"), "Cancel")
  )
}

#' import_cohort_file Server Functions
#'
#' @noRd
mod_import_cohort_file_server <- function(id, main){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #
    # reactive variables
    #
    r <- reactiveValues(
      imported_cohortData = NULL,
      asked_intersect_names = NULL
    )

    #
    # updates output$cohorts_reactable with uploaded file
    #
    observeEvent(input$file_fi, {
      file <- input$file_fi
      ext <- tools::file_ext(file$datapath)

      req(file)

      # update reactable or error
      output$cohorts_reactable <- reactable::renderReactable({

        validate(need(ext == "tsv", "Uploaded file is not a tabular-separated-values (.tsv) file. Please upload a tsv file."))

        tmp_imported_file <- read_tsv(file$datapath, show_col_types = FALSE)

        is_cohortData_message <- tryCatch(
          {as.character(FinnGenTableTypes::is_cohortData(tmp_imported_file, verbose = TRUE))}, message = function(m){m$message}
        )

        validate(
          need(is_cohortData_message=="TRUE",
               str_c("Uploaded tsv file is not in cohortData format.\n These are the reasons: \n", is_cohortData_message))
        )

        r$imported_cohortData <- tmp_imported_file

        r$imported_cohortData %>%
          distinct(COHORT_NAME, COHORT_SOURCE, COHORT_DATAFREEZE) %>%
          reactable::reactable(
            selectionId = ns("selected_index"), selection = "multiple", onClick = "select",
            searchable = TRUE)



      })
    })



    # on import_b click
    observeEvent(input$import_b, {
      main$cohortData <- r$uploaded_cohortData %>%
        filter(COHORT_NAME %in% input$import_picker)
      # r_imported_cohortData$summaryCohortData <- r_imported_cohortData$imported_cohortData %>%
      #   FinnGenTableTypes::summarise_cohortData()

      r$uploaded_cohortData <- FinnGenTableTypes::empty_cohortData()
      r$filtered_cohortData <- FinnGenTableTypes::empty_cohortData()
      output$picker_ui <- NULL
      removeModal()
    })

    #return(reactive(r$filtered_cohortData))

  })
}

## To be copied in the UI
# mod_import_cohort_file_ui("import_cohort_file_ui_1")

## To be copied in the server
# mod_import_cohort_file_server("import_cohort_file_ui_1")

