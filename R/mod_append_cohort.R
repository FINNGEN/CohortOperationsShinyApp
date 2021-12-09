#' server_append_cohort Server Functions
#'
#' @noRd
mod_append_cohort_server <- function(id, r_cohorts, r_append_cohort){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #
    # reactive variables
    #
    r <- shiny::reactiveValues(
      asked_intersect_names = NULL,
      append_accepted_counter = 0
    )

    #
    # if r_append_cohort is modified
    #
    observeEvent(r_append_cohort$cohortData, {
      req(r_append_cohort$cohortData)
      # ask if existing cohorts should be replaced
      intersect_names <- dplyr::inner_join(
        r_cohorts$cohortData %>% dplyr::distinct(COHORT_SOURCE, COHORT_NAME),
        r_append_cohort$cohortData %>% dplyr::distinct(COHORT_SOURCE, COHORT_NAME),
        by = c("COHORT_SOURCE", "COHORT_NAME")
      ) %>%
        dplyr::mutate(name = stringr::str_c(COHORT_SOURCE, COHORT_NAME, sep = " ")) %>%
        dplyr::pull(name)


      if (length(intersect_names) > 0) {
        shinyWidgets::confirmSweetAlert(
          session = session,
          inputId = "asked_intersect_names_alert",
          type = "question",
          title = "Some selected cohorts had been alredy imported:",
          text = htmltools::HTML(
            "The following cohorts had been alredy imported: <ul>",
            stringr::str_c(stringr::str_c("<li> ", intersect_names, "</li>"), collapse = ""),
            "</ul> Should these be replaced or ignored."
          ),
          btn_labels = c("Not-import", "Replace"),
          html = TRUE
        )
      } else {
        r$asked_intersect_names <- TRUE
      }
    })
    ## just pass the info to make it writable
    shiny::observe({
      r$asked_intersect_names <- input$asked_intersect_names_alert
    })

    #
    # confirmSweetAlert asked_intersect_names
    #
    shiny::observeEvent(r$asked_intersect_names, {
      # take names from main and imported
      intersect_names <- dplyr::inner_join(
        r_cohorts$cohortData %>% dplyr::distinct(COHORT_SOURCE, COHORT_NAME),
        r_append_cohort$cohortData %>% dplyr::distinct(COHORT_SOURCE, COHORT_NAME),
        by = c("COHORT_SOURCE", "COHORT_NAME")
      )

      tmp_append_cohort <- r_append_cohort$cohortData
      if (r$asked_intersect_names) {
        # remove from main
        r_cohorts$cohortData <- dplyr::anti_join(
          r_cohorts$cohortData,
          intersect_names,
          by = c("COHORT_SOURCE", "COHORT_NAME")
        )
      } else {
        tmp_append_cohort <- dplyr::anti_join(
          tmp_append_cohort,
          intersect_names,
          by = c("COHORT_SOURCE", "COHORT_NAME")
        )
      }

      r_cohorts$cohortData <- dplyr::bind_rows(
        r_cohorts$cohortData,
        tmp_append_cohort
      )
      # re calcualte all, to get the rigth years in dates
      r_cohorts$summaryCohortData <- FinnGenTableTypes::summarise_cohortData(r_cohorts$cohortData)

      # reset module
      r$asked_intersect_names <- NULL

      # pass action
      r$append_accepted_counter <- r$append_accepted_counter+1
    })


    return(reactive(r$append_accepted_counter))
  })
}

## To be copied in the UI
# mod_server_append_cohort_ui("server_append_cohort_ui_1")

## To be copied in the server
# mod_server_append_cohort_server("server_append_cohort_ui_1")
