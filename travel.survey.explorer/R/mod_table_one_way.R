#' table_one_way UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_table_one_way_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("table"))
  )
}

#' table_one_way Server Functions
#'
#' @noRd
mod_table_one_way_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    missing_codes <- c(
      "-9998", "995", "-9999", "-1", "Missing: Non-response", "Missing: Skip logic",
      "Missing: Error", "Missing: Technical error", "Missing: Non-imputable"
    )

    # find the table where the variable lives:
    # this_variable <- input$variable


    # ~~ ~ ~ ~ ~ ~ test  ~ ~ ~ ~ ~ ~
    this_variable <- "bike_freq"

    this_table <-
      tbi_dict %>%
      filter(variable == this_variable) %>%
      select(which_table) %>%
      unique() %>%
      magrittr::extract2(1)

    this_weight <-
      tbi_dict %>%
      filter(variable == this_variable) %>%
      select(wt_field) %>%
      unique() %>%
      magrittr::extract2(1)


    # a more generalized version:
    output$table <- DT::renderDataTable({
      tbi_tables[[this_table]] %>%
        select(sym(this_variable), sym(this_weight), hh_id) %>%
        # filter where the variable is missing (missing codes = "Missing: No Response", "Missing: skip logic").
        # this list of missing codes is created in data_compile.R, line ~105.
        filter(!(get(this_variable) %in% missing_codes)) %>%
        filter(!is.na(get(this_variable))) %>%
        # get rid of empty "" values as well:
        filter(!(get(this_variable) == "")) %>%
        # filter out weight is NA
        filter(!is.na(get(this_weight))) %>%
        # clean up:
        droplevels() %>%
        # big N sample size - for the whole data frame:
        mutate(
          total_N = length(hh_id), # raw sample size - number of people, trips, households, days
          total_N_hh = length(unique(hh_id))
        ) %>% # total number of households in sample
        srvyr::as_survey_design(weights = !!this_weight) %>%
        group_by(
          get(this_variable),
          # grouping by number of samples, number of households to keep this info
          total_N, total_N_hh
        ) %>%
        summarize(
          group_N = length(hh_id), # raw sample size - number of people, trips, households, days (by group)
          group_N_hh = length(unique(hh_id)), # number of households in sample (by group)
          expanded_total = srvyr::survey_total(), # expanded total and SE
          estimated_prop = srvyr::survey_prop() # estimated proportion (0.0 - 1.0) and SE; multiply by 100 for percentages.
        ) %>%
        # rename the column back to the original name - it gets weird for some reason
        rename(!!quo_name(this_variable) := `get(this_variable)`)
    })
  })
}

## To be copied in the UI
# mod_table_one_way_ui("table_one_way_ui_1")

## To be copied in the server
# mod_table_one_way_server("table_one_way_ui_1")
