#' input_utils UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_utils_two_way_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' @title input_utils Server Functions
#'
#' @noRd
#'
#' @return [shiny::reactiveValues()] `table_data` and `context_data`.
#'
mod_input_utils_two_way_server <- function(id, variable_row, variable_col, hh_ids) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    vals <- reactiveValues()

    # generate table
    two_way_rt_list <- reactive({
      create_two_way_table(variable_row = variable_row$variable_row,
                           variable_col = variable_col$variable_col,
                           hh_ids = hh_ids$hh_ids)
    })


    observe({
      # browser()
      two_way_rt <- two_way_rt_list()

      vals$table_return <- two_way_rt$table
      vals$context_return <- two_way_rt$definitions
      vals$summary_return <- two_way_rt$tab_mean

    }) %>%
      # bindCache(user_inputs$variable, user_hhs$hh_ids) %>%
      bindEvent(variable_row$variable_row, variable_col$variable_col, hh_ids$hh_ids)

    return(vals)
  })
}

## To be copied in the UI
# mod_input_utils_two_way_ui("input_utils_ui_1")

## To be copied in the server
# mod_input_utils_two_way_server("input_utils_ui_1")
