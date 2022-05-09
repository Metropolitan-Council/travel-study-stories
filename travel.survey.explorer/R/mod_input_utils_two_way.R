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
    table_return <- reactive({
      create_two_way_table(variable_row = variable_row$variable_row,
                           variable_col = variable_col$variable_col,
                           hh_ids = hh_ids$hh_ids)
    })

    # find contextual data, like variable names, question text, etc.
    context_return <- reactive({
      # browser()
      tbi_dict %>%
        dplyr::filter(variable %in% c(variable_row$variable_row, variable_col$variable_col)) %>%
        dplyr::select(variable_label, survey_question, variable_logic, which_table, category) %>%
        unique()
    })


    observe({
      # browser()
      vals$table_data <- table_return()
      vals$context_data <- context_return()

      print(vals$table_data)

      return(vals)
    }) %>%
      # bindCache(user_inputs$variable, user_hhs$hh_ids) %>%
      bindEvent(variable_row$variable_row, variable_col$variable_col, user_two_way_hhs$two_way_hhs)

    return(vals)
  })
}

## To be copied in the UI
# mod_input_utils_two_way_ui("input_utils_ui_1")

## To be copied in the server
# mod_input_utils_two_way_server("input_utils_ui_1")
