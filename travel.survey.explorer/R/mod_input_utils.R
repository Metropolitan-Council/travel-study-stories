#' input_utils UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_utils_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' @title input_utils Server Functions
#'
#' @noRd
#'
#' @return [shiny::reactiveValues()] `table_data` and `context_data`.
#'
mod_input_utils_server <- function(id, user_inputs, user_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    vals <- reactiveValues()

    # generate table
    table_return <- reactive({
      create_one_way_table(variable_row = user_inputs$variable, user_hhs = user_hhs$user_hhs)
    })

    # find contextual data, like variable names, question text, etc.
    context_return <- reactive({
      # browser()
      tbi_dict %>%
        dplyr::filter(variable == user_inputs$variable) %>%
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
      bindEvent(user_inputs$variable)



    return(vals)
  })
}

## To be copied in the UI
# mod_input_utils_ui("input_utils_ui_1")

## To be copied in the server
# mod_input_utils_server("input_utils_ui_1")
