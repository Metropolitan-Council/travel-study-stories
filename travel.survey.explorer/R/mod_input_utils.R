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
mod_input_utils_server <- function(id, user_inputs, user_hhs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    vals <- reactiveValues()

    # generate table
    one_way_rt_list <- reactive({
      create_one_way_table(variable_row = user_inputs$variable,
                           hh_ids = user_hhs$hh_ids)
    })

    observe({
      # browser()
      one_way_rt <- one_way_rt_list()

      vals$table_return <- one_way_rt$table
      vals$context_return <- one_way_rt$definitions
      vals$summary_return <- one_way_rt$tab_mean

      print(vals$table_return) # eventually comment out

      return(vals)
    }) %>%
      bindEvent(user_inputs$variable, user_hhs$hh_ids)

    return(vals)
  })
}

## To be copied in the UI
# mod_input_utils_ui("input_utils_ui_1")

## To be copied in the server
# mod_input_utils_server("input_utils_ui_1")
