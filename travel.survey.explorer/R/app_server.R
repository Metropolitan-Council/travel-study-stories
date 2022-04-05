#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  # browser()
  # input category and variable-----
  one_way_input <- mod_input_category_variable_server("1w_input_1")

  mod_input_category_variable_server("2w_input_1")
  mod_input_category_variable_server("2w_input_2")


  one_way_tab <- mod_input_utils_server("input_utils_ui_1", one_way_input)


  # tables -----
  mod_table_one_way_server("table_one_way_ui_1", table_data = one_way_tab)
  mod_table_two_way_server("table_two_way_ui_1")
}
