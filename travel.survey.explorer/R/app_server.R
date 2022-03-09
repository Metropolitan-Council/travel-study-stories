#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # input category and variable-----
  mod_input_category_variable_server("1w_input_1")
  mod_input_category_variable_server("2w_input_1")
  mod_input_category_variable_server("2w_input_2")


  # tables -----
  mod_table_one_way_server("table_one_way_ui_1")
  mod_table_two_way_server("table_two_way_ui_1")
}
