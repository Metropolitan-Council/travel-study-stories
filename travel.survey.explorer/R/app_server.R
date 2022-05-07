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

  # inputId inside the server needs to match ui inputId
  one_way_input <- mod_input_category_variable_server("oneway_input_1")
  one_way_data <- mod_filters_oneway_server()
  one_way_tab <- mod_input_utils_server(
    "input_utils_ui_1",
    user_inputs = one_way_input,
    user_data = one_way_data)

  mod_input_category_variable_server("twoway_input_1")
  mod_input_category_variable_server("twoway_input_2")




  mod_plot_one_way_server("plot_one_way_ui_1", one_way_inputs = one_way_tab)


  # tables -----
  mod_table_one_way_server("table_one_way_ui_1",
    one_way_inputs = one_way_tab
  )

  mod_table_two_way_server("table_two_way_ui_1")
}
