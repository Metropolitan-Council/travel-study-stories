#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  # browser()

  # One-way tab -------

  ## input category and variable-----
  one_way_input <- mod_input_category_variable_server("oneway_input_1")

  ## list of hh ids-----
  one_way_hhs <- mod_filters_oneway_server("filters_oneway_1")

  ## raw table data------
  one_way_tab <- mod_input_utils_server(
    "input_utils_ui_1",
    user_inputs = one_way_input,
    user_hhs = one_way_hhs)

  ## formatted table -----
  mod_table_one_way_server("table_one_way_ui_1",
                           one_way_inputs = one_way_tab
  )

  ## plot--------
  mod_plot_one_way_server("plot_one_way_ui_1", one_way_inputs = one_way_tab)

  # Two-way tab -------

  ## input category and variable-----
  twoway_var_rows <- mod_input_category_row_server("input_category_row_1")
  twoway_var_cols <- mod_input_category_col_server("input_category_col_1")

  ## list of hh ids-----
  two_way_hhs <- mod_filters_oneway_server("filters_twoway_1")

  ## raw table data------
  two_way_tab <- mod_input_utils_two_way_server(
    "input_utils_ui_1",
    variable_row = twoway_var_rows,
    variable_col = twoway_var_cols,
    hh_ids = two_way_hhs)

  ## formatted table -----
  mod_table_two_way_server("table_two_way_ui_1")

  ## plot--------

}
