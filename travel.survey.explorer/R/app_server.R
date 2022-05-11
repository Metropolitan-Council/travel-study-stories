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
  user_var_1way <- mod_input_category_variable_server("oneway_input_1")

  ## list of hh ids-----
  user_hhs_1way <- mod_filters_oneway_server("filters_oneway_1")

  ## raw table data------ this is where it seems to break!
  one_way_rts <- mod_input_utils_server(
    "input_utils_ui_1",
    user_var = user_var_1way,
    user_hhs = user_hhs_1way)

  # formatted table -----
  mod_table_one_way_server("table_one_way_ui_1",
                           one_way_table_inputs = one_way_rts
  )

  ## plot--------
  mod_plot_one_way_server("plot_one_way_ui_1", one_way_plot_inputs = one_way_rts)

  # Two-way tab -------

  ## input category and variable-----
  user_row_var <- mod_input_category_row_server("input_category_row_1")

  user_col_var <- mod_input_category_col_server("input_category_col_1")

  ## list of hh ids-----
  user_hhs_2way <- mod_filters_twoway_server("filters_twoway_1")

  ## raw table data------
  two_way_tab <- mod_input_utils_two_way_server(
    "input_utils_two_way_ui_1",
    util_variable_row = user_row_var,
    util_variable_col = user_col_var,
    util_hh_ids = user_hhs_2way)


  mod_table_two_way_server("table_two_way_ui_1", two_way_table_inputs = two_way_tab)
  mod_plot_two_way_server("plot_two_way_ui_1", two_way_plot_inputs = two_way_tab)


  # ## formatted table -----
  #
  # ## plot--------

}
