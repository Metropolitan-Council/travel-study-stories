#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_table_one_way_server("table_one_way_ui_1")
  mod_table_two_way_server("table_two_way_ui_1")
}
