#' table_two_way UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_table_two_way_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("table"))
  )
}

#' @title table_two_way Server Functions
#'
#' @noRd
mod_table_two_way_server <- function(id, two_way_table_inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    w <- waiter::Waiter$new(ns("table"),
                            color = waiter::transparent(0.5)
    )
    output$table <- DT::renderDataTable({
      w$show()

      table_data <- two_way_table_inputs$table_return
      context_row <- two_way_table_inputs$context_row_return
      context_col <- two_way_table_inputs$context_col_return

      summary_data <- two_way_table_inputs$summary_return
      display_data <- two_way_table_inputs$table_display

      # browser()

      DT::datatable(display_data,
                    rownames = display_data[1]) %>%
        DT::formatPercentage(columns = c(2:ncol(display_data)), digits = 2)

    })
  })
}

## To be copied in the UI
# mod_table_two_way_ui("table_two_way_ui_1")

## To be copied in the server
# mod_table_two_way_server("table_two_way_ui_1")
