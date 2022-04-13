#' table_one_way UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_table_one_way_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("table"), width = "80%")
  )
}

#' table_one_way Server Functions
#'
#' @noRd
mod_table_one_way_server <- function(id, table_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$table <- DT::renderDataTable({
      DT::datatable(table_data$table_data %>%
                      select(1, estimated_prop, estimated_prop_se),
                    rownames = F)
    })

  })
}

## To be copied in the UI
# mod_table_one_way_ui("table_one_way_ui_1")

## To be copied in the server
# mod_table_one_way_server("table_one_way_ui_1")
