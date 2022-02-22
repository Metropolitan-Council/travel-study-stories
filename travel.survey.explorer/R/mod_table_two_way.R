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

#' table_two_way Server Functions
#'
#' @noRd
mod_table_two_way_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$table <- DT::renderDataTable({
      shinipsum::random_DT(
        nrow = 40,
        ncol = 14
      )
    })
  })
}

## To be copied in the UI
# mod_table_two_way_ui("table_two_way_ui_1")

## To be copied in the server
# mod_table_two_way_server("table_two_way_ui_1")
