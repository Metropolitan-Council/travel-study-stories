#' @title Markdown module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_markdown_ui <- function(id) {
  ns <- NS(id)

  shiny::div(
    id = "notes",
    includeMarkdown(system.file(paste0("app/www/",id,".md"),
                                package = "travel.survey.explorer"
    )),
    reference_table()
  )
}

#' @title about Server Functions
#'
#' @noRd
mod_markdown_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_markdown_ui("about_ui_1")

## To be copied in the server
# mod_about_server("about_ui_1")
