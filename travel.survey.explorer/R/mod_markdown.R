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
    reference_table(),
    tags$footer(
      "This project is open-source. See our GitHub repository here",
      tags$a(
        href = "https://github.com/Metropolitan-Council/travel-study-stories",
        shiny::icon("external-link-alt", lib = "font-awesome"),
        target = "_blank"
      ),
      #   tags$br(),
      #   "App last updated ",
      #   "2022-06-24",
      style = "font-size: 1.5rem;
               display: block;
               text-align: right;
               padding: 1%;"
    )

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
