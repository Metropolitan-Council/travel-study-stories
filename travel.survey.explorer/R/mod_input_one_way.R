#' input_one_way UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_one_way_ui <- function(id){
  ns <- NS(id)
  tagList(

    selectInput(
      inputId = ns("category"),
      label = "Category",
      choices = shinipsum::lorem_words[1:5]
    ),


    selectInput(
      inputId = ns("variable"),
      label = "Variable",
      choices = shinipsum::lorem_words[10:20]
    )


  )
}

#' input_one_way Server Functions
#'
#' @noRd
mod_input_one_way_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_input_one_way_ui("input_one_way_ui_1")

## To be copied in the server
# mod_input_one_way_server("input_one_way_ui_1")
