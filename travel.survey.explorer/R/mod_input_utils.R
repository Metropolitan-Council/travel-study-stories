#' input_utils UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_utils_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' input_utils Server Functions
#'
#' @noRd
#'
#' @return [shiny::reactiveValues()] `table_data` and `context_data`.
#'
mod_input_utils_server <- function(id, user_inputs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # browser()


    vals <- reactiveValues()

    table_return <- reactive({
      create_one_way_table(user_inputs$variable)
      })

    observe({
      vals$table_data <- table_return()
    })


    return(vals)
  })
}

## To be copied in the UI
# mod_input_utils_ui("input_utils_ui_1")

## To be copied in the server
# mod_input_utils_server("input_utils_ui_1")
