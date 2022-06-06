#' input_category_col UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_category_col_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("category"),
      label = "Category",
      choices = unique(tbi_dict$category),
      selected = "Trips"
    ),
    selectInput(
      inputId = ns("variable"),
      label = "Variable",
      choices = input_list$Trips
      # selected = "Trip purpose - broad categories"
    )

    # textOutput(outputId = "question")
  )
}

#' input_category_col Server Functions
#'
#' @noRd
mod_input_category_col_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    vals <- reactiveValues()

    observeEvent(input$category,
      {
        # update variable input by fetching variables specific
        # to the input$category
        updateSelectInput(
          session = session,
          inputId = "variable",
          label = "Variable",
          choices = input_list[input$category][[1]]
        )
      },
      ignoreInit = TRUE
    )


    # output$question <- renderText({
    #   return("text")
    #   # browser()
    #   # input_question_list[input$variable]
    # })


    # print the selected input$category and input$variable
    observe({
      print(paste0(id, "-", input$category, ":", input$variable))
    })


    observeEvent(input$variable, {
      vals$variable <- input$variable
    })

    return(vals)
  })
}

## To be copied in the UI
# mod_input_category_col_ui("input_category_col_1")

## To be copied in the server
# mod_input_category_col_server("input_category_col_1")
