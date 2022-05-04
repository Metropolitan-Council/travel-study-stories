#' input_category_variable UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_category_variable_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("category"),
      label = "Category",
      choices = unique(tbi_dict$category),
      selected = "Demographics"
    ),
    selectInput(
      inputId = ns("variable"),
      label = "Variable",
      choices = input_list$Demographics,
      selected = "Age"
    )

    # textOutput(outputId = "question")
  )
}

#' input_category_variable Server Functions
#'
#' @noRd
mod_input_category_variable_server <- function(id) {
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
    # observe({
    #   print(paste0(id, "-", input$category, ":", input$variable))
    # })


    observeEvent(input$variable, {
      vals$variable <- input$variable
    })

    return(vals)
  })
}


## To be copied in the UI
# mod_input_category_variable_ui("input_category_variable_ui_1")

## To be copied in the server
# mod_input_category_variable_server("input_category_variable_ui_1")
