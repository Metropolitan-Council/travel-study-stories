#' input_category_variable UI Function
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param starting_category_choices list, category choices. Default is
#'     `unique(tbi_dict$category)`.
#' @param starting_category_selected character, starting category
#' @param starting_variable_choices list, starting variable choices. Default is
#'     `input_list$Demographics`
#' @param starting_variable_selected character, starting variable selection.
#'     Default is `"Age"`
#'
#' @description A shiny Module.
#' @note Category and variable must be corresponding.
#'
#' ```
#'
#' # good
#' mod_input_category_variable_ui(id = "an_id",
#'     starting_category_selected = "Demographics",
#'     starting_variable_choices = input_list$Demographics,
#'     starting_variable_selected = "Age")
#'
#' # bad
#' mod_input_category_variable_ui(id = "an_id",
#'     starting_category_selected = "Demographics",
#'     starting_variable_choices = input_list$Trips,
#'     starting_variable_selected = "City of Trip Origin")
#'
#' ```
#'
#'
#' @importFrom shiny NS tagList
mod_input_category_variable_ui <- function(id,
                                           starting_category_choices = unique(tbi_dict$category),
                                           starting_category_selected = "Demographics",
                                           starting_variable_choices = input_list$Demographics,
                                           starting_variable_selected = "Age") {
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("category"),
      label = "Category",
      choices = starting_category_choices,
      selected = starting_category_selected
    ),
    selectInput(
      inputId = ns("variable"),
      label = "Variable",
      choices = starting_variable_choices,
      selected = starting_variable_selected
    )

    # textOutput(outputId = "question")
  )
}

#'  @title input_category_variable Server Functions
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
