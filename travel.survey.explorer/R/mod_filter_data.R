#' filter_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
# mod_filter_data_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#   )
# }

#' filter_data_1way Server Functions
#'
#' @noRd
mod_filter_data_1way_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # go_button

    filtered_tbi_tables <- reactive({
      tbi_tables
    })

    observeEvent(input$go_one_way,
                 {

                   # Survey year: Filter all tables to appropriate survey year 1w_input_year

                   # Did the user filter to geography?
                   # if no, nothing
                   # if yes -
                      # figure out if the user has selected a county or a city in 1w_input_geography
                      # if county,
                          # filter hh to those that are in the selected county
                          # then filter the per, trip, day, and veh of the filtered households
                      # if city,
                           # filter hh to those that are in the selected city
                           # then filter the per, trip, day, and veh of the filtered households

                   filtered_tbi_tables() <- # put filtered data here
                 },
                 ignoreInit = TRUE
    )

    # return a filtered version of tbi_tables
    return(filtered_tbi_tables)
  })
}


#' filter_data_2way Server Functions
#'
#' @noRd
mod_filter_data_2way_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # go_button

    filtered_tbi_tables <- reactive({
      tbi_tables
    })

    observeEvent(input$go_two_way,
                 {

                   # Survey year: Filter all tables to appropriate survey year

                   # Did the user filter to geography?
                   # if no, nothing
                   # if yes -
                   # figure out if the user has selected a county or a city in 2w_input_geography
                   # if county,
                   # filter hh to those that are in the selected county
                   # then filter the per, trip, day, and veh of the filtered households
                   # if city,
                   # filter hh to those that are in the selected city
                   # then filter the per, trip, day, and veh of the filtered households

                   filtered_tbi_tables() <- # put filtered data here
                 },
                 ignoreInit = TRUE
    )

    # return a filtered version of tbi_tables
    return(filtered_tbi_tables)
  })
}
