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
      # hh <- hh %>%
      # filter(hh_in_mpo == "in_mpo")
      # ### Trim veh: Vehicles owned by HHs in MPO----------
      # veh <- veh %>%
      #   left_join(hh %>% select(hh_id, hh_in_mpo)) %>%
      #   filter(hh_in_mpo == "in_mpo") %>%
      #   select(-hh_in_mpo)
      #
      # ### Trim per: people who live in MPO----------
      # per <- per %>%
      #   left_join(hh %>% select(hh_id, hh_in_mpo)) %>%
      #   filter(hh_in_mpo == "in_mpo") %>%
      #   select(-hh_in_mpo)
      #
      # ### Trim day: days for people that live in MPO----------
      # day <- day %>%
      #   left_join(hh %>% select(hh_id, hh_in_mpo)) %>%
      #   filter(hh_in_mpo == "in_mpo") %>%
      #   select(-hh_in_mpo)
      #
      # ### Trim trip: trips made by HHs in MPO ----------
      # trip <- trip %>%
      #   left_join(select(per, person_id, hh_id)) %>%
      #   left_join(select(hh, hh_id, hh_in_mpo)) %>%
      #   filter(hh_in_mpo == "in_mpo") %>%
      #   select(-hh_in_mpo)

    })

    # observeEvent(input$go_one_way,
    #              {
    #
    #                # Survey year: Filter all tables to appropriate survey year 1w_input_year
    #
    #                # Did the user filter to geography?
    #                # if no, nothing
    #                # if yes -
    #                   # figure out if the user has selected a county or a city in 1w_input_geography
    #                   # if county,
    #                       # filter hh to those that are in the selected county
    #                       # then filter the per, trip, day, and veh of the filtered households
    #                   # if city,
    #                        # filter hh to those that are in the selected city
    #                        # then filter the per, trip, day, and veh of the filtered households
    #
    #                filtered_tbi_tables() <- # put filtered data here
    #              },
    #              ignoreInit = TRUE
    # )

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

    # observeEvent(input$go_two_way,
    #              {
    #
    #                # Survey year: Filter all tables to appropriate survey year
    #
    #                # Did the user filter to geography?
    #                # if no, nothing
    #                # if yes -
    #                # figure out if the user has selected a county or a city in 2w_input_geography
    #                # if county,
    #                # filter hh to those that are in the selected county
    #                # then filter the per, trip, day, and veh of the filtered households
    #                # if city,
    #                # filter hh to those that are in the selected city
    #                # then filter the per, trip, day, and veh of the filtered households
    #
    #                filtered_tbi_tables() <- # put filtered data here
    #              },
    #              ignoreInit = TRUE
    # )

    # return a filtered version of tbi_tables
    return(filtered_tbi_tables)
  })
}
