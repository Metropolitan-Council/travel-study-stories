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

#' @title filter_data_1way Server Functions
#'
#' @noRd
mod_filter_data_1way_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ### Default value for filtered_tables == tbi_tables ---
    filtered_tbi_tables_1way <- reactive(tbi_tables)

    ### Default list of hh_ids == hh$hh_ids ----------
    filtered_hh_ids_1way <- reactive(tbi_tables$hh$hh_id)

    # On go_one_way button, filter
    observeEvent(input$go_one_way, {

      ### Filter to hh_ids within MPO ----------
      if(input$twoway_input_mpo == TRUE){
        filtered_hh_ids_1way() <-
          tbi_tables$hh %>%
          filter(hh_in_mpo == "Household in Twin Cities region") %>%
          # keep only the household ids in the existing filtered subset:
          right_join(filtered_hh_ids_1way()) %>%
          select(hh_id)
      } else{}

      ### Filter to hh_ids within selected counties ----------
      filtered_hh_ids_1way() <-
          tbi_tables$hh %>%
          filter(hh_cty %in% input$oneway_input_counties) %>%
          right_join(filtered_hh_ids_1way()) %>%
          select(hh_id)

      ### Filter datasets: ----------
      filtered_tbi_tables_1way() <- purrr::map(tbi_tables, ~ dplyr::filter(., hh_id %in% filtered_hh_ids_1way()))


    })
  })
}


#' @title filter_data_2way Server Functions
#'
#' @noRd
mod_filter_data_2way_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ### Default value for filtered_tables == tbi_tables ---
    filtered_tbi_tables_2way <- reactive(tbi_tables)

    ### Default list of hh_ids == hh$hh_ids ----------
    filtered_hh_ids_2way <- reactive(tbi_tables$hh$hh_id)

    # On go_one_way button, filter
    observeEvent(input$go_one_way, {

      ### Filter to hh_ids within MPO ----------
      if(input$twoway_input_mpo == TRUE){
        filtered_hh_ids_2way() <-
          tbi_tables$hh %>%
          filter(hh_in_mpo == "Household in Twin Cities region") %>%
          # keep only the household ids in the existing filtered subset:
          right_join(filtered_hh_ids_2way()) %>%
          select(hh_id)
      } else{}

      ### Filter to hh_ids within selected counties ----------
      filtered_hh_ids_2way() <-
        tbi_tables$hh %>%
        filter(hh_cty %in% input$oneway_input_counties) %>%
        right_join(filtered_hh_ids_2way()) %>%
        select(hh_id)

      ### Filter datasets: ----------
      filtered_tbi_tables_2way() <- purrr::map(tbi_tables, ~ dplyr::filter(., hh_id %in% filtered_hh_ids_2way()))


    })
  })
}
