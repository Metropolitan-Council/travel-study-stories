#' filters_twoway UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_filters_twoway_ui <- function(id){
  ns <- NS(id)
  tagList(
    checkboxInput(
      inputId = ns("twoway_input_mpo"),
      label = "Filter to households in Twin Cities region (MPO)",
      value = FALSE
    ),
    selectInput(
      inputId = ns("twoway_input_year"),
      "Survey Year",
      choices = c("2018-2019", "2020-2021"),
      multiple = FALSE,
      selected = "2018-2019"
    ),
    selectInput(
      inputId = ns("twoway_input_counties"),
      "Household County",
      multiple = T,
      choices =
        c(
          "Hennepin MN",
          "Ramsey MN",
          "Dakota MN",
          "Anoka MN",
          "Washington MN",
          "Wright MN",
          "Scott MN",
          "Ring Counties"
        ),
      selected =
        c(
          "Hennepin MN",
          "Ramsey MN",
          "Dakota MN",
          "Anoka MN",
          "Washington MN",
          "Wright MN",
          "Scott MN",
          "Ring Counties"
        )
    )
    # selectInput(
    #   inputId = ns("twoway_input_cities"),
    #   "Household City/Township",
    #   multiple = T,
    #   choices =
    #     c("Minneapolis", "St. Paul"),
    #   selected = ""
    # )

  )
}

#' filters_twoway Server Functions
#'
#' @noRd
mod_filters_twoway_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    vals <- reactiveValues()

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
        filter(hh_cty %in% input$twoway_input_counties) %>%
        right_join(filtered_hh_ids_2way()) %>%
        select(hh_id)

      # ### Filter to hh_ids in selected city ----------
      # filtered_hh_ids_2way() <-
      #   tbi_tables$hh %>%
      #   filter(hh_ctu %in% input$twoway_input_cities) %>%
      #   right_join(filtered_hh_ids_2way()) %>%
      #   select(hh_id)

      ### Filter to hh_ids in selected survey year ----------
      filtered_hh_ids_2way() <-
        tbi_tables$hh %>%
        filter(survey_year == input$twoway_input_year) %>%
        right_join(filtered_hh_ids_2way()) %>%
        select(hh_id)

      ### Filter datasets: ----------
      filtered_tbi_tables_2way() <- purrr::map(tbi_tables, ~ dplyr::filter(., hh_id %in% filtered_hh_ids_2way()))
    })
  })
}

## To be copied in the UI
# mod_filters_twoway_ui("filters_twoway_1")

## To be copied in the server
# mod_filters_twoway_server("filters_twoway_1")
