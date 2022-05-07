#' filters_oneway UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_filters_oneway_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Dropdown for Survey Year ----
    selectInput(
      inputId = ns("oneway_input_year"),
      "Survey year",
      choices = c("2018-2019", "2020-2021"),
      multiple = FALSE
    ),

    # Checkbox for HHs in MPO ----
    checkboxInput(
      inputId = ns("oneway_input_mpo"),
      label = "Filter to households in Twin Cities region (MPO)",
      value = FALSE
    ),

    # Dropdown for HH County ----
    selectInput(
      inputId = ns("oneway_input_counties"),
      "Household county",
      multiple = TRUE,
      selected = unique(tbi_tables$hh$hh_county),
      choices = unique(tbi_tables$hh$hh_county)
    ),

    # Dropdown for HH City -----
    selectInput(
      inputId = ns("oneway_input_cities"),
      "Household City/Township",
      multiple = TRUE,
      choices = unique(tbi_tables$hh$hh_city),
      selected = NULL),

    # Button: Go One Way, Create Table ----
    actionButton(inputId = ns("go_one_way"), "Create Table")
  )
}

#' filters_oneway Server Functions
#'
#' @noRd
mod_filters_oneway_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Init reactive values -----
    vals <- reactiveValues()

    # Update cities dropdown when a county is selected ----
    observeEvent(input$oneway_input_counties,
                 {
                   filtered_cities <- tbi_tables$hh %>%
                     filter(hh_county %in% input$oneway_input_counties) %>%
                     select(hh_city) %>%
                     unique()


                   updateSelectInput(
                     session = session,
                     inputId = "oneway_input_cities",
                     label = "Household City/Township",
                     choices = filtered_cities
                   )
                 },
                 ignoreInit = TRUE
    )


    # Default list of hh_ids == hh$hh_ids ----------
    filtered_hh_ids_1way <- tbi_tables$hh$hh_id

    # On go_one_way button, filter tables: ----
    observeEvent(input$go_one_way, {

      # Filter to hh_ids within MPO ----------
      # only if the checkbox has been set to "TRUE"
      if (input$oneway_input_mpo == TRUE) {
        filtered_hh_ids_1way <-
          tbi_tables$hh %>%
          filter(hh_in_mpo == "Household in Twin Cities region") %>%
          # keep only the household ids in the existing filtered subset:
          right_join(filtered_hh_ids_1way) %>%
          select(hh_id)
      } else{
      }

      # Filter to hh_ids within selected counties ----------
      # only if counties are not null
      if(!is.null(input$oneway_input_counties)){
        filtered_hh_ids_1way <-
          tbi_tables$hh %>%
          filter(hh_cty %in% input$oneway_input_counties) %>%
          right_join(filtered_hh_ids_1way) %>%
          select(hh_id)
      } else {
      }

      # Filter to hh_ids in selected city ----------
      filtered_hh_ids_1way() <-
        tbi_tables$hh %>%
        filter(hh_ctu %in% input$oneway_input_cities) %>%
        right_join(filtered_hh_ids_1way()) %>%
        select(hh_id)

      # Filter to hh_ids in selected survey year ----------
      filtered_hh_ids_1way <-
        tbi_tables$hh %>%
        filter(survey == input$oneway_input_year) %>%
        right_join(filtered_hh_ids_1way()) %>%
        select(hh_id)

      ### Filter datasets: ----------
      vals$filtered_tbi_tables_1way <-
        purrr::map(tbi_tables,
                   ~ dplyr::filter(., hh_id %in% filtered_hh_ids_1way))
    })
    return(vals)
  })
}

## To be copied in the UI
# mod_filters_oneway_ui("filters_oneway_1")

## To be copied in the server
# mod_filters_oneway_server("filters_oneway_1")
