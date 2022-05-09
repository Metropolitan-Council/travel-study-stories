#' filters_oneway UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom dplyr filter select inner_join
#' @importFrom purrr pluck
#' @import bit64
mod_filters_oneway_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # Dropdown for Survey Year ----
    # selectInput(
    #   inputId = ns("oneway_input_year"),
    #   "Survey year",
    #   choices = c("2018-2019", "2020-2021"),
    #   selected = "2018-2019",
    #   multiple = FALSE
    # ),

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
      choices = unique(na.omit(tbi_tables$hh$hh_county)),
      selected = NULL,
      multiple = TRUE
    ),

    # Dropdown for HH City -----
    selectInput(
      inputId = ns("oneway_input_cities"),
      "Household City/Township",
      choices = unique(na.omit(tbi_tables$hh$hh_city)),
      multiple = TRUE,
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

    # Default list of hh_ids == hh$hh_ids ----------
    all_hh_ids <- tbi_tables$hh %>%
      dplyr::select(hh_id)

    # vals$hh_ids <- all_hh_ids %>%
    #   purrr::pluck(1)

    # When county/counties selected, ----
    # update cities dropdown to include only cities within that county ----
    observeEvent(input$oneway_input_counties,
                 {
                   if (!is.null(input$oneway_input_counties))
                   {
                     filtered_cities <- tbi_tables$hh %>%
                       dplyr::filter(hh_county %in% input$oneway_input_counties) %>%
                       dplyr::select(hh_city) %>%
                       unique() %>%
                       na.omit()

                     updateSelectInput(
                       session = session,
                       inputId = "oneway_input_cities",
                       label = "Household City/Township",
                       choices = filtered_cities$hh_city,
                       selected = NULL
                     )
                   } else {
                     selectInput(
                       session = session,
                       inputId = ns("oneway_input_cities"),
                       "Household City/Township",
                       choices = unique(na.omit(tbi_tables$hh$hh_city)),
                       selected = NULL)
                   }},
                 ignoreInit = TRUE
    )



    # Filter tables as we go (not reacting to button): ----
    reactive({

      # Filter to hh_ids within MPO ----------
      # only if the checkbox has been set to "TRUE"
      if (input$oneway_input_mpo == TRUE) {
        mpo_ids <-
          tbi_tables$hh %>%
          dplyr::filter(hh_in_mpo == "Household in Twin Cities region") %>%
          dplyr::select(hh_id)
      } else{
        mpo_ids <- all_hh_ids
      }

      # Filter to hh_ids within selected counties ----------
      # only if counties are not null
      # if(!is.null(input$oneway_input_counties)){
      #   cty_ids <-
      #     tbi_tables$hh %>%
      #     dplyr::filter(hh_cty %in% input$oneway_input_counties) %>%
      #     dplyr::select(hh_id)
      # } else {
      #   cty_ids <- all_hh_ids
      # }

      # Filter to hh_ids in selected city ----------
      # if(!is.null(input$oneway_input_city)){
      #   ctu_ids <-
      #     tbi_tables$hh %>%
      #     dplyr::filter(hh_ctu %in% input$oneway_input_city) %>%
      #     dplyr::select(hh_id)
      # } else {
      #   ctu_ids <- all_hh_ids
      # }

      # Filter to hh_ids in selected survey year ----------
      # year_ids <-
      #   tbi_tables$hh %>%
      #   dplyr::filter(survey == input$oneway_input_year) %>%
      #   dplyr::select(hh_id)

      vals$user_hhs <- all_hh_ids %>%
        # inner_join(year_ids, by = "hh_id") %>%
        # inner_join(ctu_ids, by = "hh_id") %>%
        # inner_join(cty_ids, by = "hh_id") %>%
        dplyr::inner_join(mpo_ids, by = "hh_id") %>%
        purrr::pluck(1)

    })
    return(vals)
  })
}

## To be copied in the UI
# mod_filters_oneway_ui("filters_oneway_1")

## To be copied in the server
# mod_filters_oneway_server("filters_oneway_1")
