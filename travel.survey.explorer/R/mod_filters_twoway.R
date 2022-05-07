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
    # Dropdown for Survey Year ----
    # selectInput(
    #   inputId = ns("twoway_input_year"),
    #   "Survey year",
    #   choices = c("2018-2019", "2020-2021"),
    #   selected = "2018-2019",
    #   multiple = FALSE
    # ),

    # Checkbox for HHs in MPO ----
    checkboxInput(
      inputId = ns("twoway_input_mpo"),
      label = "Filter to households in Twin Cities region (MPO)",
      value = FALSE
    ),

    # Dropdown for HH County ----
    selectInput(
      inputId = ns("twoway_input_counties"),
      "Household county",
      choices = unique(na.omit(tbi_tables$hh$hh_county)),
      selected = NULL,
      multiple = TRUE
    ),

    # Dropdown for HH City -----
    selectInput(
      inputId = ns("twoway_input_cities"),
      "Household City/Township",
      choices = unique(na.omit(tbi_tables$hh$hh_city)),
      multiple = TRUE,
      selected = NULL),

  )
}

#' filters_twoway Server Functions
#'
#' @noRd
mod_filters_twoway_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Init reactive values -----
    vals <- reactiveValues()

    # When county/counties selected, ----
    ## update cities dropdown to include only cities within that county ----
    observeEvent(input$twoway_input_counties,
                 {
                   if (!is.null(input$twoway_input_counties))
                   {
                     filtered_cities <- tbi_tables$hh %>%
                       filter(hh_county %in% input$twoway_input_counties) %>%
                       select(hh_city) %>%
                       unique()

                     updateSelectInput(
                       session = session,
                       inputId = "twoway_input_cities",
                       label = "Household City/Township",
                       choices = filtered_cities,
                       selected = NULL,
                       multiple = TRUE
                     )
                   } else {
                     selectInput(
                       session = session,
                       inputId = ns("twoway_input_cities"),
                       "Household City/Township",
                       choices = unique(na.omit(tbi_tables$hh$hh_city)),
                       multiple = TRUE,
                       selected = NULL)
                   }},
                 ignoreInit = TRUE
    )
    # Default list of hh_ids == hh$hh_ids ----------
    all_hh_ids <- tbi_tables$hh$hh_id

    # On go_one_way button, filter tables: ----
    observeEvent(input$go_one_way, {

      # Filter to hh_ids within MPO ----------
      # only if the checkbox has been set to "TRUE"
      if (input$twoway_input_mpo == TRUE) {
        mpo_ids <-
          tbi_tables$hh %>%
          filter(hh_in_mpo == "Household in Twin Cities region") %>%
          select(hh_id)
      } else{
        mpo_ids <- all_hh_ids
      }

      # Filter to hh_ids within selected counties ----------
      # only if counties are not null
      if(!is.null(input$twoway_input_counties)){
        cty_ids <-
          tbi_tables$hh %>%
          filter(hh_cty %in% input$twoway_input_counties) %>%
          select(hh_id)
      } else {
        cty_ids <- all_hh_ids
      }

      # Filter to hh_ids in selected city ----------
      if(!is.null(input$twoway_input_city)){
        ctu_ids <-
          tbi_tables$hh %>%
          filter(hh_ctu %in% input$twoway_input_city) %>%
          select(hh_id)
      } else {
        ctu_ids <- all_hh_ids
      }

      # Filter to hh_ids in selected survey year ----------
      # year_ids <-
      #   tbi_tables$hh %>%
      #   filter(survey == input$twoway_input_year) %>%
      #   select(hh_id)

      filtered_ids <- all_hh_ids %>%
        # inner_join(year_ids) %>%
        inner_join(ctu_ids) %>%
        inner_join(cty_ids) %>%
        inner_join(mpo_ids)

      ### Filter datasets: ----------
      vals$filtered_tbi_tables_2way <-
        purrr::map(tbi_tables,
                   ~ dplyr::filter(., hh_id %in% filtered_ids))
    })
    return(vals)
  })
}

## To be copied in the UI
# mod_filters_twoway_ui("filters_twoway_1")

## To be copied in the server
# mod_filters_twoway_server("filters_twoway_1")
