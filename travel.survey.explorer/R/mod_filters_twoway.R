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

    # Button: Go two Way, Create Table ----
    actionButton(inputId = ns("go_two_way"), "Create Crosstabs")
  )
}

#' filters_twoway Server Functions
#'
#' @noRd
mod_filters_twoway_server <- function(id){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update Select Inputs----

    ## When "Restrict HHs to MPO region" is checked, ----
    # update counties/cities to include only those within the MPO boundary ----

    ## When county/counties selected, ----
    # update cities dropdown to include only cities within that county ----
    observeEvent(input$twoway_input_counties,
                 {
                   if (!is.null(input$twoway_input_counties))
                   {
                     filtered_cities <- tbi_tables$hh %>%
                       dplyr::filter(hh_county %in% input$twoway_input_counties) %>%
                       dplyr::select(hh_city) %>%
                       unique() %>%
                       na.omit()

                     updateSelectInput(
                       session = session,
                       inputId = "twoway_input_cities",
                       label = "Household City/Township",
                       choices = filtered_cities$hh_city,
                       selected = NULL
                     )
                   } else {
                     selectInput(
                       session = session,
                       inputId = ns("twoway_input_cities"),
                       "Household City/Township",
                       choices = unique(na.omit(tbi_tables$hh$hh_city)),
                       selected = NULL)
                   }},
                 ignoreInit = TRUE
    )



    # Create HH ID list----

    ## Init reactive values -----
    vals <- reactiveValues()

    ## Init HH ID list----
    all_hh_ids <- tbi_tables$hh %>%
      dplyr::select(hh_id)

    observe({
      # using "observe" rather than "observeEvent" - calculating as we go

      ## Filter to MPO----
      # only if the checkbox has been set to "TRUE"
      if (input$twoway_input_mpo == TRUE) {
        mpo_ids <-
          tbi_tables$hh %>%
          dplyr::filter(hh_in_mpo == "Household in Twin Cities region") %>%
          dplyr::select(hh_id)
      } else{
        mpo_ids <- all_hh_ids
      }

      ## Filter to County----
      # only if counties are not null
      if(!is.null(input$twoway_input_counties)){
        cty_ids <-
          tbi_tables$hh %>%
          dplyr::filter(hh_county %in% input$twoway_input_counties) %>%
          dplyr::select(hh_id)
      } else {
        cty_ids <- all_hh_ids
      }

      ## Filter to City----
      if(!is.null(input$twoway_input_cities)){
        ctu_ids <-
          tbi_tables$hh %>%
          dplyr::filter(hh_city %in% input$twoway_input_cities) %>%
          dplyr::select(hh_id)
      } else {
        ctu_ids <- all_hh_ids
      }

      ## Filter to Year----
      # year_ids <-
      #   tbi_tables$hh %>%
      #   dplyr::filter(survey == input$twoway_input_year) %>%
      #   dplyr::select(hh_id)

      ## Final HH ID list----
      # inner join is the key here: gets the intersecting set.
      vals$hh_ids <- all_hh_ids %>%
        #  dplyr::inner_join(year_ids, by = "hh_id") %>%
        dplyr::inner_join(ctu_ids, by = "hh_id") %>%
        dplyr::inner_join(cty_ids, by = "hh_id") %>%
        dplyr::inner_join(mpo_ids, by = "hh_id") %>%
        # you need to use "innerjoin" on a data frame,
        # but we want a list of hh ids in the end --
        # so we use pluck to get it
        purrr::pluck(1)

    })
    return(vals)
  })
}

## To be copied in the UI
# mod_filters_twoway_ui("filters_twoway_1")

## To be copied in the server
# mod_filters_twoway_server("filters_twoway_1")
