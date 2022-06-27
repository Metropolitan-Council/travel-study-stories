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
#' @importFrom knitr combine_words
#' @importFrom stats na.omit
mod_filters_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # Dropdown for Survey Year ----
    # selectInput(
    #   inputId = ns("input_year"),
    #   "Survey year",
    #   choices = c("2018-2019", "2020-2021"),
    #   selected = "2018-2019",
    #   multiple = FALSE
    # ),

    wellPanel(
      # Checkbox for HHs in MPO ----
      checkboxInput(
        inputId = ns("input_mpo"),
        label = "Filter to households in Twin Cities region (MPO)",
        value = FALSE
      ),

      # Dropdown for HH County ----
      selectInput(
        inputId = ns("input_counties"),
        "Household county",
        choices = unique(na.omit(tbi_tables$hh$hh_county)),
        selected = NULL,
        multiple = TRUE
      ),

      # Dropdown for HH City -----
      selectInput(
        inputId = ns("input_cities"),
        "Household City/Township",
        choices = unique(na.omit(tbi_tables$hh$hh_city)),
        multiple = TRUE,
        selected = NULL
      )
    )
    # Button: Go One Way, Create Table ----
    # actionButton(inputId = ns("go_one_way"), "Create Table")
  )
}

#' filters_oneway Server Functions
#'
#' @noRd
#' @import bit64
mod_filters_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update Select Inputs----

    ## When "Restrict HHs to MPO region" is checked, ----
    # update counties/cities to include only those within the MPO boundary ----

    ## When county/counties selected, ----
    # update cities dropdown to include only cities within that county ----
    observeEvent(input$input_counties,
      {
        if (!is.null(input$input_counties)) {
          filtered_cities <- tbi_tables$hh %>%
            dplyr::filter(.data$hh_county %in% input$input_counties) %>%
            dplyr::select(.data$hh_city) %>%
            unique() %>%
            na.omit()

          updateSelectInput(
            session = session,
            inputId = "input_cities",
            label = "Household City/Township",
            choices = filtered_cities$hh_city,
            selected = NULL
          )
        } else {
          selectInput(
            inputId = ns("input_cities"),
            "Household City/Township",
            choices = unique(na.omit(tbi_tables$hh$hh_city)),
            selected = NULL
          )
        }
      },
      ignoreInit = TRUE
    )



    # Create HH ID list----

    ## Init reactive values -----
    vals <- reactiveValues()

    ## Init HH ID list----
    all_hh_ids <- tbi_tables$hh %>%
      dplyr::select(.data$hh_id)

    observe({
      # using "observe" rather than "observeEvent" - calculating as we go

      ## Filter to MPO----
      # only if the checkbox has been set to "TRUE"
      if (input$input_mpo == TRUE) {
        mpo_ids <-
          tbi_tables$hh %>%
          dplyr::filter(.data$hh_in_mpo == "Household in Twin Cities region") %>%
          dplyr::select(.data$hh_id)
      } else {
        mpo_ids <- all_hh_ids
      }

      ## Filter to County----
      # only if counties are not null
      if (!is.null(input$input_counties)) {
        cty_ids <-
          tbi_tables$hh %>%
          dplyr::filter(.data$hh_county %in% input$input_counties) %>%
          dplyr::select(.data$hh_id)
      } else {
        cty_ids <- all_hh_ids
      }

      ## Filter to City----
      if (!is.null(input$input_cities)) {
        ctu_ids <-
          tbi_tables$hh %>%
          dplyr::filter(.data$hh_city %in% input$input_cities) %>%
          dplyr::select(.data$hh_id)
      } else {
        ctu_ids <- all_hh_ids
      }

      ## Filter to Year----
      # year_ids <-
      #   tbi_tables$hh %>%
      #   dplyr::filter(survey == input$input_year) %>%
      #   dplyr::select(hh_id)

      ## Final HH ID list----
      # inner join is important: gets the intersecting set.
      vals$hh_ids <- all_hh_ids %>%
        #  dplyr::inner_join(year_ids, by = "hh_id") %>%
        dplyr::inner_join(ctu_ids, by = "hh_id") %>%
        dplyr::inner_join(cty_ids, by = "hh_id") %>%
        dplyr::inner_join(mpo_ids, by = "hh_id") %>%
        # you need to use "innerjoin" on a data frame,
        # but we want a list of hh ids in the end --
        # so we use pluck to get it
        purrr::pluck(1)

      ## Text string about households in the set ----
      citystring <-
        paste0(knitr::combine_words(
          gsub(
            "\\s*\\([^\\)]+\\)",
            "",
            as.character(input$input_cities)
          )
        ))

      countystring_1 <-
        paste0(
          knitr::combine_words(
            gsub(
              "\\s*\\([^\\)]+\\)|[[:space:]]MN|[[:space:]]WI",
              "",
              as.character(input$input_counties)
            )
          ),
          ifelse(length(input$input_counties) > 1, " Counties", " County")
        )

      countystring_withMPO <-
        ifelse(
          any(grepl("St. Croix|Sherburne|Wright", x = input$input_counties)) & input$input_mpo == T,
          paste0(countystring_1, ", restricted to areas within MPO boundary"),
          ifelse(
            any(grepl("St. Croix|Sherburne|Wright", x = input$input_counties)) & input$input_mpo == F,
            paste0(countystring_1, ", includes some areas outside MPO boundary"),
            countystring_1
          )
        )

      vals$filter_text <- ifelse( # if there is a city/cities, print that:
        length(input$input_cities) > 0,
        citystring,
        # OTHERWISE if there is a county/counties, print that:
        ifelse(
          length(input$input_counties) > 0,
          countystring_withMPO,
          # OTHERWISE, print the MPO boundary:
          ifelse(input$input_mpo == T, " the Twin Cities region (MPO Boundary)", "")
        )
      )



      print(vals$filter_text)
    })

    return(vals)
  })
}

## To be copied in the UI
# mod_filters_ui("filters_1")

## To be copied in the server
# mod_filters_server("filters_1")
