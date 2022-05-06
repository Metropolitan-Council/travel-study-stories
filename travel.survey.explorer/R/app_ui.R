#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    tags$html(lang = "en"),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    navbarPage(
      windowTitle = "Travel Survey Explorer",
      # collapsible = TRUE,
      title = div(
        img(
          src = "www/main-logo.png",
          height = "60px",
          alt = "MetCouncil logo"
        )
      ),
      id = "nav",
      tabPanel(
        title = "One-Way Table",
        sidebarLayout(
          sidebarPanel = sidebarPanel(
            width = 3,
            mod_input_category_variable_ui("oneway_input_1"),
            # Filters go here
            checkboxInput(
              inputId = "oneway_input_mpo",
              label = "Restrict to Households in Twin Cities region (MPO)",
              value = FALSE
            ),
            selectInput(
              inputId = "oneway_input_year",
              "Survey Year",
              choices = c("2018-2019", "2020-2021")
            ),
            selectInput(
              inputId = "oneway_input_counties",
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
            ),
            selectInput(
              inputId = "oneway_input_cities",
              "Household City/Township",
              multiple = T,
              choices =
                c("Minneapolis", "St. Paul"),
              selected = ""
            ),
            actionButton("go_one_way", "Create Table")
          ),
          mainPanel = mainPanel(
            mod_plot_one_way_ui("plot_one_way_ui_1"),
            mod_table_one_way_ui("table_one_way_ui_1")
          )
        )
      ),
      tabPanel(
        title = "Two-Way Table",
        wellPanel(
          h3("Filter data"),
          # Filters go here
          checkboxInput(
            inputId = "twoway_input_mpo",
            label = "Filter to households in Twin Cities region (MPO)",
            value = FALSE
          ),
          selectInput(
            inputId = "twoway_input_year",
            "Survey Year",
            choices = c("2018-2019", "2020-2021"),
            multiple = F,
            selected = "2018-2019"
          ),
          selectInput(
            inputId = "twoway_input_counties",
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
          ),
          selectInput(
            inputId = "twoway_input_cities",
            "Household City/Township",
            multiple = T,
            choices =
              c("Minneapolis", "St. Paul"),
            selected = ""
          )

          )
        ),
        wellPanel(
          h3("First variable"),
          mod_input_category_variable_ui("2w_input_2")
        ),
        wellPanel(
          h3("Second variable"),
          mod_input_category_variable_ui("2w_input_1")
        ),
        wellPanel(# go_two_way_button
          actionButton("go_two_way", "Create Crosstab")),
        mod_table_two_way_ui("table_two_way_ui_1")
      ),
      tabPanel(title = "About",
               mod_about_ui("about_ui_1"))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))

  tags$head(
    waiter::useWaiter(),
    favicon(),
    bundle_resources(# shiny::includeHTML(paste0(app_sys("app/www"), "/google-analytics.html")),
      path = app_sys("app/www"),
      app_title = "Travel Survey Explorer")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
