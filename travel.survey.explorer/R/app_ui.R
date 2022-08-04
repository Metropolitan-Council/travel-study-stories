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
            h3("How this works"),
            p("Select a category and variable. The list of variables available to choose from will update when you select a category. The plot and table to the right will update when you select a new variable."),
            mod_input_category_variable_ui("oneway_input_1"),
            h4("Filter households by geography"),
            mod_filters_ui("filters_oneway_1")
          ),
          mainPanel = mainPanel(
            mod_plot_ui("plot_one_way_ui_1"),
            p("This table shows the estimated proportion of (households, people, weekdays or trips) in each category. Numbers in gray represent estimates with a sample size of less than 30 (households, people, weekdays, or trips)."),
            mod_table_one_way_ui("table_one_way_ui_1")
          )
        )
      ),
      tabPanel(
        title = "Two-Way Table",
        fluidRow(
          column(
            width = 3,
            h3("How this works"),
            p("First, select a demographic variable. Then, select another category and variable. Use the tabbed panel below to view the table and plot.")
          ),
          shiny::column(
            width = 3,
            h4("First variable (rows)"),
            mod_input_category_variable_ui("input_category_row_1",
              starting_category_choices = "Demographics",
              starting_variable_selected = "disability"
            )
          ),
          column(
            width = 3,
            h4("Second variable (columns)"),
            mod_input_category_variable_ui(
              "input_category_col_1",
              starting_category_choices = unique(tbi_dict$category)[-1],
              starting_category_selected = "Commute",
              starting_variable_choices = input_list$Commute,
              starting_variable_selected = "work_mode"
            )
          ),
          column(
            width = 3,
            h4("Filter households by geography"),
            mod_filters_ui("filters_twoway_1")
          )
        ),
        tabsetPanel(
          tabPanel(
            title = "Table",
            mod_table_two_way_ui("table_two_way_ui_1")
          ),
          tabPanel(
            title = "Plot",
            mod_plot_ui("plot_two_way_ui_1")
          )
        )
      ),
      tabPanel(
        title = "About",
        mod_markdown_ui("START")
      )
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
    shiny::includeHTML("inst/app/www/google-analytics.html"),
    waiter::useWaiter(),
    favicon(),
    bundle_resources( # shiny::includeHTML(paste0(app_sys("app/www"), "/google-analytics.html")),
      path = app_sys("app/www"),
      app_title = "Travel Survey Explorer"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
