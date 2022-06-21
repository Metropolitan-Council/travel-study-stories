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
            p(shinipsum::random_text(nwords = 50)),
            mod_input_category_variable_ui("oneway_input_1"),
            mod_filters_ui("filters_oneway_1")
          ),
          mainPanel = mainPanel(
            mod_plot_ui("plot_one_way_ui_1"),
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
            p(shinipsum::random_text(nwords = 50))
          ),
          shiny::column(
            width = 3,
            h4("First variable (rows)"),
            mod_input_category_variable_ui("input_category_row_1",
              starting_category_choices = "Demographics",
              starting_variable_selected = "gender"
            )
          ),
          column(
            width = 3,
            h4("Second variable (columns)"),
            mod_input_category_variable_ui(
              "input_category_col_1",
              starting_category_choices = unique(tbi_dict$category)[-1],
              starting_category_selected = "Trips",
              starting_variable_choices = input_list$Trips,
              starting_variable_selected = "mode_type"
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
        mod_about_ui("about_ui_1")
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
