#' plot_two_way UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_two_way_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("plot"))
  )
}

#' @title plot_two_way Server Functions
#'
#' @noRd
mod_plot_two_way_server <- function(id, two_way_plot_inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    w_two_way <- waiter::Waiter$new(ns("plot"),
                            color = waiter::transparent(0.5))

    output$plot <- plotly::renderPlotly({
      # browser()
      w_two_way$show()
      table_data <- two_way_plot_inputs$table_return

      council_plot_ly() %>%
        plotly::add_trace(
          type = "bar",
          x = table_data[[2]],
          y = table_data$estimated_prop,
          color = table_data[[1]],
          hovertemplate = paste0(
            "<b>",
            two_way_plot_inputs$context_row_return$variable_label,
            "</b>: %{fullData.name} <br>",
            "%{x}: %{y:.0%}"
          )
        ) %>%
        plotly::layout(
          barmode = "group",
          # title = "two-way-title",
          hoverlabel = list(
            font = plotly_layout$hover_text
          ),
          xaxis = list(
            title = list(
              text = "",
              # standoff = 25,
              font = plotly_layout$axis_titlefont
            ),
            tickfont = plotly_layout$tickfont
          ),
          yaxis = list(
            title = list(
              text = "Proportion",
              # standoff = 25,
              font = plotly_layout$axis_titlefont
            ),
            tickfont = plotly_layout$tickfont,
            rangemode = "nonnegative",
            tickformat = ".0%"
          ),
          legend = list(
            title = list(
              text = two_way_plot_inputs$context_row_return$variable_label,
              font = plotly_layout$axis_titlefont
            ),
            font = plotly_layout$tickfont
          )
        )
    })
  })
}

## To be copied in the UI
# mod_plot_two_way_ui("plot_two_way_ui_1")

## To be copied in the server
# mod_plot_two_way_server("plot_two_way_ui_1")
