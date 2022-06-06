#' plot_one_way UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_one_way_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4(shiny::textOutput(ns("variable_label"))),
    h6(shiny::textOutput(ns("survey_question"))),
    shiny::textOutput(ns("filter_and_n")),
    plotly::plotlyOutput(ns("plot"))
  )
}

#' @title plot_one_way Server Functions
#'
#' @noRd
mod_plot_one_way_server <- function(id, one_way_plot_inputs, filter_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    w <- waiter::Waiter$new(ns("plot"), color = waiter::transparent(0.5))

    output$variable_label <- shiny::renderText({
      definitions_data <- one_way_plot_inputs$definitions_return
      definitions_data$variable_label
    })

    output$survey_question <- shiny::renderText({
      definitions_data <- one_way_plot_inputs$definitions_return
      definitions_data$survey_question
    })

    output$filter_and_n <- shiny::renderText({
      table_data <- one_way_plot_inputs$table_return
      n <- table_data$total_N[[1]]
      n_hh <- table_data$total_N_hh[[1]]
      units <- table_data$units[[1]]

      area_filter <- filter_info$filter_text

      if (units == "households") {
        paste0("Showing data for ", format(n_hh, big.mark = ","),
               " households",
               ifelse(area_filter == "", ".",
                      paste0(" from ", area_filter, ".")))
      } else {
        paste0("Showing data for ", format(n, big.mark = ","), " ", units, " from ", format(n_hh, big.mark = ","), " households", ifelse(area_filter == "", ".", paste0(" from ", area_filter, ".")))
      }
    })


    output$plot <- plotly::renderPlotly({
      # browser()
      w$show()
      table_data <- one_way_plot_inputs$table_return

      council_plot_ly() %>%
        plotly::add_trace(
          type = "bar",
          x = table_data[[1]],
          y = table_data$estimated_prop,
          hovertemplate = paste0(
            "<b>", one_way_plot_inputs$context_data$variable_label, "</b><br>",
            "%{x}: %{y:.0%}"
          )
        ) %>%
        plotly::layout(
          # title = "one-way-title",
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
          )
        )
    })
    # bindEvent(table_data)
  })
}

## To be copied in the UI
# mod_plot_one_way_ui("plot_one_way_ui_1")

## To be copied in the server
# mod_plot_one_way_server("plot_one_way_ui_1")
