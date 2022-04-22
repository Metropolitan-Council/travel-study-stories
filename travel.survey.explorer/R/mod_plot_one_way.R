#' plot_one_way UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_one_way_ui <- function(id){
  ns <- NS(id)
  tagList(

    plotly::plotlyOutput(ns("plot"))

  )
}

#' plot_one_way Server Functions
#'
#' @noRd
mod_plot_one_way_server <- function(id, table_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$plot <- plotly::renderPlotly({
     council_plot_ly() %>%
        plotly::add_trace(type = "bar",
                          x = table_data[[1]],
                          y = table_data$estimated_prop) %>%
        plotly::layout(
          # title = "one-way-title",
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

  })
}

## To be copied in the UI
# mod_plot_one_way_ui("plot_one_way_ui_1")

## To be copied in the server
# mod_plot_one_way_server("plot_one_way_ui_1")
