#' @title Plot_ly object, with custom styling
#'
#' @param ... Arguments passed to [plotly::plot_ly()]
#'
#' @return [plotly::plot_ly()] object
#' @export
#'
#' @examples
#' \dontrun{
#' council_plot_ly()
#' }
#' @importFrom plotly plot_ly config
council_plot_ly <- function(...) {
  plotly::plot_ly(...) %>%
    plotly::config(
      displayModeBar = "hover",
      displaylogo = FALSE,
      showSendToCloud = FALSE,
      showEditInChartStudio = FALSE,
      modeBarButtonsToRemove = list(
        "lasso2d",
        "zoomIn2d",
        "zoomOut2d"
      )
    )
}
