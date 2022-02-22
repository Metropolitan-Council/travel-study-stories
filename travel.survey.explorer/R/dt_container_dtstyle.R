#' Style DT
#'
#' @param atable a table
#' @param xvaralias character, x var alias
#' @param yvaralias character, y var alias
#'
#' @family DT styling
#'
#' @return HTML styling
#' @export
#'
#' @importFrom stringr str_subset
#' @importFrom htmltools withTags
dt_container_dtstyle <- function(atable, xvaralias, yvaralias) {

  sc.cols <- stringr::str_subset(colnames(atable), "_sc")

  num.disp.cols <- ncol(atable) - length(sc.cols)

  htmltools::withTags(
    table(
      class = "display",
      tags$thead(
        tags$tr(
          tags$th(class = "dt-center", rowspan = 2, xvaralias),
          tags$th(class = "dt-center", colspan = (num.disp.cols - 1),
                  yvaralias)
        ),
        tags$tr(
          lapply(colnames(atable)[2:num.disp.cols], tags$th)
        )
      )
    )
  )
}
