#' Style DT with MOE
#'
#' @param atable a table
#' @param xvaralias x var alias
#' @param yvaralias y var alias
#' @param tbltype character, one of `"share"`, `"estimate"`, or `"mean"`
#'
#' @family DT styling
#'
#' @return HTML styling
#' @export
#' @importFrom stringr str_subset
#' @importFrom htmltools withTags
dt_container_tblMOE_dtstyle <- function(atable,
                                        xvaralias,
                                        yvaralias,
                                        tbltype = c("share", "estimate", "mean")) {
  # ifelse(tbltype == "share", tbltype <- "Share", tbltype <- "Total")
  if (tbltype == "share") {
    tbltype <- "Share"
  } else if (tbltype == "estimate") {
    tbltype <- "Total"
  } else {
    tbltype <- "Mean"
  }

  exc.cols <- stringr::str_subset(colnames(atable),
                                  paste(xvaralias, "_MOE|_sc.*", sep = "|"))
  yval.labels <- setdiff(colnames(atable), exc.cols)

  sc.cols <- stringr::str_subset(colnames(atable), "_sc.*")
  num.disp.cols <- ncol(atable) - length(sc.cols)

  if (tbltype == "Share" | tbltype == "Total") {
    htmltools::withTags(
      table(
        class = "display",
        tags$thead(
          tags::tr(
            tags$th(class = "dt-center", rowspan = 3, xvaralias),
            tags$th(class = "dt-center", colspan = (num.disp.cols - 1), yvaralias)
          ),
          tags$tr(
            lapply(yval.labels, function(x){ tags$th(class = "dt-center", colspan = 2, x)})
          ),
          tags$tr(
            lapply(rep(c(tbltype, "Margin of Error"),
                       (num.disp.cols - 1) / 2),
                   function(x){tags$th(style = "font-size:12px", x)})
          )
        )
      )
    )
  } else {
    htmltools::withTags(
      table(
        class = "display",
        tags$thead(
          tags$tr(
            'th'(class = "dt-center", rowspan = 2, xvaralias),
            tags$th(class = "dt-center", colspan = (num.disp.cols - 1), yvaralias)
          ),
          # tr(
          #   lapply(yval.labels, function(x) th(class = 'dt-center', colspan = 2, x))
          # ),
          tags$tr(
            lapply(rep(c(tbltype, "Margin of Error"), (num.disp.cols - 1) / 2),
                   function(x) {tags$th(style = "font-size:12px", x)})
          )
        )
      )
    )
  }
}
