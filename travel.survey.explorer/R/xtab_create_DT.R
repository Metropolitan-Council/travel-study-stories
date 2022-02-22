#' Create DT
#'
#' @param atable a table
#' @param moe logical, included margin of error
#' @param acontainer container
#' @param indices2hide columns to hide
#' @param maxyvals numeric, maximum number of columns
#' @param sc.cols numeric, columns with actual data
#'
#' @return a [DT::datatable()] object
#' @export
#'
#' @importFrom DT datatable formatStyle
xtab_create_DT <- function(atable,
                           moe = c(TRUE, FALSE),
                           acontainer,
                           indices2hide,
                           maxyvals,
                           sc.cols) {

  colors <- list(ltgrey = "#bdbdc3", dkgrey = "#343439")

  if (moe == TRUE) {
    defs <- list(
      list(className = "dt-head-center dt-center",
           targets = "_all"), # DT CRAN hack
      list(visible = F, targets = indices2hide)
    ) # DT's column index starts at 0 not 1
  } else {
    defs <- list(
      list(visible = F,
           targets = indices2hide)) # DT's column index starts at 0 not 1
  }

  DT::datatable(atable,
                # caption = acaption,
                container = acontainer,
                rownames = FALSE,
                options = list(
                  bFilter = 0,
                  columnDefs = defs
                ) # DT's column index starts at 0 not 1
  ) %>%
    DT::formatStyle(
      columns = 2:maxyvals,
      valueColumns = sc.cols,
      color = styleInterval(c(30),
                            c(colors$ltgrey, colors$dkgrey))
    )
}
