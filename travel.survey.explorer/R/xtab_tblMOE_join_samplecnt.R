#' Join crosstab, MOE, sample count data
#'
#' @param xtabcleantblMOEdt crosstab, clean, with margin of error
#' @param xtabcleandt crosstab clean
#' @param dttype crosstab data type
#' @param varsXAlias X variable alias
#'
#' @return a [data.table::data.table()]
#' @export
#'
#'
#' @importFrom data.table copy setcolorder
xtab_tblMOE_join_samplecnt <- function(xtabcleantblMOEdt,
                                       xtabcleandt, dttype, varsXAlias) {
  dt.data <- xtabcleantblMOEdt
  dt.style <- data.table::copy(xtabcleandt[["sample_count"]])
  dt.sort.rows <- dt.data[[varsXAlias]]

  idx <- 2:ncol(dt.style)
  colnames(dt.style)[idx] <- paste0(letters[1:(ncol(dt.style) - 1)], "_", colnames(dt.style)[idx], "_sc")
  new.cols <- paste0(colnames(dt.style)[idx], "2")
  cols <- colnames(dt.style)[idx]
  col2order <- sort(c(cols, new.cols))
  dt.style[, (new.cols) := mapply(function(x){replicate(1, .SD[[x]])}, cols, SIMPLIFY = F)]
  data.table::set_col_order(dt.style, c(varsXAlias, col2order))

  # alter dt.style, update rows containing < 30
  # dt.style <- xtab.edit.samplecnt(dt.style, 30)

  dt <- merge(dt.data, dt.style, by = varsXAlias)

  dt[, var1.sort := factor(get(varsXAlias), levels = dt.sort.rows)]
  dt <- dt[order(var1.sort)][, var1.sort := NULL]
}
