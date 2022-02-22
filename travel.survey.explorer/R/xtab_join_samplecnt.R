#' Join crosstab with relevant sample count
#'
#' @param xtabcleandt crosstab, clean
#' @param dttype data table type
#' @param varsXAlias X variable alias
#'
#' @family shiny processing
#'
#' @return a [data.table::data.table()]
#' @export
#'
xtab_join_samplecnt <- function(xtabcleandt, dttype, varsXAlias) {
  dt.data <- xtabcleandt[[dttype]]
  dt.sort.rows <- dt.data[[varsXAlias]]
  dt.style <- copy(xtabcleandt[["sample_count"]])

  # alter dt.style, update rows containing < 30
  # dt.style <- xtab.edit.samplecnt(dt.style, 30)

  colnames(dt.style)[2:length(colnames(dt.style))] <- paste0(
    colnames(dt.style)[2:length(colnames(dt.style))], "_sc")

  dt <- merge(dt.data, dt.style, by = varsXAlias)

  dt[, var1.sort := factor(get(varsXAlias), levels = dt.sort.rows)]

  dt <- dt[order(var1.sort)][, var1.sort := NULL]
}
