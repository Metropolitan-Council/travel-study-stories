#'  Dimension related tables
#'
#' @param valuetable value table
#' @param moetable margin of error table
#' @param xalias x alias
#' @param xvalues x values
#'
#' @return a [data.table::data.table()]
#' @export
#'
create_table_joining_moe <- function(valuetable, moetable, xalias, xvalues) {
  # This is function is for Dimension related tables

  dtcols <- colnames(valuetable)[2:ncol(valuetable)]
  cols.order <- c()
  for (acol in dtcols) {
    moe.col <- paste0(acol, "_MOE")
    cols.order <- append(cols.order, c(acol, moe.col))
  }
  colnames(moetable)[2:ncol(moetable)] <- paste0(colnames(moetable)[2:ncol(moetable)], "_MOE")

  dt.sm <- merge(valuetable, moetable, by = xalias)

  dt.sm[, var1.sort := factor(get(eval(xalias)), levels = xvalues$value_text)]

  dt.sm <- dt.sm[order(var1.sort)][
    , var1.sort := NULL]

  order.colnames <- c(xalias, cols.order)
  dt.sm <- dt.sm[, ..order.colnames]
}
