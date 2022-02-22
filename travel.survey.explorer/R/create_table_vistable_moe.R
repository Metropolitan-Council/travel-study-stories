#' Title
#'
#' @param valuetable value table
#' @param moetable Margin of error table
#' @param xalias x var alias
#' @param xvalues x var values
#'
#' @return a [data.table::data.table()]
#' @export
#'
#' @importFrom data.table melt.data.table setnames
create_table_vistable_moe <- function(valuetable,
                                      moetable,
                                      xalias,
                                      xvalues) {
  msrcols <- colnames(valuetable)[!(colnames(valuetable) %in% xalias)]

  dts <- data.table::melt.data.table(valuetable,
                                     id.vars = xalias,
                                     measure.vars = msrcols,
                                     variable.name = "value",
                                     value.name = "result")

  # note use of reactive xtabTableType()

  if (xtabTableType()$Type == "dimension") {
    dtm <- data.table::melt.data.table(moetable, id.vars = xalias,
                                       measure.vars = msrcols,
                                       variable.name = "value",
                                       value.name = "result_moe")

    dt <- data.table::merge.data.table(dts, dtm, by = c(xalias, "value"))
    data.table::setnames(dt, xalias, "group")
  } else {
    dt <- data.table::merge.data.table(dts, moetable, by = c(xalias))
    data.table::setnames(dt, c(xalias, "MOE"), c("group", "result_moe"))
  }

  if (nrow(xvalues) != 0) {
    dt[
      , group := factor(group, levels = xvalues$value_text)][
        , group := fct_explicit_na(group, "No Response")]
    dt <- dt[order(group)]
  } else {
    dt[, group := factor(group)]
  }
  return(dt)
}
