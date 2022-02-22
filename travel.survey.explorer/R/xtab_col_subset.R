#' Subset cross tab columns
#'
#' @param table table
#' @param colstring defautl is col.headers
#'
#' @family shiny processing
#'
#' @return a [data.table::data.table()]
#' @export
#'
#' @importFrom stringr str_subset
xtab_col_subset <- function(table,
                            colstring = col.headers) {
  cols <- c(varsXAlias(), # var x alias
            stringr::str_subset(colnames(table),
                       paste0("^", colstring)))
  table[, ..cols]
}
