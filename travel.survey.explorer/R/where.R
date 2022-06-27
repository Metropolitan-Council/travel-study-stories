#' @title Select variables with a function
#' @description  This code is copied directly from https://github.com/r-lib/tidyselect/blob/HEAD/R/helpers-where.R
#'
#' This [selection helper][language] selects the variables for which a
#' function returns `TRUE`.
#'
#' @param fn A function that returns `TRUE` or `FALSE` (technically, a
#'   _predicate_ function). Can also be a purrr-like formula.
#'
#' @name where
where <- function(fn) {
  predicate <- as_function(fn)

  function(x, ...) {
    out <- predicate(x, ...)

    if (!is_bool(out)) {
      abort("`where()` must be used with functions that return `TRUE` or `FALSE`.")
    }

    out
  }
}
