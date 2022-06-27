#' Create HTML container for Two-way table
#'
#' @param twoway_data list, return from `create_twoway_table()`
#' @param type character, one of `c("proportion", "proportion_w_se","total", "total_w_se", "sample")`
#'
#' @return named list, of two items.
#'     - `dt_data` table data
#'     - `container` HTML DT container
#' @export
#'
create_datatable_container <- function(twoway_data,
                                       type = c(
                                         "proportion",
                                         "proportion_w_se",
                                         "total",
                                         "total_w_se",
                                         "sample"
                                       )) {
  # browser()
  these_columns <-
    dplyr::case_when(
      type == "proportion" ~ c("proportion" = "estimated_prop"),
      type == "proportion_w_se" ~ c(
        "proportion" = "estimated_prop",
        "se" = "estimated_prop_se",
        "sample" = "group_N"
      ),
      type == "total" ~ c("total" = "expanded_total"),
      # type == "total_w_se" ~ c(
      #   "total" = "expanded_total",
      #   "se" = "expanded_total_se"),
      type == "sample" ~ c("sample" = "group_N")
    )

  this_table <- twoway_data$table %>%
    dplyr::mutate(estimated_prop_se = scales::percent(.data$estimated_prop_se,
                                                      accuracy = 0.1
    )) %>%
    dplyr::select(
      row_var = 1,
      col_var = 2,
      tidyselect::all_of(these_columns)
    ) %>%
    tidyr::pivot_wider(
      names_from = col_var,
      values_from = these_columns,
      names_vary = "slowest",
      names_glue = "{col_var} ({.value})"
    )


  col_label <- twoway_data$definition_col$variable_label
  row_label <- twoway_data$definition_row$variable_label


  super_col_headers <- twoway_data$table[2] %>%
    unique() %>%
    extract2(1)


  sub_col_headers <-
    dplyr::case_when(
      type == "proportion" ~ c("Proportion"),
      type == "proportion_w_se" ~ c("Proportion", "Standard Error", "Sample size"),
      type == "total" ~ c("Total"),
      # type == "total_w_se" ~ c("Total", "Standard Error"),
      type == "sample" ~ c("Sample size")
    )


  sketch <- htmltools::withTags(
    table(
      class = "display",
      thead(
        tr(
          th(
            class = "dt-center",
            colspan = ncol(this_table), col_label
          )
        ),
        tr(
          th(class = "dt-center", rowspan = 2, row_label),
          lapply(super_col_headers,
            th,
            colspan = length(these_columns)
          ),
        ),
        tr(
          lapply(
            rep(
              sub_col_headers,
              length(super_col_headers)
            ),
            function(x) {
              th(
                class = "dt-center",
                style = "font-size:14px", x
              )
            }
          )
        )
      )
    )
  )


  return(
    list(
      dt_data = this_table,
      container = sketch
    )
  )
}
