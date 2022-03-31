#' @title Create a two-way cross table
#'
#' @param variable character, variable name.
#' @param type character, one of `"numeric"` or `"category"`.
#'     Default is `"numeric"`
#'
#' @return A tibble with columns
#'    - variable name with relevant values
#'    - `group_N` raw sample size - number of people, trips, households, days (by group)
#'    - `group_N_hh` number of households in sample (by group)
#'    - `expanded_total` and `expanded_total_se`
#'    - `estimated_prop` and `estimated_prop_se` estimated proportion (0.0 - 1.0) and SE;
#'         multiply by 100 for percentages.
#' @export
#'
#' @examples
#' /dontrun{
#'
#' library(travel.survey.explorer)
#' create_one_way_table("bike_freq")
#'
#' }
#' @importFrom rlang sym quo_name
#' @importFrom dplyr filter select mutate rename group_by summarize ungroup
#' @importFrom magrittr extract2
#' @importFrom srvyr survey_total survey_prop
#'
# create_cross_tab_with_weights
create_two_way_table <- function(variable_row, variable_col){
  # z <- 1.96 # 95% CI

  print("reading in data")

  # example: get number of trips, by category, for each age group
  # rows = age, columns = purpose
  variable_row <- "age"
  variable_col <- "d_purpose_category_imputed"


  this_table_row <-
    tbi_dict %>%
    dplyr::filter(variable == variable_row) %>%
    dplyr::select(which_table) %>%
    unique() %>%
    magrittr::extract2(1)

  this_table_col <-
    tbi_dict %>%
    dplyr::filter(variable == variable_col) %>%
    dplyr::select(which_table) %>%
    unique() %>%
    magrittr::extract2(1)

  # weights are for the columns, so in example, trip weights
  this_weight <-
    tbi_dict %>%
    dplyr::filter(variable == variable_col) %>%
    dplyr::select(wt_field) %>%
    unique() %>%
    magrittr::extract2(1)

  vartype_row <-
    tbi_tables[[this_table_row]] %>%
    select(sym(variable_row)) %>%
    summarize_all(class) %>%
    purrr::pluck(1)

  vartype_col <-
    tbi_tables[[this_table_col]] %>%
    select(sym(variable_col)) %>%
    summarize_all(class) %>%
    purrr::pluck(1)

  if(vartype == "numeric"){

    tab <- tbi_tables[[this_table]] %>%
      dplyr::filter(!(get(this_variable) %in% missing_codes))

    if(this_variable == "weighted_trip_count"){

      brks <- histogram_breaks$trip_breaks

    } else {
      tab <- tab %>%
        dplyr::filter(get(this_variable) > 0,
                      get(this_variable) < 200)

      brks <- histogram_breaks$other_breaks
    }


    tab2 <- tab %>%
      dplyr::mutate(cuts = cut(get(this_variable),
                               breaks = brks,
                               labels = names(brks),
                               order_result = TRUE
      ))


    # do a thing for numeric variables -- binning???
    # get a survey_mean? survey_median?
  } else {
    rt_tab <- tbi_tables[[this_table_col]] %>%
             left_join(tbi_tables[[this_table_row]]) %>%
             dplyr::select(
               rlang::sym(variable_row),
                 rlang::sym(variable_col),
                 rlang::sym(this_weight),
                 hh_id
               ) %>%
             # dplyr::filter where the variable is missing (missing codes = "Missing: No Response", "Missing: skip logic").
             # this list of missing codes is created in data-raw/data_compile.R, line ~105 and
             # data-raw/missing_codes.R
             dplyr::filter(!(get(variable_row) %in% missing_codes) &
                              +                       !(get(variable_col) %in% missing_codes)) %>%
             # clean up:
             droplevels() %>%
      # big N sample size - for the whole data frame:
      dplyr::mutate(total_N = length(hh_id), # raw sample size - number of people, trips, households, days
                    total_N_hh = length(unique(hh_id))) %>% # total number of households in sample
      srvyr::as_survey_design(weights = !!this_weight) %>%
      dplyr::group_by(
        # grouping by number of samples, number of households to keep this info
        total_N, total_N_hh,
        get(variable_row), get(variable_col)) %>%
      dplyr::summarize(
        group_N = length(hh_id), # raw sample size - number of people, trips, households, days (by group)
        group_N_hh = length(unique(hh_id)), # number of households in sample (by group)
        expanded_total = srvyr::survey_total(), # expanded total and SE
        estimated_prop = srvyr::survey_prop() # estimated proportion (0.0 - 1.0) and SE; multiply by 100 for percentages.
      ) %>%
      # rename the column back to the original name - it gets weird for some reason
      dplyr::rename(!!rlang::quo_name(variable_row) := `get(variable_row)`,
                    !!rlang::quo_name(variable_col) := `get(variable_col)`) %>%
      dplyr::select(all_of(variable_row), all_of(variable_col), "total_N", "total_N_hh","group_N", "group_N_hh",
                    "expanded_total", "expanded_total_se", "estimated_prop", "estimated_prop_se") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(dplyr::across(tidyselect:::where(is.numeric), round, digits = 5))

    return(rt_tab)
  }

}
