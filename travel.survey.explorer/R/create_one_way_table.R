#' @title Create a one-way cross table
#'
#' @param variable character, variable name. Must be one of `tbi_dict$variable`.
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
#' @importFrom rlang sym quo_name enquo
#' @importFrom dplyr filter select mutate rename group_by summarize ungroup summarize_all
#' @importFrom magrittr extract2
#' @importFrom srvyr survey_total survey_prop
#' @importFrom purrr pluck
#' @importFrom data.table as.ITime
#'
create_one_way_table <- function(variable_row) {

  this_table <-
    tbi_dict %>%
    dplyr::filter(variable == variable_row) %>%
    dplyr::select(which_table) %>%
    unique() %>%
    magrittr::extract2(1)

  this_weight <-
    tbi_dict %>%
    dplyr::filter(variable == variable_row) %>%
    dplyr::select(wt_field) %>%
    unique() %>%
    magrittr::extract2(1)

  vartype <-
    tbi_tables[[this_table]] %>%
    dplyr::select(rlang::sym(variable_row)) %>%
    dplyr::summarize_all(class) %>%
    purrr::pluck(1)

  if (vartype == "numeric") {
    tab <- tbi_tables[[this_table]] %>%
      dplyr::filter(!(get(variable_row) %in% missing_codes))

    # table of median and means for numeric data:
    tab_mean <-
      tab %>%
      # get rid of "Inf" values (for mpg_city, mpg_highway) :
      filter(!get(variable_row) == Inf) %>%
      srvyr::as_survey_design(weights = !!this_weight) %>%
      dplyr::summarize(mean = srvyr::survey_mean(get(variable_row)),
                       median = srvyr::survey_median(get(variable_row))) %>%
      dplyr::mutate(dplyr::across(tidyselect:::where(is.numeric), round, digits = 5))


    # cut into bins:
    brks <- histogram_breaks[[variable_row]]$breaks
    brks_labs <- histogram_breaks[[variable_row]]$labels


    tab <- tab %>%
      dplyr::mutate(cuts = cut(
        get(variable_row),
        breaks = brks,
        labels =  brks_labs,
        order_result = TRUE
      )) %>%
      dplyr::select(-rlang::sym(variable_row)) %>%
      dplyr::rename(!!rlang::enquo(variable_row) := cuts)


  } if (vartype == "ITime") {
    tab <- tbi_tables[[this_table]] %>%
      dplyr::filter(!(get(variable_row) %in% missing_codes))

    tab_mean <-
      tab %>%
      # get rid of "Inf" values (for mpg_city, mpg_highway) :
      filter(!get(variable_row) == Inf) %>%
      srvyr::as_survey_design(weights = !!this_weight) %>%
      dplyr::summarize(mean = srvyr::survey_mean(get(variable_row)),
                       median = srvyr::survey_median(get(variable_row))) %>%
      # round to nearest minute:
      dplyr::mutate(across(everything(), function(x) (x %/% 60L) * 60L)) %>%
      # make into a time obj:
      dplyr::mutate(across(everything(), data.table::as.ITime))


    brks <- histogram_breaks[[variable_row]]$breaks
    brks_labs <- histogram_breaks[[variable_row]]$labels


    tab <- tab %>%
      dplyr::mutate(cuts = cut(
        get(variable_row),
        breaks = brks,
        labels =  brks_labs,
        order_result = TRUE,
        include.lowest = TRUE
      )) %>%
      dplyr::select(-rlang::sym(variable_row)) %>%
      dplyr::rename(!!rlang::enquo(variable_row) := cuts)


  } else {
    tab <- tbi_tables[[this_table]] %>%
      dplyr::filter(!(get(variable_row) %in% missing_codes))

    # empty table of median and means for numeric data:
    tab_mean <-
      data.frame(
        mean = NA,
        mean_se = NA,
        median = NA,
        median_se = NA
      )
  }

  rt_tab <- tab %>%
    # clean up:
    droplevels() %>%
    # big N sample size - for the whole data frame:
    dplyr::mutate(total_N = length(hh_id), # raw sample size - number of people, trips, households, days
                  total_N_hh = length(unique(hh_id))) %>% # total number of households in sample
    srvyr::as_survey_design(weights = !!this_weight) %>%
    dplyr::group_by(# grouping by number of samples, number of households to keep this info
      total_N, total_N_hh,
      get(variable_row)) %>%
    dplyr::summarize(
      group_N = length(hh_id),
      # raw sample size - number of people, trips, households, days (by group)
      group_N_hh = length(unique(hh_id)),
      # number of households in sample (by group)
      expanded_total = srvyr::survey_total(),
      # expanded total and SE
      estimated_prop = srvyr::survey_prop() # estimated proportion (0.0 - 1.0) and SE; multiply by 100 for percentages.
    ) %>%
    # rename the column back to the original name - it gets weird for some reason
    dplyr::rename(!!rlang::quo_name(variable_row) := `get(variable_row)`) %>%
    dplyr::select(
      all_of(variable_row),
      "total_N",
      "total_N_hh",
      "group_N",
      "group_N_hh",
      "expanded_total",
      "expanded_total_se",
      "estimated_prop",
      "estimated_prop_se"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(tidyselect:::where(is.numeric), round, digits = 5))

  return(rt_tab)
  return(tab_mean)
}
