#' @title Create a one-way cross table
#'
#' @param variable_row character, variable name. Must be one of `tbi_dict$variable`.
#' @param hh_ids vector, integer64 vector of unique household IDs. Must be
#'     one or more of `tbi_tables$hh$hhid`
#'
#' @return A named list of tables
#'     - `table`, table with columns
#'        - variable  name, matching `variable_row`
#'        - `group_N` raw sample size - number of people, trips, households, days (by group)
#'        - `group_N_hh` number of households in sample (by group)
#'        - `expanded_total` and `expanded_total_se`
#'        - `estimated_prop` and `estimated_prop_se` estimated proportion (0.0 - 1.0) and SE;
#'            multiply by 100 for percentages.
#'     - `definitions` table with contextual information from `tbi_dict`
#'     - `summary`, table
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(travel.survey.explorer)
#' create_one_way_table("bike_freq")
#' }
#' @importFrom rlang sym quo_name enquo .data
#' @importFrom dplyr filter select mutate rename group_by summarize ungroup summarize_all
#' @importFrom magrittr extract2
#' @importFrom srvyr survey_total survey_prop
#' @importFrom purrr pluck
#' @importFrom data.table as.ITime
#' @import bit64
create_one_way_table <- function(variable_row, hh_ids) {
  this_table <-
    tbi_dict %>%
    dplyr::filter(.data$variable == variable_row) %>%
    dplyr::select(.data$which_table) %>%
    unique() %>%
    magrittr::extract2(1)

  this_weight <-
    tbi_dict %>%
    dplyr::filter(.data$variable == variable_row) %>%
    dplyr::select(.data$wt_field) %>%
    unique() %>%
    magrittr::extract2(1)

  vartype <-
    tbi_tables[[this_table]] %>%
    dplyr::select(rlang::sym(variable_row)) %>%
    dplyr::summarize_all(class) %>%
    purrr::pluck(1)

  tab <- tbi_tables[[this_table]] %>%
    dplyr::filter(!(get(variable_row) %in% missing_codes)) %>%
    # get rid of NA weights:
    dplyr::filter(!is.na(get(this_weight)))

  # numeric data -----
  if (vartype == "numeric") {


    # table of median and means for numeric data:
    summary <-
      tab %>%
      # get rid of "Inf" values (for mpg_city, mpg_highway) :
      dplyr::filter(!get(variable_row) == Inf) %>%
      # get our households:
      dplyr::filter(.data$hh_id %in% hh_ids) %>%
      srvyr::as_survey_design(weights = !!this_weight) %>%
      dplyr::summarize(
        mean = srvyr::survey_mean(get(variable_row)),
        median = srvyr::survey_median(get(variable_row))
      ) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), round, digits = 5))


    # cut into bins:
    brks <- histogram_breaks[[variable_row]]$breaks
    brks_labs <- histogram_breaks[[variable_row]]$labels


    tab <- tab %>%
      # get our households:
      dplyr::filter(.data$hh_id %in% hh_ids) %>%
      dplyr::mutate(cuts = cut(
        get(variable_row),
        breaks = brks,
        labels =  brks_labs,
        order_result = TRUE
      )) %>%
      dplyr::select(-rlang::sym(variable_row)) %>%
      dplyr::rename(!!rlang::enquo(variable_row) := .data$cuts)

    # time data -----
  } else if (vartype == "ITime") {
    summary <-
      tab %>%
      # get our households:
      dplyr::filter(.data$hh_id %in% hh_ids) %>%
      # get rid of "Inf" values (for mpg_city, mpg_highway) :
      dplyr::filter(!get(variable_row) == Inf) %>%
      srvyr::as_survey_design(weights = !!this_weight) %>%
      dplyr::summarize(
        mean = srvyr::survey_mean(get(variable_row)),
        median = srvyr::survey_median(get(variable_row))
      ) %>%
      # round to nearest minute:
      dplyr::mutate(dplyr::across(dplyr::everything(), function(x) (x %/% 60L) * 60L)) %>%
      # make into a time obj:
      dplyr::mutate(dplyr::across(dplyr::everything(), data.table::as.ITime))


    brks <- histogram_breaks[[variable_row]]$breaks
    brks_labs <- histogram_breaks[[variable_row]]$labels


    tab <- tab %>%
      # get our households:
      dplyr::filter(hh_id %in% hh_ids) %>%
      dplyr::mutate(cuts = cut(
        get(variable_row),
        breaks = brks,
        labels =  brks_labs,
        order_result = TRUE,
        include.lowest = TRUE
      )) %>%
      dplyr::select(-rlang::sym(variable_row)) %>%
      dplyr::rename(!!rlang::enquo(variable_row) := .data$cuts)

    # factor data -----
  } else {

    # empty table of median and means for numeric data:
    # TODO - add column for mode?
    summary <-
      data.frame(
        mean = NA,
        mean_se = NA,
        median = NA,
        median_se = NA
      )
  }

  table <- tab %>%
    # get our households:
    dplyr::filter(.data$hh_id %in% hh_ids) %>%
    # clean up:
    droplevels() %>%
    # big N sample size - for the whole data frame:
    dplyr::mutate(
      total_N = length(.data$hh_id), # raw sample size - number of people, trips, households, days
      total_N_hh = length(unique(.data$hh_id))
    ) %>% # total number of households in sample
    srvyr::as_survey_design(weights = !!this_weight) %>%
    dplyr::group_by( # grouping by number of samples, number of households to keep this info
      .data$total_N,
      .data$total_N_hh,
      get(variable_row)
    ) %>%
    dplyr::summarize(
      group_N = length(.data$hh_id),
      # raw sample size - number of people, trips, households, days (by group)
      group_N_hh = length(unique(.data$hh_id)),
      # number of households in sample (by group)
      expanded_total = srvyr::survey_total(),
      # expanded total and SE
      estimated_prop = srvyr::survey_prop() # estimated proportion (0.0 - 1.0) and SE; multiply by 100 for percentages.
    ) %>%
    # rename the column back to the original name - it gets weird for some reason
    dplyr::rename(!!rlang::quo_name(variable_row) := `get(variable_row)`) %>%
    dplyr::select(
      tidyselect::all_of(variable_row),
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
    dplyr::mutate(
      dplyr::across(where(is.numeric), round, digits = 5)
    ) %>%
    dplyr::mutate(units = !!this_table) %>%
    dplyr::mutate(units = dplyr::case_when(
      .data$units == "per" ~ "people",
      .data$units == "day" ~ "days",
      .data$units == "hh" ~ "households",
      .data$units == "veh" ~ "vehicles",
      .data$units == "trip" ~ "trips"
    ))

  # Dictionary -------------
  definitions <-
    tbi_dict %>%
    dplyr::filter(.data$variable == variable_row) %>%
    dplyr::select(
      .data$variable_label, .data$survey_question,
      .data$variable_logic, .data$which_table, .data$category
    ) %>%
    unique()

  one_way_rt_list <- list(
    table = table,
    definitions = definitions,
    summary = summary
  )

  return(one_way_rt_list)
}
