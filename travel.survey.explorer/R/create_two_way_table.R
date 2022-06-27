#' @title Create a two-way cross table
#'
#' @param variable_row character, variable name for the ROW cross.
#'      Must be one of `tbi_dict$variable`.
#' @param variable_col character, variable name for the COLUMN cross.
#'      Must be one of `tbi_dict$variable`.
#' @inheritParams create_one_way_table
#'
#' @return A named list of tables
#'     - `table`, table with columns
#'        - variable row name, `variable_row`
#'        - variable column name, matching `variable_col`
#'        - `group_N` raw sample size - number of people, trips, households, days (by group)
#'        - `group_N_hh` number of households in sample (by group)
#'        - `expanded_total` and `expanded_total_se`
#'        - `estimated_prop` and `estimated_prop_se` estimated proportion (0.0 - 1.0) and SE;
#'            multiply by 100 for percentages.
#'     - `definition_row` table with row variable contextual information from `tbi_dict`
#'     - `definition_col` table with column variable contextual information from `tbi_dict`
#'     - `summary` table with numeric summary information
#' @export
#'
#' @examples
#' \dontrun{
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
#'
create_two_way_table <- function(variable_row, variable_col, hh_ids){

  if(variable_row == variable_col){
    warning("Row and columns variables must be distinct values")
  }

  this_table_row <-
    tbi_dict %>%
    dplyr::filter(.data$variable == variable_row) %>%
    dplyr::select(.data$which_table) %>%
    unique() %>%
    magrittr::extract2(1)

  this_table_col <-
    tbi_dict %>%
    dplyr::filter(.data$variable == variable_col) %>%
    dplyr::select(.data$which_table) %>%
    unique() %>%
    magrittr::extract2(1)

  # weights are for the columns, so in example, trip weights
  this_weight <-
    tbi_dict %>%
    dplyr::filter(.data$variable == variable_col) %>%
    dplyr::select(.data$wt_field) %>%
    unique() %>%
    magrittr::extract2(1)

  vartype_row <-
    tbi_tables[[this_table_row]] %>%
    dplyr::select(rlang::sym(variable_row)) %>%
    dplyr::summarize_all(class) %>%
    purrr::pluck(1)

  vartype_col <-
    tbi_tables[[this_table_col]] %>%
    dplyr::select(rlang::sym(variable_col)) %>%
    dplyr::summarize_all(class) %>%
    purrr::pluck(1)

  table_row <- tbi_tables[[this_table_row]] %>%
    dplyr::select(dplyr::contains("_id"),
                  dplyr::contains("_num"),
                  dplyr::contains("weight"),
                  rlang::sym(variable_row)) %>%
    dplyr::filter(!(get(variable_row) %in% missing_codes))

  table_col <- tbi_tables[[this_table_col]] %>%
    dplyr::select(dplyr::contains("_id"),
                  dplyr::contains("_num"),
                  dplyr::contains("weight"),
                  rlang::sym(variable_col)) %>%
    dplyr::filter(!(get(variable_col) %in% missing_codes))

  tab_0 <- table_row %>%
    dplyr::inner_join(table_col) %>%
    # get our households:
    dplyr::filter(.data$hh_id %in% hh_ids) %>%
    # get rid of NA weights:
    dplyr::filter(!is.na(get(this_weight)))

  # Bin row variable ----------------------------
  if (vartype_row == "numeric") {
    # cut into bins:
    brks <- histogram_breaks[[variable_row]]$breaks
    brks_labs <- histogram_breaks[[variable_row]]$labels


    tab_1 <- tab_0 %>%
      dplyr::mutate(cuts = cut(
        get(variable_row),
        breaks = brks,
        labels =  brks_labs,
        order_result = TRUE
      )) %>%
      dplyr::select(-rlang::sym(variable_row)) %>%
      dplyr::rename(!!rlang::enquo(variable_row) := .data$cuts)


  } else if (vartype_row == "ITime") {

    brks <- histogram_breaks[[variable_row]]$breaks
    brks_labs <- histogram_breaks[[variable_row]]$labels

    tab_1 <- tab_0 %>%
      dplyr::mutate(cuts = cut(
        get(variable_row),
        breaks = brks,
        labels =  brks_labs,
        order_result = TRUE,
        include.lowest = TRUE
      )) %>%
      dplyr::select(-rlang::sym(variable_row)) %>%
      dplyr::rename(!!rlang::enquo(variable_row) := .data$cuts)


  } else {
    tab_1 <- tab_0
  }

  # Bin column variable ----------------------------
  if (vartype_col == "numeric") {
    # cut into bins:
    brks <- histogram_breaks[[variable_col]]$breaks
    brks_labs <- histogram_breaks[[variable_col]]$labels

    tab_2 <- tab_1 %>%
      dplyr::mutate(cuts = cut(
        get(variable_col),
        breaks = brks,
        labels =  brks_labs,
        order_result = TRUE
      )) %>%
      dplyr::select(-rlang::sym(variable_col)) %>%
      dplyr::rename(!!rlang::enquo(variable_col) := .data$cuts)

    # table of median and means for numeric data:
    summary <-
      tab_1 %>%
      # get rid of "Inf" values (for mpg_city, mpg_highway) :
      dplyr::filter(!get(variable_col) == Inf) %>%
      srvyr::as_survey_design(weights = !!this_weight) %>%
      dplyr::group_by(get(variable_row)) %>%
      dplyr::summarize(mean = srvyr::survey_mean(get(variable_col)),
                       median = srvyr::survey_median(get(variable_col))) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), round, digits = 5)) %>%
      dplyr::rename(!!rlang::quo_name(variable_row) := `get(variable_row)`)

  } else if (vartype_col == "ITime") {
    brks <- histogram_breaks[[variable_col]]$breaks
    brks_labs <- histogram_breaks[[variable_col]]$labels

    tab_2 <- tab_1 %>%
      dplyr::mutate(cuts = cut(
        get(variable_col),
        breaks = brks,
        labels =  brks_labs,
        order_result = TRUE,
        include.lowest = TRUE
      )) %>%
      dplyr::select(-rlang::sym(variable_col)) %>%
      dplyr::rename(!!rlang::enquo(variable_col) := .data$cuts)

    summary <-
      tab_1 %>%
      # get rid of "Inf" values (for mpg_city, mpg_highway) :
      dplyr::filter(!get(variable_col) == Inf) %>%
      srvyr::as_survey_design(weights = !!this_weight) %>%
      dplyr::group_by(get(variable_row)) %>%
      dplyr::summarize(mean = srvyr::survey_mean(get(variable_col)),
                       median = srvyr::survey_median(get(variable_col))) %>%
      # round to nearest minute:
      dplyr::mutate(across(where(is.numeric),
                           function(x) (x %/% 60L) * 60L)) %>%
      # make into a time obj:
      dplyr::mutate(across(where(is.numeric),
                           function(x) data.table::as.ITime(x))) %>%
      dplyr::rename(!!rlang::quo_name(variable_row) := `get(variable_row)`)

  } else {
    tab_2 <- tab_1

    summary <-
      data.frame(
        mean = NA,
        mean_se = NA,
        median = NA,
        median_se = NA
      )
  }

  # Final Crosstab ----------------------------
  table <- tab_2 %>%
    # clean up:
    droplevels() %>%
    # big N sample size - for the whole data frame:
    dplyr::mutate(total_N = length(hh_id), # raw sample size - number of people, trips, households, days
                  total_N_hh = length(unique(hh_id))) %>% # total number of households in sample
    srvyr::as_survey_design(weights = !!this_weight) %>%
    dplyr::group_by(# grouping by number of samples, number of households to keep this info
      .data$total_N,
      .data$total_N_hh,
      get(variable_row),
      get(variable_col)) %>%
    dplyr::summarize(
      group_N = length(.data$hh_id),
      # raw sample size - number of people, trips, households, days (by group)
      group_N_hh = length(unique(.data$hh_id)),
      # number of households in sample (by group)
      expanded_total = srvyr::survey_total(),
      # expanded total and SE
      estimated_prop = srvyr::survey_prop() # estimated proportion (0.0 - 1.0) and SE; multiply by 100 for percentages.
    ) %>%
    ungroup() %>%
    # rename the column back to the original name - it gets weird for some reason
    dplyr::rename(!!rlang::quo_name(variable_row) := `get(variable_row)`) %>%
    dplyr::rename(!!rlang::quo_name(variable_col) := `get(variable_col)`) %>%
    dplyr::select(
      tidyselect::all_of(variable_row),
      tidyselect::all_of(variable_col),
      "total_N",
      "total_N_hh",
      "group_N",
      "group_N_hh",
      "expanded_total",
      "expanded_total_se",
      "estimated_prop",
      "estimated_prop_se"
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric),
                                round, digits = 5)) %>%
    dplyr::mutate(units = !!this_table_row) %>%
    dplyr::mutate(units = dplyr::case_when(.data$units == "per" ~ "people",
                                           .data$units == "day" ~ "days",
                                           .data$units == "hh" ~ "households",
                                           .data$units == "veh" ~ "vehicles",
                                           .data$units == "trip" ~ "trips"))



  # Dictionary -------------
  definition_row <-
    tbi_dict %>%
    dplyr::filter(.data$variable == variable_row) %>%
    dplyr::select(.data$variable_label, .data$survey_question,
                  .data$variable_logic, .data$which_table, .data$category) %>%
    unique()

  definition_col <-
    tbi_dict %>%
    dplyr::filter(.data$variable == variable_col) %>%
    dplyr::select(.data$variable_label, .data$survey_question,
                  .data$variable_logic, .data$which_table, .data$category) %>%
    unique()

  # return -----
  two_way_rt_list <- list(table = table,
                          definition_row = definition_row,
                          definition_col = definition_col,
                          summary = summary)

  return(two_way_rt_list)

}
