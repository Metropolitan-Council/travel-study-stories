#' @title Create a one-way cross table
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
create_one_way_table <- function(variable){

  variable <- "distance"
  this_variable <- variable

  this_table <-
    tbi_dict %>%
    dplyr::filter(variable == this_variable) %>%
    dplyr::select(which_table) %>%
    unique() %>%
    magrittr::extract2(1)

  this_weight <-
    tbi_dict %>%
    dplyr::filter(variable == this_variable) %>%
    dplyr::select(wt_field) %>%
    unique() %>%
    magrittr::extract2(1)

  vartype <-
  tbi_tables[[this_table]] %>%
    select(sym(this_variable)) %>%
    summarize_all(class) %>%
    purrr::pluck(1)

  if(vartype == "numeric"){
    # do a thing for numeric variables -- binning???
    # get a survey_mean? survey_median?
  } else {
    rt_tab <- tbi_tables[[this_table]] %>%
      dplyr::select(rlang::sym(this_variable),
                    rlang::sym(this_weight), hh_id) %>%
      select(sym(this_variable)) %>%
      summarize_all(class) %>%
      pluck(1)
    # dplyr::filter where the variable is missing (missing codes = "Missing: No Response", "Missing: skip logic").
    # this list of missing codes is created in data-raw/data_compile.R, line ~105 and
    # data-raw/missing_codes.R
    dplyr::filter(!(get(this_variable) %in% missing_codes)) %>%
      # clean up:
      droplevels() %>%
      # big N sample size - for the whole data frame:
      dplyr::mutate(total_N = length(hh_id), # raw sample size - number of people, trips, households, days
                    total_N_hh = length(unique(hh_id))) %>% # total number of households in sample
      srvyr::as_survey_design(weights = !!this_weight) %>%
      dplyr::group_by(
        # grouping by number of samples, number of households to keep this info
        total_N, total_N_hh,
        get(this_variable),) %>%
      dplyr::summarize(
        group_N = length(hh_id), # raw sample size - number of people, trips, households, days (by group)
        group_N_hh = length(unique(hh_id)), # number of households in sample (by group)
        expanded_total = srvyr::survey_total(), # expanded total and SE
        estimated_prop = srvyr::survey_prop() # estimated proportion (0.0 - 1.0) and SE; multiply by 100 for percentages.
      ) %>%
      # rename the column back to the original name - it gets weird for some reason
      dplyr::rename(!!rlang::quo_name(this_variable) := `get(this_variable)`) %>%
      dplyr::select(all_of(this_variable), "total_N", "total_N_hh","group_N", "group_N_hh",
                    "expanded_total", "expanded_total_se", "estimated_prop", "estimated_prop_se") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(dplyr::across(tidyselect:::where(is.numeric), round, digits = 5))

    return(rt_tab)
  }

}
