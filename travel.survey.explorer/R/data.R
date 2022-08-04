#' @title Histogram breaks
#' @description Pre-set bistogram breaks
#'
#' @format named list
# histogram_breaks ----
"histogram_breaks"

#' @title Input list
#' @format named list of possible user inputs
#' @family data
# input_list -----
"input_list"

#' @title Input question list
#' @format named list for fetching the corresponding survey question for a given
#'     variable.
#' @family data
# input_question_list -----
"input_question_list"


#' @title Missing data codes
#'
#' @format vector, 10 character values indicating that data is missing
#' @family data
# missing_codes -----
"missing_codes"

#' @title Personally identifiable information
#'
#' @format vector, 26 character values indicating sensitive data. These data are removed from `tbi_tables`
#' @family data
# pii_codes -----
"pii_codes"

#' @title Plotly formatting
#' @format nested, named list with specifications for [plotly::layout()]
#' @family data
#' @examples
#' library(travel.survey.explorer)
#' plotly_layout$hover_text$size
#'
# plotly_layout -----
"plotly_layout"


#' @title Travel Behavior Inventory Dictionary
#' @format data.frame with 10 columns and 904 rows. Contains contextual
#'     and meta data o
#'     - `which_table` character,
#'         one of `c("per", "trip", "day", "veh", "hh", "trip_purpose")`
#'     - `category` character,
#'         one of  `c("Demographics", "Attitudes toward autonomous vehicles", "Shared mobility",  "Commute", "Trips", "Days without travel", "Delivery & online shopping", "Vehicle")`
#'     - `variable` character, short form variable code in snake case
#'     - `survey_question` character, survey question as it was asked to the respondent
#'     - `variable_logic` character, any associated logic. Some questions were only asked
#'         if a respondent had a certain answer to a previous question.
#'     - `variable label` character, person-readable variable label.
#'     - `value` numeric, potential response value
#'     - `value_label` character, label corresponding to response value
#'     - `value_logic` character, any associated logic. Some questions were only asked
#'         if a respondent had a certain answer to a previous question.
#'     - `wt_field` character, one of `c("person_weight", "trip_weight", "day_weight", "hh_weight", "trip_purpose_weight")`
#'
#' @family data
# tbi_dict -----
"tbi_dict"


#' @title Travel Behavior Inventory data tables
#'
#' @format named list of six tables for each type of TBI data.
#'     - `day` - Day table
#'     - `per`
#'     - `hh`
#'     - `veh`'
#'     - `trip`
#'     - `trip_purpose`
#' @family data
# tbi_tables -----
"tbi_tables"
