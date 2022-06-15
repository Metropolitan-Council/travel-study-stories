## code to prepare `input_list` dataset goes here

library(dplyr)
# library(travel.survey.explorer)

tbi_dict_app <- tbi_dict %>%
  filter(stringr::str_detect(variable, "ethnicity", negate = T))

input_list <- purrr::map(unique(tbi_dict_app$category), function(x) {
  tb <- tbi_dict_app %>%
    dplyr::filter(category == x)

  variab <- tb$variable %>%
    unique() %>%
    as.list()

  variab_name <- tb$variable_label %>%
    unique() %>%
    as.list()

  names(variab) <- variab_name
  return(variab)
})

names(input_list) <- unique(tbi_dict_app$category)


usethis::use_data(input_list, overwrite = TRUE)


## code to prepare `input_question_list` dataset goes here


input_question_list <- purrr::map(unique(tbi_dict_app$variable), function(x) {
  tb <- tbi_dict_app %>%
    dplyr::filter(variable == x)

  variab <- tb$variable %>%
    unique() %>%
    as.list()

  variab_name <- tb$survey_question %>%
    unique() %>%
    as.list()

  names(variab) <- variab_name

  return(variab)
})


names(input_question_list) <- unique(tbi_dict_app$variable)


usethis::use_data(input_question_list, overwrite = TRUE)
