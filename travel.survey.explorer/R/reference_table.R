#' @title Formatted DT for dictionary
#'
#' @return DT object
#' @export
#' @importFrom dplyr select group_by summarize
#' @importFrom DT datatable
reference_table <- function(){

  tbi_dict %>%
    dplyr::select(Category = .data$category,
                  Variable = .data$variable_label,
                  `Survey Question` = .data$survey_question,
                  .data$value_label) %>%
    dplyr::group_by(.data$Category,
                    .data$Variable,
                    .data$`Survey Question`) %>%
    dplyr::summarize(`Response values` = paste(.data$value_label, collapse = "; ", sep = "\n")) %>%
    dplyr::mutate(Category = factor(.data$Category)) %>%
    unique() %>%
    DT::datatable(rownames = F, filter = "top",
                  width = "100%",
                  options = list(
                    dom = "tp",
                    pageLength = 10
                  ))
}
