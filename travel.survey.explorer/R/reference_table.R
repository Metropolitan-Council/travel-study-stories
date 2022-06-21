#' @title Formatted DT for dictionary
#'
#' @return DT object
#' @export
#' @importFrom dplyr select group_by summarize
#' @importFrom DT datatable
reference_table <- function(){

  tbi_dict %>%
    dplyr::select(Category = category,
                  Variable = variable_label,
                  `Survey Question` = survey_question,
                  value_label) %>%
    dplyr::group_by(Category,
                    Variable,
                    `Survey Question`) %>%
    dplyr::summarize(`Response values` = paste(value_label, collapse = "; ", sep = "\n")) %>%
    dplyr::mutate(Category = factor(Category)) %>%
    unique() %>%
    DT::datatable(rownames = F, filter = "top",
                  width = "100%",
                  options = list(
                    dom = "tp",
                    pageLength = 10
                  ))
}
