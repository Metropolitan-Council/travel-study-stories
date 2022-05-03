#' table_one_way UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_table_one_way_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("table"), width = "80%")
  )
}

#' table_one_way Server Functions
#'
#' @noRd
mod_table_one_way_server <- function(id, one_way_inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$table <- DT::renderDataTable({
      # browser()
      # req(table_data)
      context_data <- one_way_inputs$context_data
      table_data <- one_way_inputs$table_data

      DT::datatable(table_data %>%
                      dplyr::select(1,
                                    estimated_prop,
                                    estimated_prop_se,
                                    group_N) %>%
                      mutate(estimated_prop_se = scales::percent(estimated_prop_se,
                                                                 accuracy = 0.01)),
                    rownames = F,
                    colnames = c(context_data$variable_label,
                                 "Estimated proportion",
                                 "Standard error",
                                 "Group size")) %>%
        DT::formatPercentage(columns = c(2), digits = 2) %>%
        DT::formatString(columns = c(3),
                         prefix = "+/-") %>%
        DT::formatRound(columns = c(4), digits = 0)
    })

  })
}

## To be copied in the UI
# mod_table_one_way_ui("table_one_way_ui_1")

## To be copied in the server
# mod_table_one_way_server("table_one_way_ui_1")
