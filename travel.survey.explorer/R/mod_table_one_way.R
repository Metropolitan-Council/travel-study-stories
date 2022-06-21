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
    DT::DTOutput(ns("table_1way"), width = "90%")
  )
}

#'  @title table_one_way Server Functions
#'
#' @noRd
mod_table_one_way_server <- function(id, one_way_table_inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    w_1way <- waiter::Waiter$new(ns("table_1way"),
      html = waiter::spin_flower()

    )



    output$table_1way <- DT::renderDataTable({
      w_1way$show()
      definitions_data <- one_way_table_inputs$definitions_return
      summary_data <- one_way_table_inputs$summary_return
      table_data <- one_way_table_inputs$table_return

      DT::datatable(table_data %>%
        dplyr::select(
          1,
          estimated_prop,
          estimated_prop_se,
          group_N
        ) %>%
        mutate(estimated_prop_se = scales::percent(estimated_prop_se,
          accuracy = 0.01
        )),
      options = list(scrollX = TRUE),
      rownames = F,
      colnames = c(
        definitions_data$variable_label,
        "Estimated proportion",
        "Standard error",
        "Group size"
      )
      ) %>%
        DT::formatPercentage(columns = c(2), digits = 2) %>%
        DT::formatString(
          columns = c(3),
          prefix = "+/-"
        ) %>%
        DT::formatRound(columns = c(4), digits = 0) %>%
        DT::formatStyle(
          valueColumns = "group_N",
          color = DT::styleInterval(30, c("#bdbdc3", councilR::colors$suppBlack)),
          columns = 1:4
        )
    })
  })
}

## To be copied in the UI
# mod_table_one_way_ui("table_one_way_ui_1")

## To be copied in the server
# mod_table_one_way_server("table_one_way_ui_1")
