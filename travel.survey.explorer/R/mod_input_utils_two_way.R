#' input_utils UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_utils_two_way_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' @title input_utils Server Functions
#'
#' @noRd
#'
#' @return [shiny::reactiveValues()] `table_data` and `context_data`.
#'
mod_input_utils_two_way_server <- function(id, util_variable_row, util_variable_col, util_hh_ids) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    vals <- reactiveValues()

    # generate table
    two_way_rt_list <- reactive({
      t <- create_two_way_table(variable_row = util_variable_row$variable,
                           variable_col = util_variable_col$variable,
                           hh_ids = util_hh_ids$hh_ids)

      t$table_display <- t$table %>%
        dplyr::select(1:2,
                      # col_var = 2,
                      proportion = estimated_prop
                      # se = estimated_prop_se,
                      # sample = group_N
                      ) %>%
        tidyr::pivot_wider(names_from = 2,
                           values_from = c(proportion
                                           # se,
                                           # sample
                                           )
                           # names_vary = "slowest",
                           # names_glue = "{col_var} ({.value})"
        )

      return(t)
    })


    observe({
      two_way_rt <- two_way_rt_list()

      vals$table_return <- two_way_rt$table
      vals$context_row_return <- two_way_rt$definition_row
      vals$context_col_return <- two_way_rt$definition_col

      vals$summary_return <- two_way_rt$summary
      vals$table_display <- two_way_rt$table_display

      print(vals$table_return) # eventually comment out

      return(vals)

    }) %>%
      bindEvent(util_variable_row$variable, util_variable_col$variable, util_hh_ids$hh_ids)

    return(vals)
  })
}

## To be copied in the UI
# mod_input_utils_two_way_ui("input_utils_two_way_ui_1")

## To be copied in the server
# mod_input_utils_two_way_server("input_utils_two_way_ui_1")
