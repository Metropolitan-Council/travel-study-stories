#' input_utils UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_utils_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' @title input_utils Server Functions
#'
#' @noRd
#'
#' @return [shiny::reactiveValues()] `table_data` and `context_data`.
#'
mod_input_utils_server <- function(id,
                                   user_var,
                                   util_hh_ids,
                                   n_ways = 1,
                                   util_variable_row,
                                   util_variable_col) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    vals <- reactiveValues()

    # generate table

    rt_list <- reactive({
      if (n_ways == 1) {
        t <- create_one_way_table(
          variable_row = user_var$variable,
          hh_ids = util_hh_ids$hh_ids
        )
        return(t)
      } else {
        validate(need(util_variable_col$variable != util_variable_row$variable,
          message = "Column and row inputs must be different"
        ))

        create_two_way_table(
          variable_row = util_variable_row$variable,
          variable_col = util_variable_col$variable,
          hh_ids = util_hh_ids$hh_ids
        )
      }
    })


    # browser()
    if (n_ways == 1) {
      observe({
        one_way_rt <- rt_list()

        vals$table_return <- one_way_rt$table
        vals$definitions_return <- one_way_rt$definitions
        vals$summary_return <- one_way_rt$summary

        print(vals$table_return) # eventually comment out

        return(vals)
      }) %>%
        bindEvent(user_var$variable, util_hh_ids$hh_ids)
    } else {
      observe({
        two_way_rt <- rt_list()

        vals$two_way_list <- two_way_rt
        vals$table_return <- two_way_rt$table
        vals$definitions_return <- two_way_rt$definition_row
        vals$context_row_return <- two_way_rt$definition_row
        vals$context_col_return <- two_way_rt$definition_col

        vals$summary_return <- two_way_rt$summary

        print(vals$table_return) # eventually comment out

        return(vals)
      }) %>%
        bindEvent(util_variable_row$variable, util_variable_col$variable, util_hh_ids$hh_ids)
    }

    return(vals)
  })
}

## To be copied in the UI
# mod_input_utils_ui("input_utils_ui_1")

## To be copied in the server
# mod_input_utils_server("input_utils_ui_1")
