# Goal: create process to programatically generate DT, formatted according to generalized
# user inputs.
# maybe use D

library(bit64)
library(travel.survey.explorer)
pkgload::load_all()

all_hh_ids <- tbi_tables$hh %>%
  dplyr::select(hh_id) %>%
  purrr::pluck(1)

twoway_data <- create_two_way_table(
  variable_row = "age",
  variable_col = "bus_payment_type",
  hh_ids = all_hh_ids
)

twoway_test$table

this_table <- twoway_test$table %>%
  dplyr::mutate(estimated_prop_se = scales::percent(estimated_prop_se,
    accuracy = 0.01
  )) %>%
  dplyr::select(
    row_var = 1,
    col_var = 2,
    proportion = estimated_prop,
    se = estimated_prop_se
    # sample = group_N
  ) %>%
  tidyr::pivot_wider(
    names_from = col_var,
    values_from = c(
      proportion,
      se
      # sample
    ),
    names_vary = "slowest",
    names_glue = "{col_var} ({.value})"
  )

colnames(this_table)

super_col_headers <- twoway_test$table[2] %>%
  unique() %>%
  extract2(1)
sub_col_headers <- c("Share", "Standard Error")

# a custom table container
sketch <- htmltools::withTags(
  table(
    class = "display",
    thead(
      tr(
        th(
          class = "dt-center",
          colspan = ncol(this_table) - 1,
          twoway_test$definition_col$variable_label
        )
      ),
      tr(
        th(class = "dt-center", rowspan = 2, twoway_test$definition_row$variable_label),
        lapply(super_col_headers,
          th,
          colspan = 2
        ),
      ),
      tr(
        lapply(rep(sub_col_headers, length(super_col_headers)), function(x) {
          th(class = "dt-center", style = "font-size:14px", x)
        })
      )
    )
  )
)


DT:::datatable(this_table, container = sketch, rownames = F) %>%
  DT::formatPercentage(columns = which(
    sapply(this_table, is.numeric),
    TRUE
  ), digits = 1) %>%
  DT::formatString(
    columns = which(
      sapply(
        this_table,
        is.character
      ),
      TRUE
    ),
    prefix = "+/-"
  )
