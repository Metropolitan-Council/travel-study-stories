testthat::test_that("PII removed from data", {
  purrr::map(
    tbi_tables, function(x) {
      colnames_present <- which(names(x) %in% pii_codes)
      if (length(colnames_present) > 1) {
        testthat::fail(
          message = c(
            "PII data present",
            glue::glue("{names(x)[colnames_present]}")
          )
        )
      } else {
        testthat::succeed("No PII present")
      }
    }
  )
})
