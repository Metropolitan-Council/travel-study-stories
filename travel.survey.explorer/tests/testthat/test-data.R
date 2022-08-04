testthat::test_that("PII removed from data", {

  pii_cols <- c(
    "sample_home_lat",
    "sample_home_lon",
    "mode_other_comment",
    "o_lat", "o_lon",
    "d_lat","d_lon",
    "make","model",
    "class_vehicle",
    "d_purpose_other",
    "school_lat", "school_lon",
    "work_lat", "work_lon",
    "race_black_other",
    "race_black_african_other",
    "race_asian_other",
    "race_hispanic_other",
    "language_at_home_other",
    "epa_tbi_veh_match_notes",
    "dps_tbi_veh_match_notes")

  purrr::map(
    tbi_tables, function(x) {
      colnames_present <- which(names(x) %in% pii_cols)
      if(length(colnames_present) > 1) {
        testthat::fail(
          message = c(
            "PII data present",
            glue::glue("{names(x)[colnames_present]}")))
      }

    })
})
