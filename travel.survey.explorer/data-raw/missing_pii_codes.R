# missing codes

missing_codes <- c(
  "-9998",
  "995",
  "-9999",
  "-1",
  "Missing: Non-response",
  "Missing: Skip logic",
  "Missing: Error",
  "Missing: Technical error",
  "Missing: Non-imputable",
  NA,
  ""
)

usethis::use_data(missing_codes, overwrite = T)

pii_codes <- c(
  # home
  "sample_home_lat", "sample_home_lon", "sample_home_bg",
  # origin
  "o_lat", "o_lon", "o_bg",
  # destination
  "d_lat", "d_lon", "d_bg", "d_purpose_other",
  # vehicle
  "make", "model", "class_vehicle", "epa_tbi_veh_match_notes",
  # mode
  "mode_other_comment",
  # school location
  "school_lat", "school_lon", "school_bg",
  # work location
  "work_lat", "work_lon", "work_bg",
  # secondary race/ethnicity
  "race_black_other",
  "race_black_african_other",
  "race_asian_other",
  "race_hispanic_other",
  # secondary language
  "language_at_home_other"
)

usethis::use_data(pii_codes, overwrite = T)
