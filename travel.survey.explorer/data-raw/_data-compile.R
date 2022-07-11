# Get TBI data from helper -----
pkgload::load_all()
load("../../metc.tbi.helper/data/tbi19.rda")
source("data-raw/slim-survey-data-columns.R")

# Write Data -------------------------
tbi_tables <- list(
  "day" = tbi19$day,
  "per" = tbi19$per,
  "hh" = tbi19$hh,
  "veh" = tbi19$veh,
  "trip" = tbi19$trip,
  "trip_purpose" = tbi19$trip_purpose
)

usethis::use_data(tbi_tables,
  overwrite = TRUE,
  compress = "xz",
  internal = FALSE
)

# Work on the dictionary ------------------
source("data-raw/create-dictionary.R")

# Create additional outputs for app --------------
source("data-raw/histogram_breaks.R")
source("data-raw/input_list.R")
source("data-raw/missing_codes.R")
