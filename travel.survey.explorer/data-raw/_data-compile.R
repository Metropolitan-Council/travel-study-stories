# Get TBI survey data from database ---------
source("data-raw/get-survey-data.R")

# Append geographic boundaries to  household, work, school, and trip -----------
source("data-raw/add-geographic-boundaries.R")

# Get EPA Efficiency Data -----------
source("data-raw/get-epa-vehicle-efficiency-data.R")

# Get DPS Vehicle Weight Data -----------
source("data-raw/get-dps-vehicle-weight-data.R")

# Extra variables ------
source("data-raw/add-var-person-race.R")
source("data-raw/add-var-hh-income-easyread.R")
source("data-raw/add-var-trip-purpose.R")
source("data-raw/add-var-trip-mode-group.R")
source("data-raw/add-var-trip-purpose-broad.R")
source("data-raw/add-var-trip-seasons.R")

# Re-format time
trip <- trip %>%
  mutate(
    depart_time_imputed = as.ITime(depart_time_imputed),
    arrive_time = as.ITime(arrive_time)
  )

# Remove PII ------------------
source("data-raw/remove-pii.R")

# Trim columns down for manageability ----------
# source("data-raw/slim-survey-data-columns.R")


# Write Data -------------------------
tbi_tables <- list(
  "day" = day,
  "per" = per,
  "hh" = hh,
  "veh" = veh,
  "trip" = trip,
  "trip_purpose" = trip_purpose
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


