# Get TBI survey data from database ---------
source("data-raw/get-survey-data.R")

# Append geographic boundaries to  household, work, school, and trip -----------
source("data-raw/add-geographic-boundaries.R")

# Get EPA Efficiency Data -----------
source("data-raw/get-epa-vehicle-efficiency-data.R")

# Get DPS Vehicle Weight Data -----------
source("data-raw/get-dps-vehicle-weight-data.R")

# Extra variables ------


# Re-format time
trip <- trip %>%
  mutate(depart_time_imputed = as.ITime(depart_time_imputed),
         arrive_time = as.ITime(arrive_time))

# Remove PII ------------------
source("data-raw/remove-pii.R")

# Trim columns down for manageability ----------
# source("data-raw/slim-survey-data-columns.R")

# Work on the dictionary ------------------
source("data-raw/create-dictionary.R")


# Write Data -------------------------
tbi_tables <- list(
  "day" = day,
  "per" = per,
  "hh" = hh,
  "veh" = veh,
  "trip" = trip
)

usethis::use_data(tbi_tables,
                  overwrite = TRUE,
                  compress = "xz",
                  internal = FALSE
)


