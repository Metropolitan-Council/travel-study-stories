# Get TBI survey data from database ---------
source("data-raw/get-survey-data.R")

# Work on the dictionary ------------------
source("data-raw/create-dictionary.R")

# Trim columns down for manageability ----------
# source("data-raw/slim-survey-data-columns.R")

# Trim survey data to MPO region -----------
source("data-raw/trim-survey-data-to-mpo.R")

# Get EPA and Vehicle Weight Data -----------
source("data-raw/get-epa-vehicle-efficiency-data.R")

source("data-raw/get-dps-vehicle-weight-data.R")


# Append Thrive Category -----------
source("data-raw/add-thrive-to-hh-trip.R")

# Append MPO boundary to trips ------------
source("data-raw/add-mpo-boundary-to-trips.R")

# Append CTU to household, trip and person (work/school location) table -----------
source("data-raw/add-var-hh-work-school-trip-ctu.R")


# Re-format time
trip <- trip %>%
  mutate(depart_time_imputed = as.ITime(depart_time_imputed),
         arrive_time = as.ITime(arrive_time))

# check for any PII and remove
# for vehicle table, remove make, model, year and name, and then round the numbers from DPS/EPA
veh <- veh %>%
  select(-make, -model, -vehicle_name, -class_vehicle) %>%
  select(-epa_tbi_veh_match_notes, -dps_tbi_veh_match_notes) %>%
  mutate(veh_age = 2019 - year) %>%
  select(-year) %>%
  mutate(
    co2_gpm = round(co2_gpm, -1),
    mpg_city = round(mpg_city, 0),
    mpg_highway = round(mpg_highway, 0),
    weight_unladen = round(weight_unladen, -2)
  ) %>%
  left_join(hh %>% select(hh_id, hh_weight))

hh <-
  hh %>%
  select(-home_lat, -home_lon)

trip <-
  trip %>%
  select(-o_lat, -o_lon, -d_lat, -d_lon)


per <- per %>%
  select(-ethnicity_other_specify)



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


