# Get TBI survey data from database ---------
source("data-raw/get-survey-data.R")

# Trim columns down for manageability ----------
source("data-raw/slim-survey-data-columns.R")

# Trim survey data to MPO region -----------
source("data-raw/trim-survey-data-to-mpo.R")

# Get EPA and Vehicle Weight Data:
source("data-raw/get-epa-vehicle-efficiency-data.R")

source("data-raw/get-dps-vehicle-weight-data.R")


# Append Thrive Category (can aggregate to other geographies here)
source("data-raw/add-thrive-to-hh-trip.R")

# Append MPO boundary to trips
source("data-raw/add-mpo-boundary-to-trips.R")


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
  )
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


tbi_dict <- dictionary %>%
  filter(category %in% c(
    "Demographics",
    "Attitudes toward autonomous vehicles",
    "Shared mobility",
    "Commute",
    "Trips",
    "Days without travel",
    "Delivery & online shopping",
    "Vehicle"
  )) %>%
  # we'll probably want to play with the ORDERING of this case-when command
  # to assign variables to tables when they appear in multiple tables.
  mutate(which_table = case_when(
    variable %in% names(per) ~ "per",
    variable %in% names(hh) ~ "hh",
    variable %in% names(trip) ~ "trip",
    variable %in% names(day) ~ "day",
    variable %in% names(veh) ~ "veh"
  )) %>%
  # find the weighting field for each table:
  mutate(wt_field = case_when(
    which_table == "per" ~ "person_weight",
    which_table == "hh" ~ "hh_weight",
    which_table == "trip" ~ "trip_weight",
    which_table == "day" ~ "day_weight",
    which_table == "veh" ~ "hh_weight"
  ))

# # Appending missing variables to tbi_dict
# missing_vars <-
# lapply(tbi_tables, function(x) setdiff(names(x), dictionary$variable) %>% as.data.frame()) %>%
#   rbindlist(idcol = "which_table")
#
# names(missing_vars) <- c("which_table", "variable")
#
# # trim out ID's and weights
# missing_vars <-
# missing_vars %>%
#   filter(!grepl("_id", variable)) %>%
#   filter(!grepl("_weight", variable)) %>%
#   filter(!grepl("_num", variable)) %>%
#   filter(!grepl("_date", variable)) %>%
#   mutate(wt_field = case_when(
#     which_table == "per" ~ "person_weight",
#     which_table == "hh" ~ "hh_weight",
#     which_table == "trip" ~ "trip_weight",
#     which_table == "day" ~ "day_weight",
#     which_table == "veh" ~ "hh_weight"
#   ))
#
# write.csv(full_join(missing_vars, dictionary), "data-raw/full_dictionary.csv")


# some work by hand occurred:
tbi_dict <- read.csv('data-raw/full_dictionary.csv')

usethis::use_data(tbi_dict,
                  overwrite = TRUE,
                  compress = "xz",
                  internal = FALSE
)


