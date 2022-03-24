# Packages -------------
suppressMessages(library(bit64, quietly = T)) # for loading in integer IDs for households/persons in TBI dataset
suppressMessages(library(tidyverse, quietly = T)) # data manipulation
suppressMessages(library(data.table, quietly = T)) # data manipulation
suppressMessages(library(sf, quietly = T)) # for mapping

suppressMessages(library(here, quietly = T)) # working directories
suppressMessages(library(lubridate, quietly = T)) # dates and times
suppressMessages(library(DBI, quietly = T)) # link to internal GIS database
suppressMessages(library(ROracle, quietly = T)) # link to internal Oracle database
suppressMessages(library(keyring, quietly = T)) # store passwords

# Get data -----------

# Configure database time zone
Sys.setenv(TZ = "America/Chicago")
Sys.setenv(ORA_SDTZ = "America/Chicago")

# read connection string (git-ignored)
source("connect_string.R")

## connect to database ------------
tbidb <- ROracle::dbConnect(
  dbDriver("Oracle"),
  dbname = connect_string,
  username = "mts_planning_data",
  # mts_planning_view for viewing data only, no write privileges.
  # mts_planning_data is the username for write privileges.
  password = keyring::key_get("mts_planning_data_pw")
)

## Load tables ---------
hh <- ROracle::dbReadTable(tbidb, "TBI19_HOUSEHOLD_RAW") %>% as.data.table()
per <- ROracle::dbReadTable(tbidb, "TBI19_PERSON_RAW") %>% as.data.table()
trip <- ROracle::dbReadTable(tbidb, "TBI19_TRIP_RAW") %>% as.data.table()
veh <- ROracle::dbReadTable(tbidb, "TBI19_VEHICLE_RAW") %>% as.data.table()
day <- ROracle::dbReadTable(tbidb, "TBI19_DAY_RAW") %>% as.data.table()

## Translate tables using dictionary -----------
dictionary <-
  ROracle::dbReadTable(tbidb, "TBI19_DICTIONARY") %>%
  select(-table) %>%
  unique() %>%
  as.data.table()

# note: this part uses data.table syntax and functions.
translate_using_dictionary <- function(dat, dictionary) {
  # select the names of columns that do not need to be translated -
  # (anything column that's not in the codebook):
  dat_id_vars <-
    names(dat[, !colnames(dat) %in% unique(dictionary$variable), with = FALSE])

  # melt the dataset:
  dat_long <-
    # suppressing warning about column types being coerced to character.
    suppressWarnings(
      melt(
        dat,
        var = "variable",
        val = "value",
        id.vars = dat_id_vars
      ),
      classes = c("message", "warning")
    )

  # convert var/value pairs to character, for both dictionary and data:
  dat_long[, c("variable", "value") := list(as.character(variable), as.character(value))]
  dictionary[, c("variable", "value") := list(as.character(variable), as.character(value))]

  # merge data to dictionary:
  dat_long <- merge(
    dat_long,
    dictionary,
    on = c("variable", "value"),
    # keep all data
    all.x = T,
    # don't keep all the extraneous dictionary
    all.y = F
  )

  # cast back to wide:
  dat_cast_formula <-
    as.formula(paste(paste(dat_id_vars, collapse = " + "), "~ variable"))
  newdat <-
    dcast(dat_long, dat_cast_formula, value.var = "value_label")

  # fix factor variables - relevel according to the order in the codebook (to start)
  namevec <- names(newdat)
  for (i in namevec) {
    if (i %in% unique(dictionary$variable)) {
      col_levels <- dictionary$value_label[dictionary$variable == i]
      newdat[, (i) := factor(get(i), levels = col_levels)]
    }
  }
  newdat <- droplevels(newdat)
  return(newdat)
}

per <- translate_using_dictionary(per, dictionary)
hh <- translate_using_dictionary(hh, dictionary)
veh <- translate_using_dictionary(veh, dictionary)
day <- translate_using_dictionary(day, dictionary)
trip <- translate_using_dictionary(trip, dictionary)

# Replace missing with NA -----------
# all the numeric codes for missing:
all_missing_codes <-
  dictionary[grep("Missing", value_label), "value", with = F]
all_missing_codes <- unique(all_missing_codes$value)

# all the value labels that include missing:
all_missing_labels <-
  dictionary[grep("Missing", value_label), "value_label", with = F]

# both as character:
all_missing_vector <- unique(rbind(all_missing_codes, all_missing_labels, use.names = F))
all_missing_vector <- all_missing_vector$x


# function to replace missing values with NA:
replace_survey_missing <- function(dat) {
  na_dat <- dat %>%
    mutate(across(
      where(is.numeric),
      ~ ifelse(. %in% all_missing_codes, NA, .)
    )) %>%
    # replace factor entries with NA:
    mutate(across(
      where(is.factor),
      ~ factor(., exclude = all_missing_vector)
    ))

  return(na_dat)
}

day <- replace_survey_missing(day)
trip <- replace_survey_missing(trip)
hh <- replace_survey_missing(hh)
per <- replace_survey_missing(per)
veh <- replace_survey_missing(veh)

rm(all_missing, all_missing_labels, all_missing_vector, all_missing_codes)

# Set IDs as Integer64 -----------
hh[, hh_id := as.integer64(hh_id)]
veh[, hh_id := as.integer64(hh_id)]
day[, c("hh_id", "person_id") := lapply(.SD, as.integer64),
  .SDcols = c("hh_id", "person_id")
]
trip[, c("hh_id", "person_id", "trip_id") := lapply(.SD, as.integer64),
  .SDcols = c("hh_id", "person_id", "trip_id")
]
per[, c("hh_id", "person_id") := lapply(.SD, as.integer64),
  .SDcols = c("hh_id", "person_id")
]

# Simplify answers to select-all questions -----------
## Race -----------
per_race <-
  per %>%
  select(person_id, starts_with("ethnicity")) %>%
  pivot_longer(cols = starts_with("ethnicity"), names_prefix = "ethnicity_") %>%
  filter(value == "Yes") %>%
  select(-value) %>%
  group_by(person_id) %>%
  add_tally(name = "num_races") %>%
  mutate(race = recode(name,
    "afam" = "Black or African-American",
    "white" = "White",
    "asian" = "Asian",
    "aiak" = " American Indian or Alaska Native",
    "hisp" = "Hispanic, Latino, or Spanish origin",
    "mideast" = "Middle-Eastern",
    "hapi" = "Native Hawaiian or other Pacific Islander",
    "other" = "Other"
  )) %>%
  mutate(race_ethnicity_simple = ifelse(num_races >= 2, "2 or more races", race)) %>%
  select(-num_races, -name, -race) %>%
  unique()


per <- per %>%
  left_join(per_race)

rm(per_race)

# Select only relevant columns
trip <- trip %>%
  select(
    person_id, trip_num, trip_id, hh_id, person_num,
    day_num, travel_date, leg_num, linked_trip_num, depart_time,
    arrive_time, o_lat, o_lon, o_bg, o_county, o_state,
    d_lat, d_lon, d_bg, d_county, d_state,
    duration_imputed, distance, speed_mph_imputed,
    depart_time_imputed, trip_weight,
    d_purpose_category_imputed, d_purpose_imputed,
    mode_type, mode_type_detailed,
    num_travelers, o_purpose_category_imputed, o_purpose_imputed
  )


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


usethis::use_data(tbi_dict,
  overwrite = TRUE,
  compress = "xz",
  internal = FALSE
)


## Add new column to dictionary #TODO----

# Connect to ancillary Data -----------
## Vehicle efficiency (EPA) -----------
veh_epa <- read.csv("data-raw/veh_epa.csv") %>%
  mutate(make = toupper(make), model = toupper(model))

veh <- veh %>%
  left_join(veh_epa)

rm(veh_epa)

## Add new column to dictionary: #TODO --------

## Vehicle weight (DPS) -----------
# Load Car Weight Data
Vehicle_wtsDPS <- read_csv("data-raw/Vehicle_wtsDPS.CSV",
  col_types = cols(X1 = col_skip())
)

veh <- veh %>%
  mutate(make = toupper(make), model = toupper(model)) %>%
  left_join(Vehicle_wtsDPS) %>%
  mutate(wt_cat = case_when(
    median_wt <= 6000 ~ "6,000 lbs or less",
    median_wt < 8500 ~ "6,001 - 8,500 lbs",
    median_wt < 10000 ~ "8,501 - 10,000 lbs"
  )) %>%
  mutate(wt_cat = factor(wt_cat, levels = c(
    "6,000 lbs or less",
    "6,001 - 8,500 lbs",
    "8,501 - 10,000 lbs"
  )))

rm(Vehicle_wtsDPS)

## Add new column to dictionary: #TODO--------




## Geospatial Data -----------
# Spatial coordinates - cast as such.
# Trip origins and destinations
trip_d_sf <- trip %>%
  select(trip_id, d_lon, d_lat) %>%
  na.omit() %>%
  st_as_sf(coords = c("d_lon", "d_lat"), crs = 4326)
trip_o_sf <- trip %>%
  select(trip_id, o_lon, o_lat) %>%
  na.omit() %>%
  st_as_sf(coords = c("o_lon", "o_lat"), crs = 4326)

# Household locations
hh_sf <- hh %>%
  select(hh_id, home_lon, home_lat) %>%
  na.omit() %>%
  st_as_sf(
    coords = c("home_lon", "home_lat"),
    crs = 4326
  )

# Work locations
work_sf <- per %>%
  select(person_id, work_lon, work_lat) %>%
  na.omit() %>%
  st_as_sf(
    coords = c("work_lon", "work_lat"),
    crs = 4326
  )

# School locations
school_sf <- per %>%
  select(person_id, school_lon, school_lat) %>%
  na.omit() %>%
  st_as_sf(
    coords = c("school_lon", "school_lat"),
    crs = 4326
  )

# Download Geographies Needed
db <- DBI::dbConnect(odbc::odbc(), "GISLibrary")

# CTU (Cities-Townships-Unincorporated)
ctu_sf <- DBI::dbGetQuery(
  db,
  "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.CTUs;"
) %>%
  st_as_sf(wkt = "geometry", crs = "+init=epsg:26915") %>%
  st_transform(crs = "+init=epsg:26915 +proj=longlat +datum=WGS84")
ctu_sf <- st_transform(ctu_sf, crs = 4326)


# County
mn_cty_sf <- DBI::dbGetQuery(
  db,
  "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.MNCounties;"
) %>%
  st_as_sf(wkt = "geometry", crs = "+init=epsg:26915") %>%
  st_transform(crs = "+init=epsg:26915 +proj=longlat +datum=WGS84") %>%
  select(CO_NAME) %>%
  mutate(CO_NAME = toupper(CO_NAME)) %>%
  mutate(State = "MN")
wi_cty_sf <- DBI::dbGetQuery(
  db,
  "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.WICounties;"
) %>%
  st_as_sf(wkt = "geometry", crs = "+init=epsg:26915") %>%
  st_transform(crs = "+init=epsg:26915 +proj=longlat +datum=WGS84") %>%
  select(CO_NAME) %>%
  mutate(State = "WI")

cty_sf <- rbind(mn_cty_sf, wi_cty_sf) %>%
  st_transform(cty_sf, crs = 4326)

# MPO Area (Metropolitan Planning Organization - Twin Cities)
mpo_sf <- DBI::dbGetQuery(
  db,
  "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.MetropolitanPlanningOrganizationArea;"
) %>%
  st_as_sf(wkt = "geometry", crs = "+init=epsg:26915") %>%
  st_transform(crs = "+init=epsg:26915 +proj=longlat +datum=WGS84")
mpo_sf <- st_transform(mpo_sf, crs = 4326)



### MPO JOINS
# (1) TRIPS

# find trip IDs where origin or destination falls within MPO area:
trip_o_ids <-
  st_join(trip_o_sf, mpo_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
  as.data.frame() %>%
  filter(OBJECTID == 1) %>%
  select(trip_id)
trip_d_ids <-
  st_join(trip_d_sf, mpo_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
  as.data.frame() %>%
  filter(OBJECTID == 1) %>%
  select(trip_id)
hh_ids <-
  st_join(hh_sf, mpo_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
  as.data.frame() %>%
  filter(OBJECTID == 1) %>%
  select(hh_id)
work_ids <-
  st_join(work_sf, mpo_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
  as.data.frame() %>%
  filter(OBJECTID == 1) %>%
  select(person_id)
school_ids <-
  st_join(school_sf, mpo_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
  as.data.frame() %>%
  filter(OBJECTID == 1) %>%
  select(person_id)

# Join back to main tables:
trip[, trip_in_mpo := ifelse(
  trip_id %in% trip_d_ids$trip_id |
    trip_id %in% trip_o_ids$trip_id,
  "trip_in_mpo",
  "trip_outside_mpo"
)]

trip[, trip_d_in_mpo := ifelse(trip_id %in% trip_d_ids$trip_id,
  "trip_ends_in_mpo",
  "trip_ends_outside_mpo"
)]
trip[, trip_o_in_mpo := ifelse(trip_id %in% trip_o_ids$trip_id,
  "trip_starts_in_mpo",
  "trip_starts_outside_mpo"
)]


hh[, hh_in_mpo := ifelse(hh_id %in% hh_ids$hh_id, "in_mpo", "outside_mpo")]
per[, work_in_mpo := ifelse(
  is.na(work_lon),
  NA,
  ifelse(person_id %in% work_ids$person_id, "in_mpo", "outside_mpo")
)]
per[, school_in_mpo := ifelse(
  is.na(school_lon),
  NA,
  ifelse(person_id %in% school_ids$person_id, "in_mpo", "outside_mpo")
)]


# (2) COUNTY & STATE
# already done for work, school, home.
trip_o_cty <-
  st_join(trip_o_sf, cty_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
  as.data.frame() %>%
  rename(trip_o_county = CO_NAME, trip_o_state = State) %>%
  # interesting problem with counties for trips - 2 trips that end on the mn/wi border
  group_by(trip_id) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(trip_id, trip_o_county, trip_o_state)

trip_d_cty <-
  st_join(trip_d_sf, cty_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
  as.data.frame() %>%
  rename(trip_d_county = CO_NAME, trip_d_state = State) %>%
  # interesting problem with counties for trips - 2 trips that end on the mn/wi border
  group_by(trip_id) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(trip_id, trip_d_county, trip_d_state)

# Join to trip table:
trip <- merge(trip, trip_o_cty, by = "trip_id", all.x = T)
trip <- merge(trip, trip_d_cty, by = "trip_id", all.x = T)

# (3) Census Block Group
# already done for work, school, home.
trip_o_cbg <-
  st_join(trip_o_sf, cbg_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
  as.data.frame() %>%
  rename(trip_o_cbg = GEOID10) %>%
  select(trip_id, trip_o_cbg)
trip_d_cbg <-
  st_join(trip_d_sf, cbg_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
  as.data.frame() %>%
  rename(trip_d_cbg = GEOID10) %>%
  select(trip_id, trip_d_cbg)

# Join to trip table:
trip <- merge(trip, trip_o_cbg, by = "trip_id", all.x = T)
trip <- merge(trip, trip_d_cbg, by = "trip_id", all.x = T)

# (3) TAZ
trip_o_taz <-
  st_join(trip_o_sf, taz_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
  as.data.frame() %>%
  rename(trip_o_taz = TAZ) %>%
  select(trip_id, trip_o_taz)
trip_d_taz <-
  st_join(trip_d_sf, taz_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
  as.data.frame() %>%
  rename(trip_d_taz = TAZ) %>%
  select(trip_id, trip_d_taz)
trip <- merge(trip, trip_o_taz, by = "trip_id", all.x = T)
trip <- merge(trip, trip_d_taz, by = "trip_id", all.x = T)

hh_taz <-
  st_join(hh_sf, taz_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
  as.data.frame() %>%
  rename(hh_taz = TAZ) %>%
  select(hh_id, hh_taz)
hh <- merge(hh, hh_taz, by = "hh_id", all.x = T)

work_taz <-
  st_join(work_sf, taz_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
  as.data.frame() %>%
  rename(work_taz = TAZ) %>%
  select(person_id, work_taz)

school_taz <-
  st_join(school_sf, taz_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
  as.data.frame() %>%
  rename(school_taz = TAZ) %>%
  select(person_id, school_taz)


per <- merge(per, work_taz, by = "person_id", all.x = T)
per <- merge(per, school_taz, by = "person_id", all.x = T)


# (5) CTU
trip_o_ctu <-
  st_join(trip_o_sf, ctu_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
  as.data.frame() %>%
  rename(trip_o_ctu = CTU_NAME) %>%
  select(trip_id, trip_o_ctu)
trip_d_ctu <-
  st_join(trip_d_sf, ctu_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
  as.data.frame() %>%
  rename(trip_d_ctu = CTU_NAME) %>%
  select(trip_id, trip_d_ctu)

trip <- merge(trip, trip_o_ctu, by = "trip_id", all.x = T)
trip <- merge(trip, trip_d_ctu, by = "trip_id", all.x = T)

hh_ctu <-
  st_join(hh_sf, ctu_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
  as.data.frame() %>%
  rename(hh_ctu = CTU_NAME) %>%
  select(hh_id, hh_ctu)
hh <- merge(hh, hh_ctu, by = "hh_id", all.x = T)

work_ctu <-
  st_join(work_sf, ctu_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
  as.data.frame() %>%
  rename(work_ctu = CTU_NAME) %>%
  select(person_id, work_ctu)

school_ctu <-
  st_join(school_sf, ctu_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
  as.data.frame() %>%
  rename(school_ctu = CTU_NAME) %>%
  select(person_id, school_ctu)


per <- merge(per, work_ctu, by = "person_id", all.x = T)
per <- merge(per, school_ctu, by = "person_id", all.x = T)


### Thrive Category -----------
### MPO boundary -----------
### PUMA -----------

# Custom temporal categories -----------
### Seasons -----------
getSeason <- function(input.date) {
  numeric.date <- 100 * month(input.date) + day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  # This is set to meterological seasons https://www.timeanddate.com/calendar/aboutseasons.html
  cuts <- base::cut(numeric.date, breaks = c(0, 301, 0531, 0831, 1130, 1231))
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter", "Spring", "Summer", "Fall", "Winter")
  return(cuts)
}

day <- day %>%
  mutate(season = getSeason(travel_date))

trip <- trip %>%
  mutate(season = getSeason(travel_date))

rm(getSeason)

### Times of day -----------

# Custom intersection tables -----------
## Vehicle x Household -----------
## Vehicle x EPA -----------
## Vehicle x Trip -----------
## Household x Race -----------
## Linked trips -----------
## Trip purpose -----------
## Trip mode -----------

# Aggregating detailed categories -----------
## Income -----------
## Race -----------
## Trip mode -----------
## Trip purpose -----------

# Select relevant columns -----------

# Redact all lat/lon data

# Make tables into survey objects -----------
per_svy <- as_survey_design(per, weights = person_weight)
