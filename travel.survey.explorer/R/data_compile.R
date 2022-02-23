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
source('connect_string.R')

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
dictionary <-  ROracle::dbReadTable(tbidb, "TBI19_DICTIONARY") %>% as.data.table()

# note: this part uses data.table syntax and functions.
translate_using_dictionary <- function(dat, dictionary) {
  # select the names of columns that do not need to be translated -
  # (anything column that's not in the codebook):
  dat <- hh

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
  dat_long[, c('variable', 'value') := list(as.character(variable), as.character(value))]
  dictionary[, c('variable', 'value') := list(as.character(variable), as.character(value))]

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
    dcast(dat_long, dat_cast_formula, value.var = 'value_label')

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

translate_using_dictionary(day, dictionary)

# Replace missing with NA -----------

# Set IDs as Integer64

# Simplify answers to select-all questions -----------
## Race -----------


# Connect to ancillary Data -----------
## Vehicle efficiency (EPA) -----------
veh_epa <- read.csv('Data/veh_epa.csv') %>%
  mutate(make = toupper(make), model = toupper(model))

veh <- veh %>%
  left_join(veh_epa)
## Vehicle weight (DPS) -----------
# Load Car Weight Data
Vehicle_wtsDPS <- read_csv("Vehicle_wtsDPS.CSV",
                           col_types = cols(X1 = col_skip()))

veh <- veh %>%
  mutate(make = toupper(make), model = toupper(model)) %>%
  left_join(Vehicle_wtsDPS) %>%
  mutate(wt_cat = case_when(median_wt <= 6000 ~ "6,000 lbs or less",
                            median_wt < 8500 ~ "6,001 - 8,500 lbs",
                            median_wt < 10000 ~ "8,501 - 10,000 lbs")) %>%
  mutate(wt_cat = factor(wt_cat, levels = c("6,000 lbs or less",
                                            "6,001 - 8,500 lbs",
                                            "8,501 - 10,000 lbs")))

rm(Vehicle_wtsDPS)
## Geospatial Data -----------
### Thrive Category -----------
### MPO boundary -----------
### PUMA -----------

# Custom temporal categories -----------
### Seasons -----------
getSeason <- function(input.date){
  numeric.date <- 100*month(input.date)+day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  # This is set to meterological seasons https://www.timeanddate.com/calendar/aboutseasons.html
  cuts <- base::cut(numeric.date, breaks = c(0,301,0531,0831,1130,1231))
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
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

# Make tables into survey objects -----------
