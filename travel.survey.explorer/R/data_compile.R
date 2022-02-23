# Connect to database -----------

# Translate tables using dictionary -----------

# Replace missing with NA -----------

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
