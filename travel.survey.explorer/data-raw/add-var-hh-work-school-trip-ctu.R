library(DBI)
library(sf)
library(tidyverse)

# Download Geographies Needed
db <- DBI::dbConnect(odbc::odbc(), "GISLibrary")

# CTU (Cities-Townships-Unincorporated)
ctu_sf <- DBI::dbGetQuery(
  db,
  "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.CTUs;"
) %>%
  st_as_sf(wkt = "geometry", crs = "+init=epsg:26915") %>%
  st_transform(crs = "+init=epsg:26915 +proj=longlat +datum=WGS84")
ctu_sf <- st_transform(ctu_sf, crs = 4326) %>%
  st_make_valid()

# Household locations
hh_sf <- hh %>%
  select(hh_id, home_lon, home_lat) %>%
  na.omit() %>%
  st_as_sf(
    coords = c("home_lon", "home_lat"),
    crs = 4326
  )
#
# trip_o_ctu <-
#   st_join(trip_o_sf, ctu_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
#   as.data.frame() %>%
#   rename(trip_o_ctu = CTU_NAME) %>%
#   select(trip_id, trip_o_ctu)
#
# trip_d_ctu <-
#   st_join(trip_d_sf, ctu_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
#   as.data.frame() %>%
#   rename(trip_d_ctu = CTU_NAME) %>%
#   select(trip_id, trip_d_ctu)
#
# trip <- merge(trip, trip_o_ctu, by = "trip_id", all.x = T)
# trip <- merge(trip, trip_d_ctu, by = "trip_id", all.x = T)

hh_ctu <-
  st_join(hh_sf, ctu_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
  as.data.frame() %>%
  rename(hh_ctu = CTU_NAME) %>%
  select(hh_id, hh_ctu)
hh <- merge(hh, hh_ctu, by = "hh_id", all.x = T)

# # Tally by CTU:
hh_tally <-
  hh %>%
  group_by(hh_ctu) %>%
  filter(!is.na(hh_ctu)) %>%
  summarize(
    n_hh = length(unique(hh_id)),
    mean_wt = mean(hh_weight),
    sd_wt = sd(hh_weight)
  ) %>%
  mutate(cv = sd_wt / mean_wt) %>%
  filter(n_hh >= 100)

# Get rid of cities with < 100 samples:
hh <-
  hh %>%
  mutate(hh_ctu = ifelse(hh_ctu %in% hh_tally$hh_ctu, hh_ctu, NA))

# Append to dictionary
new_entry <- data.frame(
  which_table = "hh", variable = "hh_ctu", wt_field = "hh_weight", category = "Demographics", variable_label = "Household City/Township",
  value = unique(hh$hh_ctu)
)

tbi_dict <- bind_rows(tbi_dict, new_entry)

usethis::use_data(tbi_dict,
  overwrite = TRUE,
  compress = "xz",
  internal = FALSE
)

message("New column created in hh table: hh_ctu")


# work_ctu <-
#   st_join(work_sf, ctu_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
#   as.data.frame() %>%
#   rename(work_ctu = CTU_NAME) %>%
#   select(person_id, work_ctu)
#
# school_ctu <-
#   st_join(school_sf, ctu_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
#   as.data.frame() %>%
#   rename(school_ctu = CTU_NAME) %>%
#   select(person_id, school_ctu)


# per <- merge(per, work_ctu, by = "person_id", all.x = T)
# per <- merge(per, school_ctu, by = "person_id", all.x = T)
