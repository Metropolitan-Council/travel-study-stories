### Toolbox ----------
library(DBI)
library(sf)
library(tidyverse)


db <- DBI::dbConnect(odbc::odbc(), "GISLibrary")

### Get MPO shapefile -------------
mpo_sf <- DBI::dbGetQuery(
  db,
  "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.MetropolitanPlanningOrganizationArea;"
) %>%
  st_as_sf(wkt = "geometry", crs = "+init=epsg:26915") %>%
  st_transform(crs = "+init=epsg:26915 +proj=longlat +datum=WGS84")
mpo_sf <- st_transform(mpo_sf, crs = 4326)

### Create spatial features object from hh table ------------
hh_sf <- hh %>%
  select(hh_id, home_lon, home_lat) %>%
  na.omit() %>%
  st_as_sf(
    coords = c("home_lon", "home_lat"),
    crs = 4326
  )

### Trim hh: households in MPO ----------
hh_ids <-
  st_join(hh_sf, mpo_sf, join = st_within) %>% # this takes TIME, especially with a lot of data.
  as.data.frame() %>%
  filter(OBJECTID == 1) %>%
  select(hh_id)

hh[, hh_in_mpo := ifelse(hh_id %in% hh_ids$hh_id, "in_mpo", "outside_mpo")]



### Connect to Database ---------
db <- dbConnect(odbc::odbc(), "GISLibrary") # connect to the Met Council GIS Library for handy Shapefiles

# CTU Shapefile (Cities-Townships-Unincorporated) -----
ctu_sf <- DBI::dbGetQuery(db,
                          "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.CTUs;") %>%
  st_as_sf(wkt = "geometry", crs = "+init=epsg:26915") %>%
  st_transform(crs = "+init=epsg:26915 +proj=longlat +datum=WGS84")
ctu_sf <- st_transform(ctu_sf, crs = 4326)

# County Shapefile ----
# Minnesota:
mn_cty_sf <- DBI::dbGetQuery(db,
                             "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.MNCounties;") %>%
  st_as_sf(wkt = "geometry", crs = "+init=epsg:26915") %>%
  st_transform(crs = "+init=epsg:26915 +proj=longlat +datum=WGS84") %>%
  select(CO_NAME) %>%
  mutate(CO_NAME = toupper(CO_NAME)) %>%
  mutate(State = "MN")

# Wisconsin:
wi_cty_sf <- DBI::dbGetQuery(db,
                             "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.WICounties;") %>%
  st_as_sf(wkt = "geometry", crs = "+init=epsg:26915") %>%
  st_transform(crs = "+init=epsg:26915 +proj=longlat +datum=WGS84") %>%
  select(CO_NAME) %>%
  mutate(State = "WI")

# Both states:
cty_sf <- rbind(mn_cty_sf, wi_cty_sf) %>%
  st_transform(cty_sf, crs = 4326)

# Join to County ---------
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

