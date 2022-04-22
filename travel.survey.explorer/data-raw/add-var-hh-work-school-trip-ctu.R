library(DBI)
library(sf)
library(tidyverse)

# Download Geographies Needed
db <- DBI::dbConnect(odbc::odbc(), "GISLibrary")

# CTU (Cities-Townships-Unincorporated)
ctu_sf <- DBI::dbGetQuery(db,
                          "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.CTUs;") %>%
  st_as_sf(wkt = "geometry", crs = "+init=epsg:26915") %>%
  st_transform(crs = "+init=epsg:26915 +proj=longlat +datum=WGS84")
ctu_sf <- st_transform(ctu_sf, crs = 4326)


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
