### Toolbox ----------
library(DBI)
library(sf)
library(dplyr)

### List of Counties ----------
county_list <-
  c(
  "Hennepin MN",
  "Ramsey MN",
  "Dakota MN",
  "Anoka MN",
  "Washington MN",
  "Wright MN",
  "Scott MN"
)

### List of Cities ----------
city_list <- c("Minneapolis", "St. Paul")

### Create SF objects from TBI tables ------------
##### households -----
hh_sf <- hh %>%
  select(hh_id, home_lon, home_lat) %>%
  na.omit() %>%
  st_as_sf(
    coords = c("home_lon", "home_lat"),
    crs = 4326
  )
##### trip origins/destinations -----
trip_d_sf <- trip %>%
  select(trip_id, d_lon, d_lat) %>%
  na.omit() %>%
  st_as_sf(coords = c("d_lon", "d_lat"), crs = 4326)

trip_o_sf <- trip %>%
  select(trip_id, o_lon, o_lat) %>%
  na.omit() %>%
  st_as_sf(coords = c("o_lon", "o_lat"), crs = 4326)

##### work locations -----
work_sf <- per %>%
  select(person_id, work_lon, work_lat) %>%
  na.omit() %>%
  st_as_sf(
    coords = c("work_lon", "work_lat"),
    crs = 4326
  )

##### school locations -----
school_sf <- per %>%
  select(person_id, school_lon, school_lat) %>%
  na.omit() %>%
  st_as_sf(
    coords = c("school_lon", "school_lat"),
    crs = 4326
  )



### Get Shapefiles -------------
db <- DBI::dbConnect(odbc::odbc(), "GISLibrary")

##### MPO: ----
mpo_sf <- DBI::dbGetQuery(
  db,
  "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.MetropolitanPlanningOrganizationArea;"
) %>%
  st_as_sf(wkt = "geometry", crs = 26915) %>%
  st_transform(crs = 4326) %>%
  st_make_valid()

##### Counties: ----
# Minnesota:
mn_cty_sf <- DBI::dbGetQuery(db,
                             "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.MNCounties;") %>%
  st_as_sf(wkt = "geometry", crs = 26915) %>%
  st_transform(crs = 4326) %>%
  rename(county = CO_NAME) %>%
  mutate(county = paste(county, "MN")) %>%
  select(county)%>%
  st_make_valid()

# Wisconsin:
wi_cty_sf <- DBI::dbGetQuery(db,
                             "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.WICounties;") %>%
  st_as_sf(wkt = "geometry", crs = 26915) %>%
  st_transform(crs = 4326) %>%
  rename(county = CO_NAME) %>%
  mutate(county = paste(stringr::str_to_title(county), "WI")) %>%
  select(county)%>%
  st_make_valid()

# Both states:
cty_sf <- rbind(mn_cty_sf, wi_cty_sf)%>%
  st_make_valid()


##### Cities: ----
ctu_sf <- DBI::dbGetQuery(db,
                          "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.CTUs;") %>%
  st_as_sf(wkt = "geometry", crs = 26915) %>%
  select(CTU_NAME) %>%
  rename(community_name = CTU_NAME) %>%
  st_transform(crs = 4326)%>%
  st_make_valid()

##### Thrive: ----
thrive2040 <- dbGetQuery(
  db,
  "SELECT *, SHAPE.STAsText() as geometry FROM GISLibrary.DBO.THRIVEMSP2040COMMUNITYDESIGNATION;"
) %>%
  sst_as_sf(wkt = "geometry", crs = 26915) %>%
  select(COMDESNAME) %>%
  rename(thrive_category = COMDESNAME) %>%
  st_transform(crs = 4326) %>%
  st_make_valid()


### Household geographic Info ----------
##### MPO: ----
hh_mpo <-
  st_join(hh_sf, mpo_sf %>% select(OBJECTID), join = st_within) %>%
  st_drop_geometry() %>%
  rename(hh_in_mpo = OBJECTID) %>%
  mutate(hh_in_mpo = case_when(hh_in_mpo == 1 ~ "Household in Twin Cities region", TRUE ~ "Household outside Twin Cities region"))

##### County: ----
hh_cty <-
  st_join(hh_sf, cty_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(hh_county = case_when(
    county %in% county_list ~ county
  )) %>%
  select(-county)

##### City: ----
hh_ctu <-
  st_join(hh_sf, ctu_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(hh_city = case_when(
    community_name %in% city_list ~ community_name
  )) %>%
  select(-community_name)

hh <- hh %>%
  left_join(hh_mpo) %>%
  left_join(hh_cty) %>%
  left_join(hh_ctu) %>%
  mutate(across(c(hh_in_mpo, hh_county, hh_city), ~ as.factor(.)))

### Append Geographic Info to Trip Origin & Destination ----------
##### MPO: ----
trip_o_mpo <-
  st_join(trip_o_sf, mpo_sf %>% select(OBJECTID), join = st_within) %>%
  st_drop_geometry() %>%
  rename(trip_o_in_mpo = OBJECTID) %>%
  mutate(trip_o_in_mpo = case_when(trip_o_in_mpo == 1 ~ "Trip begins in Twin Cities region", TRUE ~ "Trip begins outside Twin Cities region"))

trip_d_mpo <-
  st_join(trip_d_sf, mpo_sf %>% select(OBJECTID), join = st_within) %>%
  st_drop_geometry() %>%
  rename(trip_d_in_mpo = OBJECTID) %>%
  mutate(trip_d_in_mpo = case_when(trip_d_in_mpo == 1 ~ "Trip ends in Twin Cities region", TRUE ~ "Trip ends outside Twin Cities region"))

##### County: ----
trip_o_cty <-
  st_join(trip_o_sf, cty_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(trip_o_county = county) %>%
  mutate(trip_o_county = case_when(
    trip_o_county %in% county_list ~ trip_o_county
  ))

trip_d_cty <-
  st_join(trip_d_sf, cty_sf, join = st_within) %>%
  st_drop_geometry() %>%
  rename(trip_d_county = county) %>%
  mutate(trip_d_county = case_when(
    trip_d_county %in% county_list ~ trip_d_county
  ))

##### City: ----
trip_o_ctu <-
  st_join(trip_o_sf, ctu_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(trip_o_city = case_when(
    community_name %in% city_list ~ community_name
  )) %>%
  select(-community_name)

trip_d_ctu <-
  st_join(trip_d_sf, ctu_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(trip_d_city = case_when(
    community_name %in% city_list ~ community_name
  )) %>%
  select(-community_name)

trip_test <- trip %>%
  left_join(trip_o_mpo) %>%
  left_join(trip_d_mpo) %>%
  left_join(trip_o_cty) %>%
  left_join(trip_d_cty) %>%
  left_join(trip_o_ctu) %>%
  left_join(trip_d_ctu) %>%
  mutate(across(c(trip_o_in_mpo, trip_o_county, trip_o_city,
                  trip_d_in_mpo, trip_d_county, trip_d_city), ~ as.factor(.)))

summary(trip_test)

### Append Geographic Info to Work ----
##### MPO: ----
work_mpo <-
  st_join(work_sf, mpo_sf %>% select(OBJECTID), join = st_within) %>%
  st_drop_geometry() %>%
  rename(work_in_mpo = OBJECTID) %>%
  mutate(work_in_mpo = case_when(work_in_mpo == 1 ~ "Workplace in Twin Cities region", TRUE ~ "Workplace outside Twin Cities region"))

##### County: ----
work_cty <-
  st_join(work_sf, cty_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(work_county = case_when(
    county %in% county_list ~ county
  )) %>%
  select(-county)

##### City: ----
work_ctu <-
  st_join(work_sf, ctu_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(work_city = case_when(
    community_name %in% city_list ~ community_name
  )) %>%
  select(-community_name)

hh <- hh %>%
  left_join(work_mpo) %>%
  left_join(work_cty) %>%
  left_join(work_ctu) %>%
  mutate(across(c(work_in_mpo, work_county, work_city), ~ as.factor(.)))

per <- per %>%
  left_join(work_mpo) %>%
  left_join(work_cty) %>%
  left_join(work_ctu) %>%
  mutate(across(c(work_in_mpo, work_county, work_city), ~ as.factor(.)))

### Append Geographic Info to School ----
##### MPO: ----
school_mpo <-
  st_join(school_sf, mpo_sf %>% select(OBJECTID), join = st_within) %>%
  st_drop_geometry() %>%
  rename(school_in_mpo = OBJECTID) %>%
  mutate(school_in_mpo = case_when(school_in_mpo == 1 ~ "School in Twin Cities region", TRUE ~ "School outside Twin Cities region"))

##### County: ----
school_cty <-
  st_join(school_sf, cty_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(school_county = case_when(
    county %in% county_list ~ county
  )) %>%
  select(-county)

##### City: ----
school_ctu <-
  st_join(school_sf, ctu_sf, join = st_within) %>%
  st_drop_geometry() %>%
  mutate(school_city = case_when(
    community_name %in% city_list ~ community_name
  )) %>%
  select(-community_name)

per <- per %>%
  left_join(school_mpo) %>%
  left_join(school_cty) %>%
  left_join(school_ctu) %>%
  mutate(across(c(school_in_mpo, school_county, school_city), ~ as.factor(.)))

