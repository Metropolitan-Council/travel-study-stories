### Fix "Change Mode" -----
# change_mode_trips <-
#   trip %>%
#   filter(d_purpose_category_imputed == "Change mode") %>%
#   select(
#     person_id,
#     linked_trip_num,
#     mode_type,
#     mode_type_detailed,
#     d_purpose_category_imputed,
#     d_purpose_imputed
#   ) %>%
#   rename_with(
#     ~ paste0(.x, "_1"),
#     c(
#       mode_type,
#       mode_type_detailed,
#       d_purpose_category_imputed,
#       d_purpose_imputed
#     )
#   ) %>%
#   left_join(
#     trip %>% filter(!d_purpose_category_imputed == "Change mode") %>%
#       select(
#         person_id,
#         linked_trip_num,
#         mode_type,
#         mode_type_detailed,
#         d_purpose_category_imputed,
#         d_purpose_imputed
#       )
#   ) %>%
#   mutate()
#   # key for aggregation of linked trips -- person and trip_linked #
#   group_by(person_id, linked_trip_num) %>%
#   # summarize for each linked trip:
#   summarize(
#     trip_id = first(trip_id),
#     n_links = length(trip_id),
#     across(c(trip_weight), ~ max(., na.rm = T)),
#     across(
#       c(
#         mode_type_id,
#         mode_type_detailed_id,
#         o_purpose_category_imputed_id,
#         d_purpose_category_imputed_id,
#         o_purpose_imputed_id,
#         d_purpose_imputed_id
#       ),
#       ~ suppressWarnings(min(., na.rm = T))
#     )) %>%
#   ungroup()


### Trip Purpose Table ------------
homecats <- c("Spent the night at non-home location", "Home")
nonhomecats <- c(
  "Work",
  "School",
  "Shop",
  "Social/Recreation",
  "Meal",
  "Errand/Other",
  "Work-related",
  "Escort",
  "School-related",
  "Change mode"
)

trip_type <- trip %>%
  select(
    trip_id,
    trip_weight,
    o_purpose_category_imputed,
    d_purpose_category_imputed,
    o_purpose_imputed,
    d_purpose_imputed
  ) %>%
  mutate(
    trip_type = case_when(
      o_purpose_category_imputed %in% homecats |
        d_purpose_category_imputed %in% homecats ~ "home-based",
      TRUE ~ "non-home-based"
    )
  )

#### Home-based trip purpose = NOT home ----------------
homebasedtrips <- trip_type %>%
  filter(trip_type == "home-based") %>%
  mutate(
    purpose_category = case_when(
      # when coming FROM home, the purpose is the destination
      o_purpose_category_imputed %in% homecats ~ as.character(d_purpose_category_imputed),
      # when going TO home, the purpose is the origin:
      d_purpose_category_imputed %in% homecats ~ as.character(o_purpose_category_imputed)
    ),
    purpose = case_when(
      # when coming FROM home, the purpose is the destination
      o_purpose_category_imputed %in% homecats ~ as.character(d_purpose_imputed),
      # when going TO home, the purpose is the origin:
      d_purpose_category_imputed %in% homecats ~ as.character(o_purpose_imputed)
    )
  ) %>%
  select(
    -o_purpose_category_imputed,
    -o_purpose_imputed,
    -d_purpose_category_imputed,
    -d_purpose_imputed
  ) %>%
  mutate(trip_type = "Home-based")

### Trip Weight Adjustment: 50% for each half of the trip ----------------
nonhomebasedtrips_o <-
  trip_type %>%
  filter(trip_type == "non-home-based") %>%
  pivot_longer(
    cols = c("o_purpose_category_imputed", "d_purpose_category_imputed"),
    values_to = "purpose_category"
  ) %>%
  select(-name) %>%
  mutate(trip_weight = 0.5 * trip_weight) %>%
  mutate(trip_type = "Non-Home-based")

nonhomebasedtrips_d <-
  trip_type %>%
  filter(trip_type == "non-home-based") %>%
  pivot_longer(cols = c("o_purpose_imputed", "d_purpose_imputed"),
               values_to = "purpose") %>%
  select(-name) %>%
  mutate(trip_weight = 0.5 * trip_weight) %>%
  mutate(trip_type = "Non-Home-based")



#### Merge home-based and non-homebased trips ------------
trip_purpose <- bind_rows(homebasedtrips, nonhomebasedtrips_o, nonhomebasedtrips_d) %>%
  select(-trip_type) %>%
  select(-"d_purpose_category_imputed",
         -"d_purpose_imputed",
         -"o_purpose_category_imputed",
         -"o_purpose_imputed") %>%
  rename(trip_purpose_weight = trip_weight)

setdiff(names(trip_purpose), names(trip))

rm(homebasedtrips,
   nonhomebasedtrips_o,
   nonhomebasedtrips_d,
   trip_type,
   homecats,
   nonhomecats)
