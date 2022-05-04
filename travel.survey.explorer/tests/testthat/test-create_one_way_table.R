
test_bike_freq <-
  create_one_way_table("bike_freq")

expect_bike_freq <-
  tibble::tribble(
    ~bike_freq, ~total_N, ~total_N_hh, ~group_N, ~group_N_hh, ~expanded_total, ~expanded_total_se, ~estimated_prop, ~estimated_prop_se,
    "6-7 days a week", 13032, 7816, 163, 149, 24321.80626, 3506.59353, 0.00799, 0.00115,
    "5 days a week", 13032, 7816, 191, 180, 32412.15513, 4323.06254, 0.01064, 0.00142,
    "4 days a week", 13032, 7816, 196, 187, 31180.30774, 4108.29054, 0.01024, 0.00135,
    "2-3 days a week", 13032, 7816, 672, 601, 127489.35502, 8600.60126, 0.04187, 0.00279,
    "1 day a week", 13032, 7816, 559, 500, 129459.17456, 9035.96291, 0.04251, 0.00292,
    "1-3 days a month", 13032, 7816, 1031, 881, 242458.03179, 12349.76871, 0.07962, 0.00394,
    "Less than monthly", 13032, 7816, 3398, 2589, 784361.78589, 20993.22219, 0.25757, 0.00626,
    "Never", 13032, 7816, 6822, 4684, 1673529.56654, 29007.60499, 0.54956, 0.00713
  )

purrr::map(
  names(test_bike_freq)[2:9],
  function(x) {
    testthat::expect_equal(test_bike_freq[x], expect_bike_freq[x])
  }
) %>%
  suppressMessages()



test_edu <- create_one_way_table("education")
expect_edu <-
  # datapasta::tribble_paste(test_edu)
  tibble::tribble(
    ~education, ~total_N, ~total_N_hh, ~group_N, ~group_N_hh, ~expanded_total, ~expanded_total_se, ~estimated_prop, ~estimated_prop_se,
    "Less than high school", 12980, 7784, 174, 161, 66185.26582, 6598.4337, 0.022, 0.00218,
    "High school graduate/GED", 12980, 7784, 1302, 1095, 360140.32488, 14908.51073, 0.11969, 0.00474,
    "Some college", 12980, 7784, 1601, 1423, 407267.93642, 15858.41047, 0.13536, 0.00501,
    "Vocational/technical training", 12980, 7784, 893, 803, 212505.3997, 11185.741, 0.07063, 0.00364,
    "Associate degree", 12980, 7784, 893, 823, 239986.29926, 12319.77649, 0.07976, 0.00398,
    "Bachelor's degree", 12980, 7784, 4817, 3854, 1042932.57502, 23560.60663, 0.34662, 0.00682,
    "Graduate/post-graduate degree", 12980, 7784, 3205, 2598, 635388.62677, 18692.87181, 0.21117, 0.00579,
    "Prefer not to answer", 12980, 7784, 95, 75, 44425.92788, 5707.51676, 0.01477, 0.00189
  )

purrr::map(
  names(test_edu)[2:9],
  function(x) {
    testthat::expect_equal(test_edu[x], expect_edu[x])
  }
) %>%
  suppressMessages()

# Test a numeric variable - trip distance ------------
test_trip_distance <-
  create_one_way_table("distance")

expect_trip_distance <-
  tibble::tribble(
    ~bike_freq, ~total_N, ~total_N_hh, ~group_N, ~group_N_hh, ~expanded_total, ~expanded_total_se, ~estimated_prop, ~estimated_prop_se,
    "6-7 days a week", 13032, 7816, 163, 149, 24321.80626, 3506.59353, 0.00799, 0.00115,
    "5 days a week", 13032, 7816, 191, 180, 32412.15513, 4323.06254, 0.01064, 0.00142,
    "4 days a week", 13032, 7816, 196, 187, 31180.30774, 4108.29054, 0.01024, 0.00135,
    "2-3 days a week", 13032, 7816, 672, 601, 127489.35502, 8600.60126, 0.04187, 0.00279,
    "1 day a week", 13032, 7816, 559, 500, 129459.17456, 9035.96291, 0.04251, 0.00292,
    "1-3 days a month", 13032, 7816, 1031, 881, 242458.03179, 12349.76871, 0.07962, 0.00394,
    "Less than monthly", 13032, 7816, 3398, 2589, 784361.78589, 20993.22219, 0.25757, 0.00626,
    "Never", 13032, 7816, 6822, 4684, 1673529.56654, 29007.60499, 0.54956, 0.00713
  )

purrr::map(
  names(test_trip_distance)[2:9],
  function(x) {
    testthat::expect_equal(test_bike_freq[x], expect_trip_distance[x])
  }
) %>%
  suppressMessages()

# Test a time variable - departure time ------------
test_trip_departure_time <-
  create_one_way_table("departure_time_imputed")

expect_trip_departure_time <-
  tibble::tribble(
    ~bike_freq, ~total_N, ~total_N_hh, ~group_N, ~group_N_hh, ~expanded_total, ~expanded_total_se, ~estimated_prop, ~estimated_prop_se,
    "6-7 days a week", 13032, 7816, 163, 149, 24321.80626, 3506.59353, 0.00799, 0.00115,
    "5 days a week", 13032, 7816, 191, 180, 32412.15513, 4323.06254, 0.01064, 0.00142,
    "4 days a week", 13032, 7816, 196, 187, 31180.30774, 4108.29054, 0.01024, 0.00135,
    "2-3 days a week", 13032, 7816, 672, 601, 127489.35502, 8600.60126, 0.04187, 0.00279,
    "1 day a week", 13032, 7816, 559, 500, 129459.17456, 9035.96291, 0.04251, 0.00292,
    "1-3 days a month", 13032, 7816, 1031, 881, 242458.03179, 12349.76871, 0.07962, 0.00394,
    "Less than monthly", 13032, 7816, 3398, 2589, 784361.78589, 20993.22219, 0.25757, 0.00626,
    "Never", 13032, 7816, 6822, 4684, 1673529.56654, 29007.60499, 0.54956, 0.00713
  )

purrr::map(
  names(test_trip_departure_time)[2:9],
  function(x) {
    testthat::expect_equal(test_trip_departure_time[x], expect_trip_departure_time[x])
  }
) %>%
  suppressMessages()
