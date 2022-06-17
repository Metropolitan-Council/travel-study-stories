
test_bike_freq <-
  create_one_way_table("bike_freq",
    hh_ids = travel.survey.explorer::tbi_tables$hh$hh_id
  )

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
  names(test_bike_freq$table)[2:9],
  function(x) {
    testthat::expect_equal(test_bike_freq$table[x], expect_bike_freq[x])
  }
) %>%
  suppressMessages()

#
# test_edu <- create_one_way_table("education",
#                                  hh_ids = travel.survey.explorer::tbi_tables$hh$hh_id)
# expect_edu <-
#   tibble::tribble(
#     ~education, ~total_N, ~total_N_hh, ~group_N, ~group_N_hh, ~expanded_total, ~expanded_total_se, ~estimated_prop, ~estimated_prop_se,
#     "Less than high school", 12980, 7784, 174, 161, 66185.26582, 6598.4337, 0.022, 0.00218,
#     "High school graduate/GED", 12980, 7784, 1302, 1095, 360140.32488, 14908.51073, 0.11969, 0.00474,
#     "Some college", 12980, 7784, 1601, 1423, 407267.93642, 15858.41047, 0.13536, 0.00501,
#     "Vocational/technical training", 12980, 7784, 893, 803, 212505.3997, 11185.741, 0.07063, 0.00364,
#     "Associate degree", 12980, 7784, 893, 823, 239986.29926, 12319.77649, 0.07976, 0.00398,
#     "Bachelor's degree", 12980, 7784, 4817, 3854, 1042932.57502, 23560.60663, 0.34662, 0.00682,
#     "Graduate/post-graduate degree", 12980, 7784, 3205, 2598, 635388.62677, 18692.87181, 0.21117, 0.00579,
#     "Prefer not to answer", 12980, 7784, 95, 75, 44425.92788, 5707.51676, 0.01477, 0.00189
#   )
#
# purrr::map(
#   names(test_edu$table)[2:9],
#   function(x) {
#     testthat::expect_equal(test_edu$table[x], expect_edu[x])
#   }
# ) %>%
#   suppressMessages()
#
# # trip distance ------------
# test_trip_distance <-
#   create_one_way_table("distance",
#                        hh_ids = travel.survey.explorer::tbi_tables$hh$hh_id)
# test_trip_distance$table
#
# expect_trip_distance <-
#   tibble::tribble(
#     ~distance, ~total_N, ~total_N_hh, ~group_N, ~group_N_hh, ~expanded_total, ~expanded_total_se, ~estimated_prop, ~estimated_prop_se,  ~units,
#     "1 or less",   351063,        7540,    82747,        6187,   3503695.17822,        36609.85295,         0.21229,              0.002, "trips",
#     "1-3",   351063,        7540,    87900,        6428,   4148717.39876,        39582.38681,         0.25137,            0.00212, "trips",
#     "3-5",   351063,        7540,    47576,        5719,   2186174.23879,        29779.99465,         0.13246,            0.00169, "trips",
#     "5-10",   351063,        7540,    58171,        5946,   2873095.23661,        34736.25419,         0.17408,            0.00192, "trips",
#     "10-20",   351063,        7540,    45913,        5461,   2385501.27947,        31747.26103,         0.14454,            0.00179, "trips",
#     "20-50",   351063,        7540,    21978,        3634,   1173712.63814,        21573.93686,         0.07112,            0.00127, "trips",
#     "50 or more",   351063,        7540,     6778,        1687,    233446.10133,         9994.67721,         0.01414,             0.0006, "trips"
#   )
#
# purrr::map(
#   names(test_trip_distance$table)[2:9],
#   function(x) {
#     testthat::expect_equal(test_trip_distance$table[x], expect_trip_distance[x])
#   }
# ) %>%
#   suppressMessages()
#
# # time variable - departure time ------------
# testthat::expect_error(  create_one_way_table("departure_time_imputed",
#                                               hh_ids = travel.survey.explorer::tbi_tables$hh$hh_id)
# )
#
# test_trip_departure_time <-
#   create_one_way_table("depart_time_imputed",
#                        hh_ids = travel.survey.explorer::tbi_tables$hh$hh_id)
#
# expect_trip_departure_time <-
#   tibble::tribble(
#     ~depart_time_imputed, ~total_N, ~total_N_hh, ~group_N, ~group_N_hh, ~expanded_total, ~expanded_total_se, ~estimated_prop, ~estimated_prop_se,  ~units,
#     "12-6 AM",   351179,        7548,    16164,        3627,   1038105.40238,        20994.51568,         0.06277,            0.00123, "trips",
#     "6-9 AM",   351179,        7548,    59919,        6256,   3106445.45454,        35244.52652,         0.18784,            0.00194, "trips",
#     "9 AM-12 PM",   351179,        7548,    68930,        6437,   2817112.94715,        35173.89547,         0.17034,            0.00194, "trips",
#     "12-3 PM",   351179,        7548,    74389,        6637,   3515640.34958,        39067.44382,         0.21258,             0.0021, "trips",
#     "3-6 PM",   351179,        7548,    88020,        6410,   4226898.29246,        39092.47791,         0.25559,            0.00211, "trips",
#     "6-9 PM",   351179,        7548,    37022,        5048,   1604479.49264,        23190.87684,         0.09702,            0.00136, "trips",
#     "9 PM-12 AM",   351179,        7548,     6735,        2186,    229353.59966,         9073.38894,         0.01387,            0.00055, "trips"
#   )
#
# purrr::map(
#   names(test_trip_departure_time$table)[2:9],
#   function(x) {
#     testthat::expect_equal(test_trip_departure_time$table[x],
#                            expect_trip_departure_time[x])
#   }
# ) %>%
#   suppressMessages()
