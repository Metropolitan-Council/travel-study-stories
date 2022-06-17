
test_disab_mode_group <-
  create_two_way_table("disability", "mode_group",
                       hh_ids = travel.survey.explorer::tbi_tables$hh$hh_id) %>%
  suppressMessages()

expect_disab_mode_group <-
  tibble::tribble(
    ~disability, ~mode_group, ~total_N, ~total_N_hh, ~group_N, ~group_N_hh, ~expanded_total, ~expanded_total_se, ~estimated_prop, ~estimated_prop_se,   ~units,
    "No",     "Drive",   289489,        7479,   232117,        6931,  11177905.23241,        65296.44509,         0.85956,             0.0019, "people",
    "No",     "Other",   289489,        7479,     9241,        1604,    279866.31962,         8685.14405,         0.02152,            0.00067, "people",
    "No",   "Transit",   289489,        7479,     7868,        1147,    315156.00788,        11620.17391,         0.02424,            0.00088, "people",
    "No",      "Walk",   289489,        7479,    32650,        3532,    1227360.4821,        22012.33471,         0.09438,            0.00162, "people",
    "No",   "Bicycle",   289489,        7479,      136,          47,      3867.83033,          750.80431,          0.0003,            0.00006, "people",
    "Yes",     "Drive",   289489,        7479,     3774,         320,     275845.2889,        10945.71777,         0.68709,            0.01695, "people",
    "Yes",     "Other",   289489,        7479,      190,          53,     23222.42576,         3947.06161,         0.05784,            0.00946, "people",
    "Yes",   "Transit",   289489,        7479,      484,         100,     55438.75346,         6209.66355,         0.13809,              0.014, "people",
    "Yes",      "Walk",   289489,        7479,      720,         118,     46960.99533,         4367.47837,         0.11697,            0.01035, "people",
    "Yes",   "Bicycle",   289489,        7479,        3,           2,               0,                  0,               0,                  0, "people",
    "Prefer not to answer",     "Drive",   289489,        7479,     1767,         126,    215052.48057,        13358.08368,         0.84863,            0.01526, "people",
    "Prefer not to answer",     "Other",   289489,        7479,       46,          15,      2167.53209,          1019.2799,         0.00855,            0.00402, "people",
    "Prefer not to answer",   "Transit",   289489,        7479,      142,          24,     15639.85796,         2607.69307,         0.06172,            0.01021, "people",
    "Prefer not to answer",      "Walk",   289489,        7479,      351,          48,     20552.76436,         2690.98586,          0.0811,            0.01069, "people"
  )


purrr::map(
  names(test_disab_mode_group$table)[3:11],
  function(x) {
    testthat::expect_equal(test_disab_mode_group$table[x], expect_disab_mode_group[x], ignore_attr = TRUE)
  }
) %>%
  suppressMessages()



# Test a numeric variable - trip distance x purpose ------------
testthat::expect_error(
  create_two_way_table(
    "d_purpose_category_imputed", "distance",
    hh_ids = c(1))
) %>%
  suppressMessages()
#
#
# test_student_distancex <-
#   create_two_way_table("student_status", "distance",
#                        hh_ids =  travel.survey.explorer::tbi_tables$hh$hh_id) %>%
#   suppressMessages()
#
#
#
#
# expect_student_distance <- tibble::tribble(
#                                ~student_status,    ~distance, ~total_N, ~total_N_hh, ~group_N, ~group_N_hh, ~expanded_total, ~expanded_total_se, ~estimated_prop, ~estimated_prop_se,   ~units,
#                                           "No",  "1 or less",   351063,        7540,    66084,        5956,   2673201.44333,        33178.60495,         0.21044,            0.00235, "people",
#                                           "No",        "1-3",   351063,        7540,    68090,        6195,   3032274.82081,        35268.44173,         0.23871,            0.00246, "people",
#                                           "No",        "3-5",   351063,        7540,    37096,        5481,   1631223.15963,        26639.47213,         0.12841,            0.00196, "people",
#                                           "No",       "5-10",   351063,        7540,    46386,        5733,   2182857.23362,        31000.18707,         0.17184,            0.00223, "people",
#                                           "No",      "10-20",   351063,        7540,    38393,        5276,   1976009.44055,        29298.49565,         0.15556,            0.00213, "people",
#                                           "No",      "20-50",   351063,        7540,    18523,        3512,   1010846.80142,         20278.7233,         0.07958,            0.00154, "people",
#                                           "No", "50 or more",   351063,        7540,     5429,        1593,    196387.61686,         9184.39402,         0.01546,            0.00072, "people",
#                               "Yes, full-time",  "1 or less",   351063,        7540,    14465,        1471,    741184.15131,        14887.73549,         0.22165,            0.00399, "people",
#                               "Yes, full-time",        "1-3",   351063,        7540,    17823,        1611,   1003961.84981,        17541.95617,         0.30024,            0.00446, "people",
#                               "Yes, full-time",        "3-5",   351063,        7540,     9539,        1373,    495887.81855,        12465.39965,          0.1483,            0.00346, "people",
#                               "Yes, full-time",       "5-10",   351063,        7540,    10501,        1395,    607340.36453,        14722.52238,         0.18163,            0.00395, "people",
#                               "Yes, full-time",      "10-20",   351063,        7540,     6422,        1164,    330367.40621,        11039.94657,          0.0988,            0.00312, "people",
#                               "Yes, full-time",      "20-50",   351063,        7540,     2924,         684,    130977.41745,         6413.91093,         0.03917,            0.00188, "people",
#                               "Yes, full-time", "50 or more",   351063,        7540,     1188,         301,     34194.39551,         3887.20889,         0.01023,            0.00116, "people",
#                               "Yes, part-time",  "1 or less",   351063,        7540,     2198,         228,     89309.58358,           5553.533,         0.19516,            0.01105, "people",
#                               "Yes, part-time",        "1-3",   351063,        7540,     1987,         236,    112480.72814,         5925.52754,         0.24579,            0.01169, "people",
#                               "Yes, part-time",        "3-5",   351063,        7540,      941,         216,     59063.26061,         5208.11666,         0.12906,             0.0105, "people",
#                               "Yes, part-time",       "5-10",   351063,        7540,     1284,         220,     82897.63846,          6141.1348,         0.18115,            0.01195, "people",
#                               "Yes, part-time",      "10-20",   351063,        7540,     1098,         200,     79124.43271,         5689.02606,          0.1729,            0.01125, "people",
#                               "Yes, part-time",      "20-50",   351063,        7540,      531,         129,     31888.41927,         3745.87567,         0.06968,            0.00786, "people",
#                               "Yes, part-time", "50 or more",   351063,        7540,      161,          62,      2864.08896,          687.09956,         0.00626,             0.0015, "people"
#                               )
#
#
#
# purrr::map(
#     names(test_student_distancex$table)[3:11],
#   function(x) {
#     testthat::expect_equal(test_student_distancex$table[x], expect_student_distance[x], ignore_attr = TRUE)
#   }
# ) %>%
#   suppressMessages()
#

