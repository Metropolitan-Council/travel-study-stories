## code to prepare `histogram_breaks` dataset goes here

histogram_breaks <-
  list(
    "num_trips" = list(
      breaks = c(-.01, 0, 2, 4, 6, 8, 10, 100),
      labels = c("0", "1-2", "3-4", "5-6", "7-8", "9-10", "11 or more")
    ),

    "num_adults" = list(breaks = c(0, 1, 2, 3, 4, 10),
                        labels = c("1", "2", "3", "4", "5 or more")),

    "num_kids" = list(breaks = c(-0.1, 0, 1, 2, 3, 4, 10),
                      labels = c("0", "1", "2", "3", "4", "5 or more")),

    "num_students" = list(breaks = c(-0.1, 0, 1, 2, 3, 4, 10),
                          labels = c("0", "1", "2", "3", "4", "5 or more")),

    "num_workers" = list(breaks = c(-0.1, 0, 1, 2, 3, 4, 10),
                         labels = c("0", "1", "2", "3", "4", "5 or more")),

    "co2_gpm" = list(breaks = c(-0.1, 0, 100, 200, 300, 400, 500, 600, 700, 1000),
                     labels = c("0", "1-100", "101-200", "201-300", "301-400", "401-500", "501-600", "601-700", "More than 700")),

    "mpg_city" = list(breaks = c(),
                      labels = c()),

    "mpg_highway" = list(breaks = c(),
                         labels = c()),

    "weight_unladen" = list(breaks = c(),
                            labels = c()),

    "veh_age" = list(breaks = c(),
                     labels = c()),

    # Time will be weird -- need to remember how to cut time
    "depart_time_imputed" = list(breaks = c(),
                                 labels = c()),

    "arrive_time" = list(breaks = c(),
                         labels = c()),

    "duration_imputed" = list(breaks = c(),
                              labels = c()),

    "distance" = list(breaks = c(),
                      labels = c()),

    "speed_mph_imputed" = list(breaks = c(),
                               labels = c())
  )



usethis::use_data(histogram_breaks, overwrite = TRUE)
