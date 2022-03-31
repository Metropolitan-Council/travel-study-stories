## code to prepare `histogram_breaks` dataset goes here

histogram_breaks <-
  list(
    "num_trips" = list(
      breaks = c(-.01, 0, 2, 4, 6, 8, 10, 100),
      labels = c("0", "1-2", "3-4", "5-6", "7-8", "9-10", "More than 10")
    ),

    "num_adults" = list(breaks = c(),
                        labels = c()),

    "num_kids" = list(breaks = c(),
                      labels = c()),

    "num_students" = list(breaks = c(),
                          labels = c()),

    "num_workers" = list(breaks = c(),
                         labels = c()),

    "co2_gpm" = list(breaks = c(),
                     labels = c()),

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
