## code to prepare `histogram_breaks` dataset goes here
#
# hist_breaks <- list(
#   "0 to 1" = 0,
#   "1 to 3" = 1,
#   "3 to 5" = 3,
#   "5 to 10"= 5,
#   "10 to 20" = 10,
#   "20 to 30" = 20,
#   "30 to 45" = 35,
#   "45 to 60" = 45,
#   "60 to 180" = 60
# )
#
#
# hist_breaks_num_trips <- list(
#   "0" = -0.01,
#   "1-2" = 0,
#   "3-4" = 2,
#   "5-6" = 4,
#   "7-8" = 6,
#   "9-10" = 8,
#   "11-12" = 10,
#   "13-14" =12,
#   "14-16" = 14,
#   "17-18" = 16,
#   "19-20" = 18,
#   "20-100" = 20
# )


hist_breaks <- c(0, 1, 3, 5, 10, 20, 30, 45, 60, 180)
hist_breaks_labels <- c(
  "0 to 1", "1 to 3", "3 to 5",
  "5 to 10", "10 to 20", "20 to 30",
  "30 to 45", "45 to 60", "60 to 180"
)
hist_breaks_num_trips <- c(-.01, 0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 100)
hist_breaks_num_trips_labels <- c(
  "0", "1-2",
  "3-4", "5-6", "7-8",
  "9-10", "11-12", "13-14",
  "14-16", "17-18", "19-20",
  "20-100"
)

histogram_breaks <- list(
  trip_breaks = hist_breaks_num_trips,
  trip_breaks_labs = hist_breaks_num_trips_labels,
  other_breaks = hist_breaks,
  other_breaks_labs = hist_breaks_labels
)

usethis::use_data(histogram_breaks, overwrite = TRUE)
