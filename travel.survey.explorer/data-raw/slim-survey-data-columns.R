# Trip ----------------
# trip <- trip %>%
#   select(
#     trip_id, trip_weight,
#     person_id, hh_id, trip_num, day_num,
#     travel_date, depart_time_imputed, arrive_time,
#     duration_imputed, distance, speed_mph_imputed,
#     leg_num, linked_trip_num,
#     num_travelers,
#     d_purpose_category_imputed, d_purpose_imputed,
#     o_purpose_category_imputed, o_purpose_imputed,
#     vehicle_driver, mode_group, bike_freq,
#     mode_type, mode_type_detailed, veh_id
#   )

pkgload::load_all()
library(bit64)

#### Select only relevant columns--------
# Day:
day <- day %>%
  select(person_id, day_num, hh_id, travel_date, num_trips, day_weight)

hh <- hh %>%
  select(
    hh_id, hh_weight,
    residence_type, rent_own, rent_cost,
    num_adults, num_kids, num_students, num_workers, num_people, num_vehicles,
    income_broad, income_detailed
  )

# per <- per %>%
#   select(
#     person_id, person_weight, hh_id,
#     age, gender, disability, education, employment_status, student_status,
#     starts_with("ethnicity"),
#     job_type, license, num_jobs
#   )

# veh <- veh %>%
#   select(hh_id, vehicle_num,
#          # vehicle_name,
#          year, make, model, fuel, veh_id)
