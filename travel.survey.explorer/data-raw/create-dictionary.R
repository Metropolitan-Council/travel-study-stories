
# some work by hand occurred:

tbi_dict <- read.csv("data-raw/full_dictionary_filled.csv")
# tbi_dict <- tbi19$dictionary

tbi_dict <- tbi_dict %>%
  filter(!category %in% c("Survey metadata", "PII"))


usethis::use_data(tbi_dict,
  overwrite = TRUE,
  compress = "xz",
  internal = FALSE
)

## Clean up---------------
