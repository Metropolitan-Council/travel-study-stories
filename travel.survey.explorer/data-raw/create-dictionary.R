pkgload::load_all()
# some work by hand occurred:

# tbi_dict <- read.csv("data-raw/full_dictionary_filled.csv")
tbi_dict <- read.csv("../../metc.tbi.helper/data-raw/final_dictionary_2019.csv")
# some work by hand occurred:

tbi_dict <- tbi_dict %>%
  filter(!category %in% c("Survey metadata", "PII")) %>%
  mutate(dplyr::across(everything(),
    iconv, "latin1", "ASCII",
    sub = ""
  ))

waldo::compare(travel.survey.explorer::tbi_dict, tbi_dict)

usethis::use_data(tbi_dict,
  overwrite = TRUE,
  compress = "xz",
  internal = FALSE
)
