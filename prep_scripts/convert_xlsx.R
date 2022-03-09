library(openxlsx)
library(data.table)
library(magrittr)

# wrkdir <- "path/to/travel-study-stories/shiny" # local
wrkdir <- "J:/Projects/Surveys/HHTravel/Survey2017/Data/travel_crosstab/for_shiny_app"
outdir <- "path/to/travel-study-stories/data"

lookup_variables <- read.xlsx(file.path(wrkdir, "variables.xlsx")) %>% as.data.table()
# lookup_values <- read.xlsx(file.path(wrkdir, 'variables_values.xlsx')) %>% as.data.table

fwrite(lookup_variables, file.path(outdir, "variables.csv"))
# fwrite(lookup_values, file.path(outdir, "variables_values.csv"))
