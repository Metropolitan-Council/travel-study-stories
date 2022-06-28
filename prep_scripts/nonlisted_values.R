# This script cross-checks for variables in variables.xlsx that are not compiled in variables_values.xlsx

# variables_values::Field = raw column names from source household survey data
# variables::Variables = column names in processed household survey data for this app. Equivalent to variables_values::Label

library(data.table)
library(openxlsx)
library(tidyverse)

this.dir <- getwd()
dir <- "path/to/travel-study-stories/shiny"
setwd(dir)

lookup_variables <- read.xlsx("variables.xlsx") %>% as.data.table()
lookup_values <- read.xlsx("variables_values.xlsx") %>% as.data.table()

fields.with.values <- lookup_values[, .(Label), by = Label][["Label"]]
fields <- lookup_variables[["Variables"]]

nonlisted <- sort(setdiff(fields, fields.with.values))

lookup_variables[Variables %in% nonlisted]

setwd(this.dir)
