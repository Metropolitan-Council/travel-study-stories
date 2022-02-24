# The R version of travel_crosstab.py

library(data.table)
library(tidyverse)


# create_cross_tab_with_weights
cross_tab <- function(table, var1, var2,
                      wt_field, type) {
  # z <- 1.96 # 95% CI

  print("reading in data")

  cols <- c(var1, var2)

  if (type == "dimension") {
    setkeyv(table, cols)
    table[table == ""] <- NA # change "" values to NA

    for (missing in missing_codes) {
      #
      table <- subset(table, get(var1) != missing)
      table <- subset(table, get(var2) != missing)
    }

    table <- na.omit(table, cols = cols) # filter out rows with NA in var1 or var2
    table <- table[!is.na(get(wt_field))] # filter out rows with NA weight

    raw <- table[, .(sample_count = .N), by = cols] # get sample count
    N_hh <- table[, .(hhid = uniqueN(hhid)), by = var1] # get unique household sample count


    expanded <- table[, lapply(.SD, sum), .SDcols = wt_field, by = cols] # total up weight field by var1 and var2
    expanded_tot <- expanded[, lapply(.SD, sum), .SDcols = wt_field, by = var1] # total up weight field by var1

    setnames(expanded, wt_field, "estimate")
    expanded <- merge(expanded, expanded_tot, by = var1) # merge all same counts
    expanded[, share := estimate / get(eval(wt_field))] # calculate share = estimate/weight

    expanded <- merge(expanded, N_hh, by = var1)
    expanded[, p_col := p_MOE] # set MOE column
    expanded[
      , ("in") := (p_col * (1 - p_col)) / hhid
    ][ # in = (MOE column * (1 - MOE column)) / n_households
      , MOE := z * sqrt(get("in"))
    ][ # MOE = 1.645 * sqrt(in)
      , N_HH := hhid
    ] # re-name n_households column

    expanded$estMOE <- expanded$MOE * expanded[[wt_field]] # MOE * weight

    crosstab <- merge(raw, expanded, by = cols)


    crosstab <- dcast.data.table( # long to wide transformation
      crosstab,
      get(eval(var1)) ~ get(eval(var2)), # id var = var1, measure var = var2
      value.var = c(
        "sample_count", # columns to be filled
        "estimate", "estMOE",
        "share", "MOE", "N_HH"
      )
    )
  } else if (type == "fact") {
    # calculate mean

    cols <- c(var1, var2, "hhid", wt_field) # var1, var2, hhid and weight

    var_weights <- table[, cols, with = FALSE] # make table with columns var1, var2, hhid, weight


    for (missing in missing_codes) { # filter out missing var1 or var2
      var_weights <- subset(var_weights, get(var1) != missing)
      var_weights <- subset(var_weights, get(var2) != missing)
    }

    var_weights <- na.omit(var_weights) # filter out NA weights

    raw <- var_weights[, .(sample_count = .N), by = var1] # calculate n responses
    N_hh <- var_weights[, .(hhid = uniqueN(hhid)), by = var1] # calculate n households

    var_weights <- var_weights[eval(parse(text = var2)) > min_float] # filter var2 > 0
    var_weights <- var_weights[eval(parse(text = var2)) < max_float] # filter var2 < 200

    var_weights[, weighted_total := # weighted total = weight * var2
                  get(eval((wt_field))) * get(eval((var2)))]

    expanded <- var_weights[
      , lapply(.SD, sum),
      .SDcols = "weighted_total", by = var1
    ][ # sum weighted_total, grouped by var1
      order(get(eval(var1)))
    ] # reorder var1

    expanded_tot <- var_weights[
      , lapply(.SD, sum),
      .SDcols = wt_field, by = var1
    ] # sum weights, grouped by var1

    expanded_moe <- var_weights[
      , lapply(.SD, function(x) z * sd(x) / sqrt(length(x))), # 1.645 * sd(var2) / sqrt(length(var2)), grouped by var1
      .SDcols = var2, by = var1
    ][
      order(get(eval(var1)))
    ] # reorder var1

    setnames(expanded_moe, var2, "MOE")

    # join expanded total and MOE tables
    expanded <- merge(expanded, expanded_tot, by = var1)
    expanded <- merge(expanded, expanded_moe, by = var1)

    expanded[, mean := weighted_total / get(eval(wt_field))] # mean = weighted_total/weight

    N_hh <- merge(raw, N_hh, by = var1) # join raw and number of households

    expanded <- merge(expanded, N_hh, by = var1) # join expanded and N_hh

    setnames(expanded, var1, "var1")
    setnames(expanded, "hhid", "N_HH")
    crosstab <- expanded
    print(crosstab)
  }

  return(crosstab)
}

simple_table <- function(table, var, wt_field, type) {
  if (type == "dimension") {
    setkeyv(table, var)
    table[table == ""] <- NA # mark "" values as NA

    for (missing in missing_codes) { # filter out var is missing
      table <- subset(table, get(var) != missing)
    }

    table <- na.omit(table, cols = var) # filter out var is NA

    raw <- table[, .(sample_count = .N), by = var] # calculate sample count
    N_hh <- table[, .(hhid = uniqueN(hhid))] # calculate n households

    table <- table[!is.na(get(wt_field))] # filter out weight NAs

    expanded <- table[, lapply(.SD, sum), .SDcols = wt_field, by = var] # sum weight field, grouped by var

    expanded_tot <- expanded[
      , lapply(.SD, sum),
      .SDcols = wt_field
    ][[ # sum weight field
      eval(wt_field)]]

    expanded[, "hhid" := N_hh[["hhid"]][1]] # add column hhid where hhid = n households?

    setnames(expanded, wt_field, "estimate")

    expanded[, share := estimate / eval(expanded_tot)] # estimate / expanded total
    expanded[, p_col := p_MOE] # add MOE column
    expanded[, ("in") := (p_col * (1 - p_col)) / hhid][ # in = (MOEcol * (1-MOEcol))/n households
      , MOE := z * sqrt(get("in"))
    ][ # MOE  = 1.645 * sqrt(in)
      , N_HH := hhid
    ] # rename n households column

    expanded$total <- sum(expanded$estimate) # total = sum(estimate)
    expanded$estMOE <- expanded$MOE * expanded$total # estMOE = MOE * total

    s_table <- merge(raw, expanded, by = var) # merge raw and expanded
  } else if (type == "fact") {
    # rework this because really the cuts are just acting as the variables
    # I think this can have the same logic as the code above.

    setkeyv(table, var)
    table[table == ""] <- NA # change "" values to NA
    for (missing in missing_codes) { # filter out missing var values
      table <- subset(table, get(var) != missing)
    }

    cols <- c(var, wt_field)
    table <- na.omit(table) # filter out NA var values

    if (var == "weighted_trip_count") { # if weighted trip count, then use specified histogram bins
      breaks <- hist_breaks_num_trips
      hist_labels <- hist_breaks_num_trips_labels
    } else { # otherwise, use different histogram bins
      table <- table[eval(parse(text = var)) > min_float] # var  > 0
      table <- table[eval(parse(text = var)) < max_float] # var < 200
      breaks <- hist_breaks
      hist_labels <- hist_breaks_labels
    }

    # break var into specified histogram bins
    var_breaks <- table[, cuts := cut(eval(parse(text = var)),
                                      breaks,
                                      labels = hist_labels, order_result = TRUE,
    )]
    # to do: find a way to pull out this hard code


    N_hh <- table[, .(hhid = uniqueN(hhid)), by = cuts] # n unique households, grouped by bins
    raw <- table[, .(sample_count = .N), by = cuts] # sample count, grouped by bins

    var_cut <- var_breaks[, lapply(.SD, sum),
                          .SDcols = wt_field, by = cuts
    ] # sum weights, grouped by bins

    setnames(var_cut, wt_field, "estimate")

    var_cut$total <- sum(var_cut$estimate) # total  = sum(estimate)
    var_cut[, share := estimate / total] # share = estimate/total
    var_cut <- merge(var_cut, N_hh, by = "cuts") # merge n households and var_cut
    var_cut[, ("in") := (share * (1 - share)) / hhid][ # in = (share * (1-share))/n households
      , MOE := z * sqrt(get("in"))
    ][ # MOE  = 1.654 *  sqrt(in)
      , N_HH := hhid
    ] # rename n households column

    var_cut$estMOE <- var_cut$MOE * var_cut$total # estimate MOE is MOE * total
    var_cut <- merge(raw, var_cut, by = "cuts") # join raw and var cut

    s_table <- setnames(var_cut, "cuts", var) # set names
  }

  return(s_table)
}
