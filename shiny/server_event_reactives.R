# event reactives


EV_REACT_stabTableType <- eventReactive(input$stab_go, {
  select.vars <- variables.lu[variable %in% c(input$stab_xcol), ]
  tables <- unique(select.vars$table_name)
  dtypes <- as.vector(unique(select.vars$dtype))

  if ("Trip" %in% tables) {
    res <- "Trip"
  } else if ("Person" %in% tables) {
    res <- "Person"
  } else {
    res <- "Household"
  }

  if ("fact" %in% dtypes) {
    type <- "fact"
  } else {
    type <- "dimension"
  }

  return(list(Res = res, Type = type))
})

# return list of tables subsetted by value types
EV_REACT_stabTable <- eventReactive(input$stab_go, {
  table.type <- EV_REACT_stabTableType()$Res
  wt_field <- table_names[[table.type]]$weight_name

  # temp statement b/c this cat was only asked in 2019. Use 2019 weights
  if (input$stab_xcat == "Reason for leaving previous residence") {
    wt_field <- hh_move_weight_name
  }

  if (input$stab_xcol == "weighted_trip_count") {
    # use a special weight here because trip counts are a weird case
    wt_field <- hh_day_weight_name
  }

  sql.query <- paste("SELECT seattle_home, hhid,", input$stab_xcol, ",", wt_field, "FROM", table_names[[table.type]]$table_name)
  survey <- read.dt(sql.query, "sqlquery")
  type <- EV_REACT_stabTableType()$Type

  if (input$stab_fltr_sea == T) survey <- survey[seattle_home == "Home in Seattle", ]

  xa <- EV_REACT_stab.varsXAlias()

  simtable <- simple_table(survey, input$stab_xcol, wt_field, type)

  xvals <- EV_REACT_stabXValues()[, .(value_order, value_text)][]

  # check input type and xvals. sometimes xvals doesn't exist for some variables
  if ((typeof(input$stab_xcol) == "character") & (nrow(xvals) > 0)) {
    simtable <- merge(simtable, xvals, by.x = input$stab_xcol, by.y = "value_text")
    setorder(simtable, value_order)
  }

  dtypes <- dtype.choice.stab
  selcols <- c(xa, names(dtypes))
  setnames(simtable, c(input$stab_xcol, dtypes), selcols)
  setcolorder(simtable, selcols)

  dt <- simtable[!(get(eval(xa)) %in% "")][, ..selcols]
})


# match input x and y col with corresponding alias names
# this can be remedied by using return values in selectInput()
# variable X alias
EV_REACT_varsXAlias <- eventReactive(input$xtab_go, { # on go
  # get variable name as shown in table
  xvar.alias <- variables.lu[variable %in% input$xtab_xcol, .(variable_name)]
  # returns character
  unique(xvar.alias$variable_name)
})

# variable Y alias
EV_REACT_varsYAlias <- eventReactive(input$xtab_go, {
  yvar.alias <- variables.lu[variable %in% input$xtab_ycol, .(variable_name)]
  # returns character
  unique(yvar.alias$variable_name)
})


EV_REACT_xtabXValues <- eventReactive(input$xtab_go, { # on go
  # filter values to get variable matching input$xtab_col, re-order
  dt <- values.lu[variable %in% input$xtab_xcol, ][order(value_order)]
  # return datatable
})

EV_REACT_xtabYValues <- eventReactive(input$xtab_go, { # on go
  # filter values to get variable matching input$ytab_col, re-order
  dt <- values.lu[variable %in% input$xtab_ycol, ][order(value_order)]
  v <- as.vector(dt$value_text)
  # return vector
})

#


EV_REACT_xtabTableType <- eventReactive(input$xtab_go, { # on go button

  # using input x col and y col, subset variables.lu to get variable names
  select.vars <- variables.lu[variable %in% c(input$xtab_xcol, input$xtab_ycol), ]

  # get table name and table type
  tables <- as.vector(unique(select.vars$table_name))
  dtypes <- as.vector(unique(select.vars$dtype))

  # assign res to one of Trip, Person, or Household
  if ("Trip" %in% tables) {
    res <- "Trip"
  } else if ("Person" %in% tables) {
    res <- "Person"
  } else {
    res <- "Household"
  }

  # assign type to one of fact or dimension
  if ("fact" %in% dtypes) {
    type <- "fact"
  } else {
    type <- "dimension"
  }

  # return named list Res = table name, Type = table type
  return(list(Res = res, Type = type))
})


# return list of tables subsetted by value types
# ! ONLY USED ONCE INSIDE A REACTIVE
EV_REACT_xtabTable <- eventReactive(input$xtab_go, { # on "Go button"
  table.type <- EV_REACT_xtabTableType()$Res # get table name
  wt_field <- table_names[[table.type]]$weight_name # get weight name

  # temp statement b/c this cat was only asked in 2019. Use 2019 weights
  if (input$xtab_xcat == "Reason for leaving previous residence" ||
      input$xtab_ycat == "Reason for leaving previous residence") {
    # if x or y input is "reason for leaving", then
    wt_field <- hh_move_weight_name
  }

  if (input$xtab_xcol == "weighted_trip_count" ||
      input$xtab_ycol == "weighted_trip_count") {
    # if x or y input is "weighted_trip_count", then
    # use a special weight here because trip counts are a weird case
    wt_field <- hh_day_weight_name
  }

  sql.query <- paste(
    "SELECT seattle_home, hhid,",
    input$xtab_xcol, # x col input
    ",",
    input$xtab_ycol, # y col input
    ",",
    wt_field, # which weight
    "FROM",
    table_names[[table.type]]$table_name # which table
  )
  survey <- read.dt(sql.query, "sqlquery")

  type <- EV_REACT_xtabTableType()$Type

  if (input$xtab_fltr_sea == T) {
    # if input$ filter home in Seattle, then filter survey
    survey <- survey[seattle_home == "Home in Seattle", ]
  }

  # create crosstab
  crosstab <- cross_tab(survey, input$xtab_xcol, input$xtab_ycol, wt_field, type)

  # fetch x-column values datatable
  # select value_order and value_text columns
  xvals <- EV_REACT_xtabXValues()[, .(value_order, value_text)]

  # join with crosstab using value_text
  crosstab <- merge(crosstab, xvals, by.x = "var1", by.y = "value_text")
  # set order
  setorder(crosstab, value_order)

  # rename columns according to aliases
  setnames(crosstab, "var1", EV_REACT_varsXAlias(), skip_absent = TRUE)

  # run function FUN_xtab.col.subset
  xtab.crosstab <- purrr::partial(FUN_xtab.col.subset, table = crosstab)

  if (type == "dimension") {
    column.headers <- col.headers
  } else if (type == "fact") {
    column.headers <- col.headers.facts
  }

  dt.list <- map(as.list(column.headers), xtab.crosstab)
  names(dt.list) <- column.headers

  return(dt.list)
  # return list of dt
})

# ! ONLY USED ONCE INSIDE A REACTIVE
EV_REACT_xtabDtypeBtns <- eventReactive(input$xtab_go, {
  # This reactive will change the display of 'Summary Types' radio buttons
  # depending on whether it is a dimension or fact related table

  if (EV_REACT_xtabTableType()$Type == "dimension") {
    btns <- wellPanel(
      radioButtons("xtab_dtype_rbtns",
                   label = strong("Summary Types"),
                   choices = dtype.choice.xtab
      ),
      div(p("Shares are based on rowwise totals."), style = "font-size: 85%")
    ) # end wellPanel
  } else if (EV_REACT_xtabTableType()$Type == "fact") {
    btns <- wellPanel(
      radioButtons("xtab_dtype_rbtns_fact",
                   label = strong("Summary Types"),
                   choices = dtype.choice.xtab.facts
      )
    ) # end wellPanel
  }

  return(btns)
})

EV_REACT_stab.varsXAlias <- eventReactive(input$stab_go, {
  xvar.alias <- variables.lu[variable %in% input$stab_xcol, .(variable_name)]
  unique(xvar.alias$variable_name)
})

EV_REACT_stabXValues <- eventReactive(input$stab_go, {
  dt <- values.lu[variable %in% input$stab_xcol, ][order(value_order)] # return dt
})


# captions -----
EV_REACT_stabCaption <- eventReactive(input$stab_go, {
  if (input$stab_fltr_sea == T) {
    cap <- "Seattle results"
  } else {
    cap <- "Regional results"
  }
  return(cap)
})

EV_REACT_xtabCaption <- eventReactive(input$xtab_go, { # on go
  if (input$xtab_fltr_sea == T) {
    cap <- "Seattle results"
  } else {
    cap <- "Regional results"
  }
  return(cap)
  # return character
})

