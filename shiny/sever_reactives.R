# all reactives ( NOT eventReactives

# variables and aliases -----


# fetch variables available given the category (xtab_cat)
REACT_varsListX <- reactive({
  t <- variables.lu[category %in% input$xtab_xcat & dtype != "fact", ]

  vars.raw <- as.list(unique(t$variable))
  vars.list <- setNames(vars.raw, as.list(unique(t$variable_name)))

  return(vars.list)
})

# variable Y alias list
# ! ONLY USED ONCE
REACT_varsListY <- reactive({
  t <- variables.lu[category %in% input$xtab_ycat, ]

  vars.raw <- as.list(unique(t$variable))
  vars.list <- setNames(vars.raw, as.list(unique(t$variable_name)))

  return(vars.list)
})


# table cleaning -----
# clean xtabTable()
REACT_xtabTableClean <- reactive({
  dt.list <- EV_REACT_xtabTable() # list of data.tables
  # yv <- EV_REACT_xtabYValues()
  xa <- EV_REACT_varsXAlias()
  # col.headers <- lapply(col.headers, function(x) paste0(x, "_")) %>% unlist
  # regex <- paste(col.headers, collapse = "|")

  if (EV_REACT_xtabTableType()$Type == "dimension") {
    yv <- EV_REACT_xtabYValues()
    col.headers <- lapply(col.headers, function(x) paste0(x, "_")) %>% unlist()
    regex <- paste(col.headers, collapse = "|")

    # evaluates for NA columns & rows, and excludes it
    for (i in 1:length(dt.list)) {
      dt.list[[i]] <- dt.list[[i]][!(get(eval(xa)) %in% "")]

      new.colnames <- str_extract(colnames(dt.list[[i]])[
        2:length(colnames(dt.list[[i]]))
      ], paste0("(?<=", regex, ").+")) # includes blank

      if (any(is.na(new.colnames))) { # if contains any NA columns
        nonna.new.colnames <- str_subset(new.colnames, ".")
        setnames(
          dt.list[[i]], colnames(dt.list[[i]]),
          c(xa, new.colnames)
        ) # blank becomes NA
        keep.cols <- colnames(dt.list[[i]])[!is.na(colnames(dt.list[[i]]))]
        dt.list[[i]] <- dt.list[[i]][, ..keep.cols]

        if (length(yv) != 0) {
          yv.subset <- yv[yv %in% nonna.new.colnames] # only account for yv vals that exist in dt
          setcolorder(dt.list[[i]], c(xa, yv.subset))
        }
      } else {
        setnames(dt.list[[i]], colnames(dt.list[[i]]), c(xa, new.colnames))
        if (!is.null(yv)) {
          yv.subset <- yv[yv %in% new.colnames] # are all yv vals accounted for in new.colnames
          setcolorder(dt.list[[i]], c(xa, yv.subset))
        }
      }
    }
  } else if (EV_REACT_xtabTableType()$Type == "fact") {
    new.colnames.fact <- c(
      "Mean" = "mean",
      "Sample Count" = "sample_count",
      "Number of Households" = "N_HH"
    )
    for (i in 1:length(dt.list)) {
      # set colnames for mean, sample count, number of households field
      if (names(dt.list[i]) %in% new.colnames.fact) {
        setnames(dt.list[[i]],
                 names(dt.list[i]),
                 names(new.colnames.fact[new.colnames.fact %in% names(dt.list[i])]),
                 skip_absent = TRUE
        )
      } else {
        next
      }
    }
  }

  return(dt.list)
})




# create separate table of shares alongside margin of errors
REACT_xtabTableClean.ShareMOE <- reactive({
  xa <- EV_REACT_varsXAlias()
  xvals <- EV_REACT_xtabXValues()[, .(value_order, value_text)]

  dt.s <- REACT_xtabTableClean()[["share"]]
  dt.m <- REACT_xtabTableClean()[["MOE"]]
  dt.sm <- FUN_create.table.joining.moe(dt.s, dt.m, xa, xvals)

  return(dt.sm)
})




# create separate table of estimates alongside margin of errors
REACT_xtabTableClean.EstMOE <- reactive({
  xa <- EV_REACT_varsXAlias()
  xvals <- EV_REACT_xtabXValues()[, .(value_order, value_text)]

  dt.s <- REACT_xtabTableClean()[["estimate"]]
  dt.m <- REACT_xtabTableClean()[["estMOE"]]

  dt.sm <- FUN_create.table.joining.moe(dt.s, dt.m, xa, xvals)

  return(dt.sm)
})

# create separate table of mean (for fact related tables) alongside margin of errors
REACT_xtabTableClean.MeanMOE <- reactive({
  xa <- EV_REACT_varsXAlias()
  xvals <- EV_REACT_xtabXValues()[, .(value_order, value_text)]

  dt.s <- REACT_xtabTableClean()[["mean"]]
  dt.m <- REACT_xtabTableClean()[["MOE"]]

  dt <- merge(dt.s, dt.m, by = xa)
  dt[, var1.sort := factor(get(eval(xa)), levels = xvals$value_text)]
  dt.sm <- dt[order(var1.sort)][, var1.sort := NULL]

  return(dt.sm)
})

# two-way table with mean and MOE
REACT_xtabTableClean.DT.MeanMOE <- reactive({
  t <- copy(REACT_xtabTableClean.MeanMOE())

  t[, MOE := lapply(.SD, function(x) {
    prettyNum(round(x, 2),
              big.mark = ",",
              preserve.width = "none"
    )
  }),
  .SDcols = "MOE"
  ]
  t[, MOE := lapply(.SD, function(x) paste0("+/-", as.character(x))),
    .SDcols = "MOE"
  ]

  return(t)
})

# two-way table with share and MOE
REACT_xtabTableClean.DT.ShareMOE <- reactive({
  t <- copy(REACT_xtabTableClean.ShareMOE())

  moe.cols <- str_subset(colnames(t), "_MOE$")
  t[, (moe.cols) := lapply(.SD, function(x) round(x * 100, 1)), .SDcols = moe.cols]
  t[, (moe.cols) := lapply(.SD, function(x) paste0("+/-", as.character(x), "%")),
    .SDcols = moe.cols
  ]

  for (j in seq_along(t)) {
    set(t, i = which(t[[j]] == "+/-NA%"), j = j, value = "")
  }
  return(t)
})

# two-way table with estimate and MOE
REACT_xtabTableClean.DT.EstMOE <- reactive({
  t <- copy(REACT_xtabTableClean.EstMOE())

  moe.cols <- str_subset(colnames(t), "_MOE$")
  t[, (moe.cols) := lapply(
    .SD,
    function(x) {
      prettyNum(round(x, 0),
                big.mark = ",",
                preserve.width = "none"
      )
    }
  ),
  .SDcols = moe.cols
  ]
  t[, (moe.cols) := lapply(.SD, function(x) paste0("+/-", as.character(x))),
    .SDcols = moe.cols
  ]

  for (j in seq_along(t)) {
    set(t, i = which(t[[j]] == "+/-NA"), j = j, value = "")
  }
  return(t)
})

# two way table for plotting estimate and MOE
REACT_xtabVisTable.EstMOE <- reactive({
  xa <- EV_REACT_varsXAlias()
  xvals <- EV_REACT_xtabXValues()[, .(value_order, value_text)]

  dt.s <- REACT_xtabTableClean()[["estimate"]]
  dt.m <- REACT_xtabTableClean()[["estMOE"]]

  dt <- FUN_create.table.vistable.moe(dt.s, dt.m, xa, xvals)

  return(dt)
})


# two way table for plotting share and MOE
REACT_xtabVisTable.ShareMOE <- reactive({
  xa <- EV_REACT_varsXAlias()
  xvals <- EV_REACT_xtabXValues()[, .(value_order, value_text)]
  dt.s <- REACT_xtabTableClean()[["share"]]
  dt.m <- REACT_xtabTableClean()[["MOE"]]

  dt <- FUN_create.table.vistable.moe(dt.s, dt.m, xa, xvals)

  return(dt)
})

# two way table for plotting mean and MOE
REACT_xtabVisTable.meanMOE <- reactive({
  xa <- EV_REACT_varsXAlias()
  xvals <- EV_REACT_xtabXValues()[, .(value_order, value_text)]

  dt.s <- REACT_xtabTableClean()[["mean"]]
  dt.m <- REACT_xtabTableClean()[["MOE"]]

  dt <- FUN_create.table.vistable.moe(dt.s, dt.m, xa, xvals)

  return(dt)
})

# two-way table for
REACT_xtabVisTable <- reactive({
  dt.list <- REACT_xtabTableClean()
  xvals <- EV_REACT_xtabXValues()[, .(value_order, value_text)]

  visdt.list <- NULL
  for (i in 1:length(dt.list)) {
    idcol <- EV_REACT_varsXAlias()
    msrcols <- colnames(dt.list[[i]])[!(colnames(dt.list[[i]]) %in% idcol)]

    varcol <- "value"
    t <- melt.data.table(dt.list[[i]],
                         id.vars = idcol, measure.vars = msrcols,
                         variable.name = "value", value.name = "result"
    )

    t[, type := names(dt.list[i])]
    setnames(t, idcol, "group")

    if (nrow(xvals) != 0) {
      t[, group := factor(group, levels = xvals$value_text)][
        , group := fct_explicit_na(group, "No Response")
      ]

      t <- t[order(group)]
    } else {
      t[, group := factor(group)]
    }
    visdt.list[[names(dt.list[i])]] <- t
  }
  return(visdt.list)
})


# create 2-way table for download
REACT_xtabDownloadOutput <- reactive({
  dtlist <- copy(REACT_xtabTableClean())
  t <- dtlist[["sample_count"]]

  data.type <- EV_REACT_xtabTableType()$Type
  geog <- EV_REACT_xtabCaption()

  if (data.type == "dimension") {
    tsm <- copy(REACT_xtabTableClean.DT.ShareMOE()) # share with MOE
    tem <- copy(REACT_xtabTableClean.DT.EstMOE()) # estimate with MOE

    # Format tsm, every other column as string starting at index 2
    nums <- seq(1, length(colnames(tsm)))
    evens <- unlist(lapply(nums, function(x) x %% 2 == 0))
    ind <- nums[evens]

    cols.to.str <- colnames(tsm)[ind]
    tsm[, (cols.to.str) := lapply(.SD, function(x) paste0(as.character(round(x * 100, 1)), "%")),
        .SDcols = cols.to.str
    ]

    # Format tem, every other column as string starting at index 2
    cols.to.prettynum <- colnames(tem)[ind]
    tem[, (cols.to.prettynum) := lapply(.SD, function(x) prettyNum(round(x), big.mark = ",")),
        .SDcols = cols.to.prettynum
    ]

    for (j in seq_along(tsm)) {
      set(tsm, i = which(tsm[[j]] == "NA%"), j = j, value = "")
    }

    for (j in seq_along(tem)) {
      set(tem, i = which(tem[[j]] == "NA"), j = j, value = "")
    }

    tsm[, `Result Type` := geog]
    tem[, `Result Type` := geog]
    t[, `Result Type` := geog]

    tbllist <- list(
      "About" = readme.dt,
      "Share with Margin of Error" = tsm,
      "Total with Margin of Error" = tem,
      "Sample Count" = t
    )
  } else if (data.type == "fact") {
    # join mean/MOE with sample count
    tmm <- copy(REACT_xtabTableClean.DT.MeanMOE())

    tjoin <- tmm[t, on = EV_REACT_varsXAlias()]
    tj <- tjoin[
      , Mean := lapply(.SD, function(x) round(x, 2)),
      .SDcols = "Mean"
    ][
      , `Sample Count` := lapply(.SD, function(x) prettyNum(x, big.mark = ",")),
      .SDcols = "Sample Count"
    ][
      , `Result Type` := geog
    ]


    setnames(tj, "MOE", "Margin of Error (Mean)")

    tbllist <- list(
      "About" = readme.dt,
      "Mean with Margin of Error" = tj
    )
  }
  return(tbllist)

  # return list with readme.dt and table
})



# clean Margin of Error column and column reorder
REACT_stabTable.DT <- reactive({

  #
  xa <- EV_REACT_stab.varsXAlias() # variable NAME
  dt <- data.table::copy(EV_REACT_stabTable()) # fetch table with values given inputs


  # lookup value name based on abbreviation
  col <- names(dtype.choice[dtype.choice %in% "MOE"])
  col2 <- names(dtype.choice[dtype.choice %in% "estMOE"])

  dt[, (col) := lapply(.SD, function(x) round(x * 100, 1)), .SDcols = col][ # for all MOE columns, round
    , (col2) := lapply(.SD, function(x) {
      prettyNum(round(x, 0), # for all estMOE columns, prettyNum
                big.mark = ",", preserve.width = "none"
      )
    }),
    .SDcols = col2
  ]

  # add +/- text for MOE and estMOE
  dt[,
     (col) := lapply(.SD, function(x) paste0("+/-", as.character(x), "%")),
     .SDcols = col
  ][
    , (col2) := lapply(.SD, function(x) paste0("+/-", as.character(x))),
    .SDcols = col2
  ]


  new.colorder <- c(
    xa, # variable name
    names(dtype.choice[dtype.choice %in% c("share")]), # "Share"
    col, # MOE
    names(dtype.choice[dtype.choice %in% c("estimate")]), # "Total"
    col2, # estMOE
    names(dtype.choice[dtype.choice %in% c("sample_count")]) # "Sample Count"
  )

  setcolorder(dt, new.colorder)

  # dt will have columns for
  # variable name, share, MOE, Total, estMOE, and Sample Count
  return(dt)
})


# generate table for plotting sample count
REACT_stabVisTable <- reactive({
  dt <- EV_REACT_stabTable()
  idvar <- EV_REACT_stab.varsXAlias()
  # xvals <- EV_REACT_stabXValues()[, .(ValueOrder, ValueText)]
  xvals <- EV_REACT_stabXValues()[, .(value_order, value_text)]

  cols <- names(dtype.choice[dtype.choice %in% c("sample_count")])
  dt[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
  msr.vars <- names(dtype.choice[names(dtype.choice) %in% colnames(dt)])
  t <- melt.data.table(dt,
                       id.vars = idvar, measure.vars = msr.vars,
                       variable.name = "type", value.name = "result"
  )
  setnames(t, idvar, "value")

  if (nrow(xvals) != 0) {
    # t[, value := factor(value, levels = xvals$ValueText)][, value := fct_explicit_na(value, "No Response")]
    t[, value := factor(value, levels = xvals$value_text)][
      , value := fct_explicit_na(value, "No Response")
    ]
    t <- t[order(value)]
  } else {
    t[, value := factor(value)]
  }
  return(t)

  # returns dt, must have columns group, result
})

# data for plotting share with share MOE
REACT_stabVisTable.shareMOE <- reactive({
  types <- names(dtype.choice[dtype.choice %in% c("share", "MOE")])

  dt <- REACT_stabVisTable()[type %in% types]
  t <- dcast.data.table(dt, value ~ type, value.var = "result")

  setnames(
    t, str_subset(colnames(t), paste(types, collapse = "|")),
    c("result", "result_moe")
  )
  return(t)
  # returns dt, must have columns group, result, result_moe
})

# data for plotting estimate with estimate MOE
REACT_stabVisTable.estMOE <- reactive({
  types <- names(dtype.choice[dtype.choice %in% c("estimate", "estMOE")])
  dt <- REACT_stabVisTable()[type %in% types]
  t <- dcast.data.table(dt, value ~ type,
                        value.var = "result"
  )
  setnames(
    t, str_subset(
      colnames(t),
      paste(types, collapse = "|")
    ),
    c("result", "result_moe")
  )
  return(t)
})


# create one way table download
REACT_stabDownloadOutput <- reactive({
  t <- copy(EV_REACT_stabTable())
  geog <- EV_REACT_stabCaption()

  # update column names
  cols.fmt.per <- str_subset(colnames(t), "Share")
  cols.fmt.nom <- str_subset(colnames(t), "Total")
  cols.fmt.moe <- str_subset(colnames(t), "Margin of Error")

  t[, (cols.fmt.per) := lapply(.SD, function(x) paste0(as.character(round(x * 100, 1)), "%")),
    .SDcols = cols.fmt.per
  ][
    , (cols.fmt.nom) := lapply(.SD, function(x) prettyNum(round(x), big.mark = ",")),
    .SDcols = cols.fmt.nom
  ][
    , (cols.fmt.moe) := lapply(.SD, function(x) paste0("+/-", x)),
    .SDcols = cols.fmt.moe
  ][
    , `Result Type` := geog
  ]

  tlist <- list("About" = readme.dt, "Simple Table" = t)

  return(tlist)
  # retuns list with README.dt and table
})
