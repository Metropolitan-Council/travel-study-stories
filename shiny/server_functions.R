# functions - plain jane functions

# Column Subset Crosstab Generator
FUN_xtab.col.subset <- function(table, colstring = col.headers) {

  # fetch X variable alias
  # subset table column names to match X variable alias
  cols <- c(EV_REACT_varsXAlias(),
            str_subset(colnames(table),
                       paste0("^", colstring)))
  table[, ..cols]
  # return dt
}

FUN_xtab.join.samplecnt <- function(xtabcleandt, dttype, EV_REACT_varsXAlias) {
  # fetch table type
  dt.data <- xtabcleandt[[dttype]]
  # fetch x var alias
  dt.sort.rows <- dt.data[[EV_REACT_varsXAlias]]

  dt.style <- copy(xtabcleandt[["sample_count"]])

  # alter dt.style, update rows containing < 30
  # dt.style <- xtab.edit.samplecnt(dt.style, 30)

  colnames(dt.style)[2:length(colnames(dt.style))] <- paste0(colnames(dt.style)[2:length(colnames(dt.style))], "_sc")
  dt <- merge(dt.data, dt.style, by = EV_REACT_varsXAlias)
  dt[, var1.sort := factor(get(EV_REACT_varsXAlias), levels = dt.sort.rows)]
  dt <- dt[order(var1.sort)][, var1.sort := NULL]
}

FUN_xtab.tblMOE.join.samplecnt <- function(xtabcleantblMOEdt, xtabcleandt, dttype, EV_REACT_varsXAlias) {
  dt.data <- xtabcleantblMOEdt
  dt.style <- copy(xtabcleandt[["sample_count"]])
  dt.sort.rows <- dt.data[[EV_REACT_varsXAlias]]

  idx <- 2:ncol(dt.style)
  colnames(dt.style)[idx] <- paste0(letters[1:(ncol(dt.style) - 1)], "_", colnames(dt.style)[idx], "_sc")
  new.cols <- paste0(colnames(dt.style)[idx], "2")
  cols <- colnames(dt.style)[idx]
  col2order <- sort(c(cols, new.cols))
  dt.style[, (new.cols) := mapply(function(x) replicate(1, .SD[[x]]), cols, SIMPLIFY = F)]
  setcolorder(dt.style, c(EV_REACT_varsXAlias, col2order))

  # alter dt.style, update rows containing < 30
  # dt.style <- xtab.edit.samplecnt(dt.style, 30)

  dt <- merge(dt.data, dt.style, by = EV_REACT_varsXAlias)

  dt[, var1.sort := factor(get(EV_REACT_varsXAlias), levels = dt.sort.rows)]
  dt <- dt[order(var1.sort)][, var1.sort := NULL]
}

FUN_xtab.create.DT <- function(atable, moe = c(TRUE, FALSE), acontainer, indices2hide, maxyvals, sc.cols) {
  colors <- list(ltgrey = "#bdbdc3", dkgrey = "#343439")
  if (moe == TRUE) {
    defs <- list(
      list(className = "dt-head-center dt-center", targets = "_all"), # DT CRAN hack
      list(visible = F, targets = indices2hide)
    ) # DT's column index starts at 0 not 1
  } else {
    defs <- list(list(visible = F, targets = indices2hide)) # DT's column index starts at 0 not 1
  }

  DT::datatable(atable,
    # caption = acaption,
    container = acontainer,
    rownames = FALSE,
    options = list(
      bFilter = 0,
      columnDefs = defs
    ) # DT's column index starts at 0 not 1
  ) %>%
    formatStyle(
      columns = 2:maxyvals,
      valueColumns = sc.cols,
      color = styleInterval(c(30), c(colors$ltgrey, colors$dkgrey))
    )
}

FUN_dt.container.dtstyle <- function(atable, xvaralias, yvaralias) {
  sc.cols <- str_subset(colnames(atable), "_sc")
  num.disp.cols <- ncol(atable) - length(sc.cols)
  htmltools::withTags(
    table(
      class = "display",
      thead(
        tr(
          th(class = "dt-center", rowspan = 2, xvaralias),
          th(class = "dt-center", colspan = (num.disp.cols - 1), yvaralias)
        ), # end tr
        tr(
          lapply(colnames(atable)[2:num.disp.cols], th)
        ) # end tr
      ) # end thead
    ) # end table
  ) # end withTags
}

FUN_dt.container.tblMOE.dtstyle <- function(atable, xvaralias, yvaralias, tbltype = c("share", "estimate", "mean")) {
  # ifelse(tbltype == "share", tbltype <- "Share", tbltype <- "Total")
  if (tbltype == "share") {
    tbltype <- "Share"
  } else if (tbltype == "estimate") {
    tbltype <- "Total"
  } else {
    tbltype <- "Mean"
  }

  exc.cols <- str_subset(colnames(atable), paste(xvaralias, "_MOE|_sc.*", sep = "|"))
  yval.labels <- setdiff(colnames(atable), exc.cols)

  sc.cols <- str_subset(colnames(atable), "_sc.*")
  num.disp.cols <- ncol(atable) - length(sc.cols)

  if (tbltype == "Share" | tbltype == "Total") {
    htmltools::withTags(
      table(
        class = "display",
        thead(
          tr(
            th(class = "dt-center", rowspan = 3, xvaralias),
            th(class = "dt-center", colspan = (num.disp.cols - 1), yvaralias)
          ), # end tr
          tr(
            lapply(yval.labels, function(x) th(class = "dt-center", colspan = 2, x))
          ), # end tr
          tr(
            lapply(rep(c(tbltype, "Margin of Error"), (num.disp.cols - 1) / 2), function(x) th(style = "font-size:12px", x))
          ) # end tr
        ) # end thead
      ) # end table
    ) # end withTags
  } else {
    htmltools::withTags(
      table(
        class = "display",
        thead(
          tr(
            th(class = "dt-center", rowspan = 2, xvaralias),
            th(class = "dt-center", colspan = (num.disp.cols - 1), yvaralias)
          ), # end tr
          # tr(
          #   lapply(yval.labels, function(x) th(class = 'dt-center', colspan = 2, x))
          # ), # end tr
          tr(
            lapply(rep(c(tbltype, "Margin of Error"), (num.disp.cols - 1) / 2), function(x) th(style = "font-size:12px", x))
          ) # end tr
        ) # end thead
      ) # end table
    ) # end withTags
  }
}



FUN_create.table.vistable.moe <- function(valuetable, moetable, xalias, xvalues) {
  msrcols <- colnames(valuetable)[!(colnames(valuetable) %in% xalias)]
  dts <- melt.data.table(valuetable, id.vars = xalias, measure.vars = msrcols, variable.name = "value", value.name = "result")

  if (xtabTableType()$Type == "dimension") {
    dtm <- melt.data.table(moetable, id.vars = xalias, measure.vars = msrcols, variable.name = "value", value.name = "result_moe")
    dt <- merge(dts, dtm, by = c(xalias, "value"))
    setnames(dt, xalias, "group")
  } else {
    dt <- merge(dts, moetable, by = c(xalias))
    setnames(dt, c(xalias, "MOE"), c("group", "result_moe"))
  }

  if (nrow(xvalues) != 0) {
    dt[, group := factor(group, levels = xvalues$value_text)][, group := fct_explicit_na(group, "No Response")]
    dt <- dt[order(group)]
  } else {
    dt[, group := factor(group)]
  }
  return(dt)
}



FUN_create.table.joining.moe <- function(valuetable, moetable, xalias, xvalues) {
  # This is function is for Dimension related tables

  dtcols <- colnames(valuetable)[2:ncol(valuetable)]
  cols.order <- c()
  for (acol in dtcols) {
    moe.col <- paste0(acol, "_MOE")
    cols.order <- append(cols.order, c(acol, moe.col))
  }
  colnames(moetable)[2:ncol(moetable)] <- paste0(colnames(moetable)[2:ncol(moetable)], "_MOE")
  dt.sm <- merge(valuetable, moetable, by = xalias)
  dt.sm[, var1.sort := factor(get(eval(xalias)), levels = xvalues$value_text)]
  dt.sm <- dt.sm[order(var1.sort)][, var1.sort := NULL]
  order.colnames <- c(xalias, cols.order)
  dt.sm <- dt.sm[, ..order.colnames]
}
