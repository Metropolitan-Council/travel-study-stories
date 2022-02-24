
function(input, output, session) {

  # one way table == stab
  # two way table == xtab

# One-way Table ------------------------------------------------------------

  # fancy show/hide variable definition
  observe({
    onclick(
      "stabXtoggleAdvanced",
      toggle(id = "stabXAdvanced", anim = TRUE)
    )
  })

  output$stab_xcol_det <- renderText({
    # fetch "detail" column given stab_xcol
    xvar.det <- lookup_variables[variable %in% input$stab_xcol, .(detail)]
    # return unique text
    return(unique(xvar.det$detail))
  })


  # use REACT_varsListX to keep stab_xcol up to date
  # with stab_xcat
  output$ui_stab_xcol <- renderUI({
    selectInput(
      inputId = "stab_xcol",
      label = "Variable",
      choices = REACT_varsListX()
    )
  })


  output$ui_stab_res_type_title <- renderUI({
    # heading noting whether Seattle or Regional
    h4(EV_REACT_stabCaption())
  })



  # render simple table
  output$stab_tbl <- DT::renderDataTable({
    colors <- list(ltgrey = "#bdbdc3", dkgrey = "#343439")


    dt <- REACT_stabTable.DT()

    fmt.per <- names(dtype.choice[dtype.choice %in% c("share")])
    fmt.num <- names(dtype.choice[dtype.choice %in% c("estimate", "sample_count")])
    DT::datatable(dt,
                  options = list(
                    bFilter = 0,
                    # pageLength = 10,
                    autoWidth = FALSE,
                    columnDefs = list(list(className = "dt-center", width = "100px", targets = c(2:ncol(dt))))
                  )
    ) %>%
      formatPercentage(fmt.per, 1) %>%
      formatRound(fmt.num, 0) %>%
      formatStyle(
        columns = 2:ncol(dt),
        valueColumns = ncol(dt),
        color = styleInterval(c(30), c(colors$ltgrey, colors$dkgrey))
      )
  })


  # render simple table plotly
  output$stab_vis <- renderPlotly({
    xlabel <- EV_REACT_stab.varsXAlias() # first dim
    dttype <- input$stab_dtype_rbtns
    selection <- names(dtype.choice[dtype.choice %in% dttype])
    geog.caption <- EV_REACT_stabCaption()

    if (dttype %in% col.headers) {
      dt <- REACT_stabVisTable()[type %in% selection, ]
    } else {
      if (dttype == "share_with_MOE") {
        dt <- REACT_stabVisTable.shareMOE()
      }
      if (dttype == "estimate_with_MOE") {
        dt <- REACT_stabVisTable.estMOE()
      }
    }

    l <- length(stabXValues()$ValueText)
    if (l == 0) {
      l <- length(unique(dt$value))
    } # evaluate if values are not in lookup (length 0)

    if (dttype == "share") {
      if (l < 10) {
        p <- stab.plot.bar(
          table = dt,
          format = "percent",
          xlabel = xlabel,
          geog.caption = geog.caption
        )
      } else {
        p <- stab.plot.bar2(
          table = dt,
          format = "percent",
          xlabel = xlabel,
          geog.caption = geog.caption
        )
      }
      return(p)
    } else if (dttype %in% c("estimate", "sample_count", "N_HH")) {
      if (l < 10) {
        p <- stab.plot.bar(
          table = dt,
          format = "nominal",
          xlabel = xlabel,
          geog.caption = geog.caption
        )
      } else {
        p <- stab.plot.bar2(
          table = dt,
          format = "nominal",
          xlabel = xlabel,
          geog.caption = geog.caption
        )
      }
      return(p)
    } else if (dttype == "share_with_MOE") {
      if (l < 10) {
        p <- stab.plot.bar.moe(
          table = dt,
          format = "percent",
          xlabel = xlabel,
          geog.caption = geog.caption
        )
      } else {
        p <- stab.plot.bar2.moe(
          table = dt,
          format = "percent",
          xlabel = xlabel,
          geog.caption = geog.caption
        )
      }
      return(p)
    } else if (dttype == "estimate_with_MOE") {
      if (l < 10) {
        p <- stab.plot.bar.moe(
          table = dt,
          format = "nominal",
          xlabel = xlabel,
          geog.caption = geog.caption
        )
      } else {
        p <- stab.plot.bar2.moe(
          table = dt,
          format = "nominal",
          xlabel = xlabel,
          geog.caption = geog.caption
        )
      }
      return(p)
    } else {
      return(NULL)
    }
  })

  output$ui_stab_tbl <- renderUI({
    # if (EV_REACT_stabTableType()$Type == 'dimension') {
    div(DT::dataTableOutput("stab_tbl"),
        style = "font-size: 95%; width: 85%"
    )
    # } #else {
    #   #div(p('Tabular results not available. This functionality is in progress.'),
    #    style = 'display: flex; justify-content: center; align-items: center; margin-top: 5em;')
    #  }
  })



  # return values associated with category selected
  output$ui_xtab_ycol <- renderUI({
    selectInput("xtab_ycol",
                "Variable",
                REACT_varsListY(),
                selected = REACT_varsListY()[[2]]
    )
  })

  output$ui_stab_vis <- renderUI({
    plotlyOutput("stab_vis", width = "85%")
  })



  # Enable/Disable Download button
  vs <- reactiveValues(
    stabxcol = NULL,
    stabgo = 0,
    stabfltrsea = F
  )

  observeEvent(input$stab_go, { # on one-way go
    vs$stabxcol <- input$stab_xcol
    vs$stabgo <- vs$stabgo + 1
    vs$stabfltrsea <- input$stab_fltr_sea
  })

  observe({
    if (vs$stabgo == 0 || (vs$stabxcol != input$stab_xcol) ||
        (vs$stabfltrsea != input$stab_fltr_sea)) {
      disable("stab_download")
    } else if (vs$stabgo > 0) {
      enable("stab_download")
    }
  })


  output$stab_download <- downloadHandler(
    filename = function() {
      paste0(
        "HHSurvey2017_19_",
        EV_REACT_stab.varsXAlias(), "_",
        EV_REACT_stabCaption(), ".xlsx"
      )
    },
    content = function(file) {
      # write.xlsx(EV_REACT_stabTable(), file)
      write.xlsx(REACT_stabDownloadOutput(), file)
    }
  )



  # 2-way table -----

  # show/hide vars definition
  # implemented with JS
  # we can replace this with a helper pop-up with variable descriptions?
  observe({
    onclick(
      "xtabXtoggleAdvanced",
      toggle(id = "xtabXAdvanced", anim = TRUE)
    )
    onclick(
      "xtabYtoggleAdvanced",
      toggle(id = "xtabYAdvanced", anim = TRUE)
    )
  })

  # renders text detail on x-column selection
  output$xtab_xcol_det <- renderText({
    xvar.det <- lookup_variables[variable %in% input$xtab_xcol, .(detail)]
    unique(xvar.det$detail)
  })

  # renders text detail on x-column selection
  output$xtab_ycol_det <- renderText({
    yvar.det <- lookup_variables[variable %in% input$xtab_ycol, .(detail)]
    unique(yvar.det$detail)
  })

  # XTAB Update X and Y Categories -----------------------------------------------


  # update y-cat selectInput based on x-cat value
  observeEvent(input$xtab_xcat, { # on x-category input change
    move.var <- "Reason for leaving previous residence"
    hh.var <- "Household"
    move.choices <- c(hh.var, move.var)

    x <- input$xtab_xcat
    y <- input$xtab_ycat

    if (x == move.var) {
      # filter to two options
      y.choices <- move.choices
    } else if ((!(x %in% move.choices) & (y == hh.var))) {
      # exclude 'Reason...'
      y.choices <- vars.cat[!(vars.cat %in% move.var)]
    } else if (x == hh.var) {
      # default options
      y.choices <- vars.cat[!(vars.cat %in% "None")]
    } else if (x != move.var & y != move.var) {
      # exclude 'Reason...'
      y.choices <- vars.cat[!(vars.cat %in% move.var)]
    } else {
      # default options
      y.choices <- vars.cat[!(vars.cat %in% "None")]
    }

    updateSelectInput(session, "xtab_ycat",
                      label = "Category",
                      selected = y,
                      choices = y.choices
    )
  })

  # update x-cat selectInput based on y-cat value
  observeEvent(input$xtab_ycat, ignoreInit = TRUE, {
    move.var <- "Reason for leaving previous residence"
    hh.var <- "Household"
    move.choices <- c(hh.var, move.var)

    x <- input$xtab_xcat
    y <- input$xtab_ycat

    if (y == move.var) {
      # filter to two options
      x.choices <- move.choices
    } else if (!(y %in% move.choices)) {
      # exclude only 'Reason...'
      x.choices <- vars.cat[!(vars.cat %in% move.var)]
    } else if (y == hh.var) {
      # default options
      x.choices <- vars.cat[!(vars.cat %in% "None")]
    } else {
      # default options
      x.choices <- vars.cat[!(vars.cat %in% "None")]
    }

    updateSelectInput(session, "xtab_xcat",
                      label = "Category",
                      selected = x,
                      choices = x.choices
    )
  })



  # render crosstab table name as h4 HTML
  output$ui_xtab_res_type_title <- renderUI({
    h4(EV_REACT_xtabCaption())
  })


  # xtab plot
  output$xtab_vis <- renderPlotly({
    xlabel <- EV_REACT_varsXAlias() # x axis variable first dim
    ylabel <- EV_REACT_varsYAlias() # y axis variable second dim
    geog.caption <- EV_REACT_xtabCaption() # caption text

    # type cross tb tble type is dimension, return NULL
    if (EV_REACT_xtabTableType()$Type == "dimension") {
      if (is.null(input$xtab_dtype_rbtns)) {
        return(NULL)
      }

      dttype <- input$xtab_dtype_rbtns
      dttype.label <- names(dtype.choice.xtab[dtype.choice.xtab == dttype])

      if (dttype %in% c("sample_count", "estimate", "share", "MOE", "N_HH")) {
        dt <- REACT_xtabVisTable()[[dttype]]
      } else {
        if (dttype == "share_with_MOE") {
          dt <- REACT_xtabVisTable.ShareMOE()
        } else if (dttype == "estimate_with_MOE") {
          dt <- REACT_xtabVisTable.EstMOE()
        }
      }

      l <- length(unique(dt$value))


      if (dttype == "share") {
        if (l > 10) {
          p <- xtab.plot.bar.pivot(
            table = dt,
            format = "percent",
            xlabel = xlabel,
            ylabel = ylabel,
            dttype.label = dttype.label,
            geog.caption = geog.caption
          )
        } else {
          p <- xtab.plot.bar(
            table = dt,
            format = "percent",
            xlabel = xlabel,
            ylabel = ylabel,
            dttype.label = dttype.label,
            geog.caption = geog.caption
          )
        }
        return(p)
      } else if (dttype %in% c("estimate", "sample_count", "N_HH")) {
        if (l > 10) {
          p <- xtab.plot.bar.pivot(
            table = dt,
            format = "nominal",
            xlabel = xlabel,
            ylabel = ylabel,
            dttype.label = dttype.label,
            geog.caption = geog.caption
          )
        } else {
          p <- xtab.plot.bar(
            table = dt,
            format = "nominal",
            xlabel = xlabel,
            ylabel = ylabel,
            dttype.label = dttype.label,
            geog.caption = geog.caption
          )
        }
        return(p)
      } else if (dttype %in% c("share_with_MOE")) {
        if (l > 10) {
          p <- xtab.plot.bar.moe.pivot(
            table = dt,
            format = "percent",
            xlabel = xlabel,
            ylabel = ylabel,
            geog.caption = geog.caption
          )
        } else {
          p <- xtab.plot.bar.moe(
            table = dt,
            format = "percent",
            xlabel = xlabel,
            ylabel = ylabel,
            geog.caption = geog.caption
          )
        }
        return(p)
      } else if (dttype %in% c("estimate_with_MOE")) {
        if (l > 10) {
          p <- xtab.plot.bar.moe.pivot(
            table = dt,
            format = "nominal",
            xlabel = xlabel,
            ylabel = ylabel,
            geog.caption = geog.caption
          )
        } else {
          p <- xtab.plot.bar.moe(
            table = dt,
            format = "nominal",
            xlabel = xlabel,
            ylabel = ylabel,
            geog.caption = geog.caption
          )
        }
        return(p)
      } else {
        return(NULL)
      }
    } else { # if EV_REACT_xtabTableType()$Type == 'fact'
      if (is.null(input$xtab_dtype_rbtns_fact)) {
        return(NULL)
      }
      dttype <- input$xtab_dtype_rbtns_fact
      dttype.label <- names(dtype.choice.xtab.facts[dtype.choice.xtab.facts == dttype])

      if (dttype %in% c("sample_count", "mean", "MOE", "N_HH")) {
        dt <- REACT_xtabVisTable()[[dttype]]
      } else { # if (dttype == "mean_with_MOE")
        dt <- REACT_xtabVisTable.meanMOE()
      }

      if (dttype %in% c("sample_count", "mean", "N_HH")) {
        p <- xtab.plot.bar.fact(
          table = dt,
          format = "nominal",
          xlabel = xlabel,
          ylabel = ylabel,
          dttype.label = dttype.label,
          geog.caption = geog.caption
        )
        return(p)
      } else { # mean_with_MOE
        p <- xtab.plot.bar.fact.moe(
          table = dt,
          format = "nominal",
          xlabel = xlabel,
          ylabel = ylabel,
          dttype.label = dttype.label,
          geog.caption = geog.caption
        )
        return(p)
      }
    } # end of if/else dim or fact
  })


  output$ui_xtab_dtype_rbtns <- renderUI(
    EV_REACT_xtabDtypeBtns()
  )

  # xtab table
  output$xtab_tbl <- DT::renderDataTable({
    if ((EV_REACT_xtabTableType()$Type == "dimension")) {
      if (is.null(input$xtab_dtype_rbtns)) {
        return(NULL)
      }
      dttype <- input$xtab_dtype_rbtns

      if (dttype %in% c("sample_count", "estimate", "estMOE", "share", "MOE", "N_HH")) {
        # This if/else chunk joins sample count to the table of choice with the purpose
        # of greying out values where sample counts are low.
        dt <- FUN_xtab.join.samplecnt(
          xtabcleandt = REACT_xtabTableClean(),
          dttype = dttype,
          EV_REACT_varsXAlias = EV_REACT_varsXAlias()
        )
        sc.cols <- str_which(colnames(dt), "_sc")
        sc.idx <- sc.cols - 1
        disp.col.max <- length(setdiff(colnames(dt), str_subset(colnames(dt), "_sc")))
      } else {
        if (dttype %in% c("share_with_MOE")) {
          dt <- FUN_xtab.tblMOE.join.samplecnt(
            xtabcleantblMOEdt = REACT_xtabTableClean.DT.ShareMOE(),
            xtabcleandt = REACT_xtabTableClean(),
            dttype = dttype,
            EV_REACT_varsXAlias = EV_REACT_varsXAlias()
          )
        } else if (dttype %in% c("estimate_with_MOE")) {
          dt <- FUN_xtab.tblMOE.join.samplecnt(
            xtabcleantblMOEdt = REACT_xtabTableClean.DT.EstMOE(),
            xtabcleandt = REACT_xtabTableClean(),
            dttype = dttype,
            EV_REACT_varsXAlias = EV_REACT_varsXAlias()
          )
        }

        moe.colnms <- str_subset(colnames(dt)[2:ncol(dt)], "_MOE")
        sc.colnms <- str_subset(colnames(dt)[2:ncol(dt)], "_sc.*")
        cols.fmt <- setdiff(colnames(dt)[2:ncol(dt)], c(moe.colnms, sc.colnms))
        sc.cols <- str_which(colnames(dt), "_sc.*")
        sc.idx <- sc.cols - 1
        disp.col.max <- length(setdiff(colnames(dt), str_subset(colnames(dt), "_sc.*")))
      }

      sketch.dtstyle <- FUN_dt.container.dtstyle(
        atable = dt,
        xvaralias = EV_REACT_varsXAlias(),
        yvaralias = EV_REACT_varsYAlias()
      )

      if (dttype == "share") {
        FUN_xtab.create.DT(
          atable = dt, moe = F,
          acontainer = sketch.dtstyle,
          indices2hide = sc.idx,
          maxyvals = disp.col.max,
          sc.cols = sc.cols
        ) %>%
          formatPercentage(colnames(dt)[2:disp.col.max], 1)
      } else if (dttype == "estimate") {
        FUN_xtab.create.DT(
          atable = dt,
          moe = F,
          acontainer = sketch.dtstyle,
          indices2hide = sc.idx,
          maxyvals = disp.col.max,
          sc.cols = sc.cols
        ) %>%
          formatRound(colnames(dt)[2:disp.col.max], 0)
      } else if (dttype == "sample_count") {
        FUN_xtab.create.DT(
          atable = dt,
          moe = F,
          acontainer = sketch.dtstyle,
          indices2hide = sc.idx,
          maxyvals = disp.col.max,
          sc.cols = sc.cols
        ) %>%
          formatRound(colnames(dt)[2:disp.col.max], 0)
      } else if (dttype == "share_with_MOE") {
        sketch.dtstyle.exp <- FUN_dt.container.tblMOE.dtstyle(
          atable = dt,
          xvaralias = EV_REACT_varsXAlias(),
          yvaralias = EV_REACT_varsYAlias(),
          tbltype = "share"
        )
        FUN_xtab.create.DT(
          atable = dt,
          moe = T,
          acontainer = sketch.dtstyle.exp,
          indices2hide = sc.idx,
          maxyvals = disp.col.max,
          sc.cols = sc.cols
        ) %>%
          formatPercentage(cols.fmt, 1)
      } else if (dttype == "estimate_with_MOE") {
        sketch.dtstyle.exp <- FUN_dt.container.tblMOE.dtstyle(
          atable = dt,
          xvaralias = EV_REACT_varsXAlias(),
          yvaralias = EV_REACT_varsYAlias(),
          tbltype = "estimate"
        )
        FUN_xtab.create.DT(
          atable = dt, moe = T,
          acontainer = sketch.dtstyle.exp,
          indices2hide = sc.idx,
          maxyvals = disp.col.max,
          sc.cols = sc.cols
        ) %>%
          formatRound(cols.fmt, 0)
      }
    } else if ((EV_REACT_xtabTableType()$Type == "fact")) {
      if (is.null(input$xtab_dtype_rbtns_fact)) {
        return(NULL)
      }
      dttype <- input$xtab_dtype_rbtns_fact

      if (dttype %in% c("mean", "sample_count")) {
        # This if/else chunk joins sample count to the table of choice with the purpose
        # of greying out values where sample counts are low.

        dt <- FUN_xtab.join.samplecnt(
          xtabcleandt = REACT_xtabTableClean(),
          dttype = dttype,
          EV_REACT_varsXAlias = EV_REACT_varsXAlias()
        )
        sc.cols <- str_which(colnames(dt), "_sc")
        sc.idx <- sc.cols - 1
        disp.col.max <- length(setdiff(colnames(dt), str_subset(colnames(dt), "_sc")))
      } else {
        if (dttype %in% c("mean_with_MOE")) {
          dt <- FUN_xtab.tblMOE.join.samplecnt(
            xtabcleantblMOEdt = REACT_xtabTableClean.DT.MeanMOE(),
            xtabcleandt = REACT_xtabTableClean(),
            dttype = dttype,
            EV_REACT_varsXAlias = EV_REACT_varsXAlias()
          )
        }

        moe.colnms <- str_subset(colnames(dt)[2:ncol(dt)], "MOE")
        sc.colnms <- str_subset(colnames(dt)[2:ncol(dt)], "_sc.*")
        cols.fmt <- setdiff(colnames(dt)[2:ncol(dt)], c(moe.colnms, sc.colnms))
        sc.cols <- str_which(colnames(dt), "_sc.*")
        sc.idx <- sc.cols - 1
        disp.col.max <- length(setdiff(colnames(dt), str_subset(colnames(dt), "_sc.*")))
      }

      sketch.dtstyle <- FUN_dt.container.dtstyle(
        atable = dt,
        xvaralias = EV_REACT_varsXAlias(),
        yvaralias = EV_REACT_varsYAlias()
      )

      if (dttype == "mean") {
        FUN_xtab.create.DT(
          atable = dt,
          moe = F,
          acontainer = sketch.dtstyle,
          indices2hide = sc.idx,
          maxyvals = disp.col.max,
          sc.cols = sc.cols
        ) %>%
          formatRound(colnames(dt)[2:disp.col.max], 2)
      } else if (dttype == "sample_count") {
        FUN_xtab.create.DT(
          atable = dt, moe = F,
          acontainer = sketch.dtstyle,
          indices2hide = sc.idx,
          maxyvals = disp.col.max,
          sc.cols = sc.cols
        ) %>%
          formatRound(colnames(dt)[2:disp.col.max], 0)
      } else if (dttype == "mean_with_MOE") {
        sketch.dtstyle.exp <- FUN_dt.container.tblMOE.dtstyle(
          atable = dt,
          xvaralias = EV_REACT_varsXAlias(),
          yvaralias = EV_REACT_varsYAlias(),
          tbltype = "mean"
        )
        FUN_xtab.create.DT(
          atable = dt,
          moe = T,
          acontainer = sketch.dtstyle.exp,
          indices2hide = sc.idx,
          maxyvals = disp.col.max,
          sc.cols = sc.cols
        ) %>%
          formatRound(cols.fmt, 2)
      }
    }
  })


  # xtab table..?
  output$ui_xtab_tbl <- renderUI({
    div(DT::dataTableOutput("xtab_tbl"),
        style = "font-size: 95%; width: 85%", class = "visual-display",
    )
  })

  output$ui_xtab_vis <- renderUI({
    # if (EV_REACT_xtabTableType()$Type == 'dimension') {
    #   plotlyOutput("xtab_vis", width = "85%")
    # } else {
    #   div(p('Results not available. This functionality is in progress.'),
    #       style = 'display: flex; justify-content: center; align-items: center; margin-top: 5em;')
    # }
    div(plotlyOutput("xtab_vis", width = "85%"), class = "visual-display")
  })


  # Enable/Disable download button
  v <- reactiveValues(
    xtabxcol = NULL,
    xtabycol = NULL,
    xtabgo = 0,
    xtabfltrsea = F
  )

  observeEvent(input$xtab_go, { # on two way go
    v$xtabxcol <- input$xtab_xcol
    v$xtabycol <- input$xtab_ycol
    v$xtabgo <- v$xtabgo + 1
    v$xtabfltrsea <- input$xtab_fltr_sea
  })

  observe({
    if (v$xtabgo == 0 || (v$xtabycol != input$xtab_ycol) ||
        (v$xtabxcol != input$xtab_xcol) || (v$xtabfltrsea != input$xtab_fltr_sea)) {
      disable("xtab_download")
    } else if (v$xtabgo > 0) {
      enable("xtab_download")
    }
  })


  output$xtab_download <- downloadHandler(
    filename = function() {
      paste0(
        "HHSurvey2017_19_", EV_REACT_varsXAlias(),
        "_by_", EV_REACT_varsYAlias(), "_", EV_REACT_xtabCaption(), ".xlsx"
      )
    },
    content = function(file) {
      write.xlsx(REACT_xtabDownloadOutput(), file)
    }
  )

  # use REACT_varsListX to keep xtab_xcol up to date
  # with xtab_xcat
  output$ui_xtab_xcol <- renderUI({
    selectInput(
      inputId = "xtab_xcol",
      label = "Variable",
      choices = REACT_varsListX()
    )
  })

}
