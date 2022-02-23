
function(input, output, session) {

  # one way table == stab
  # two way table == xtab


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
    xvar.det <- variables.lu[variable %in% input$xtab_xcol, .(detail)]
    unique(xvar.det$detail)
  })

  # renders text detail on x-column selection
  output$xtab_ycol_det <- renderText({
    yvar.det <- variables.lu[variable %in% input$xtab_ycol, .(detail)]
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
        if (dttype == "share_with_MOE") dt <- REACT_xtabVisTable.ShareMOE()
        if (dttype == "estimate_with_MOE") dt <- REACT_xtabVisTable.EstMOE()
      }

      l <- length(unique(dt$value))

      if (dttype == "share") {
        ifelse(l > 10, p <- xtab.plot.bar.pivot(dt, "percent", xlabel, ylabel, dttype.label, geog.caption), p <- xtab.plot.bar(dt, "percent", xlabel, ylabel, dttype.label, geog.caption))
        return(p)
      } else if (dttype %in% c("estimate", "sample_count", "N_HH")) {
        ifelse(l > 10, p <- xtab.plot.bar.pivot(dt, "nominal", xlabel, ylabel, dttype.label, geog.caption), p <- xtab.plot.bar(dt, "nominal", xlabel, ylabel, dttype.label, geog.caption))
        return(p)
      } else if (dttype %in% c("share_with_MOE")) {
        ifelse(l > 10, p <- xtab.plot.bar.moe.pivot(dt, "percent", xlabel, ylabel, geog.caption), p <- xtab.plot.bar.moe(dt, "percent", xlabel, ylabel, geog.caption))
        return(p)
      } else if (dttype %in% c("estimate_with_MOE")) {
        ifelse(l > 10, p <- xtab.plot.bar.moe.pivot(dt, "nominal", xlabel, ylabel, geog.caption), p <- xtab.plot.bar.moe(dt, "nominal", xlabel, ylabel, geog.caption))
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
        p <- xtab.plot.bar.fact(dt, "nominal", xlabel, ylabel, dttype.label, geog.caption)
        return(p)
      } else { # mean_with_MOE
        p <- xtab.plot.bar.fact.moe(dt, "nominal", xlabel, ylabel, dttype.label, geog.caption)
        return(p)
      }
    } # end of if/else dim or fact
  })

  # Crosstab Generator Table Rendering --------------------------------------------

  output$ui_xtab_dtype_rbtns <- renderUI(
    EV_REACT_xtabDtypeBtns()
  )

  output$xtab_tbl <- DT::renderDataTable({
    if ((EV_REACT_xtabTableType()$Type == "dimension")) {
      if (is.null(input$xtab_dtype_rbtns)) {
        return(NULL)
      }
      dttype <- input$xtab_dtype_rbtns

      if (dttype %in% c("sample_count", "estimate", "estMOE", "share", "MOE", "N_HH")) {
        # This if/else chunk joins sample count to the table of choice with the purpose
        # of greying out values where sample counts are low.
        dt <- FUN_xtab.join.samplecnt(REACT_xtabTableClean(), dttype, EV_REACT_varsXAlias())
        sc.cols <- str_which(colnames(dt), "_sc")
        sc.idx <- sc.cols - 1
        disp.col.max <- length(setdiff(colnames(dt), str_subset(colnames(dt), "_sc")))
      } else {
        if (dttype %in% c("share_with_MOE")) {
          dt <- FUN_xtab.tblMOE.join.samplecnt(REACT_xtabTableClean.DT.ShareMOE(), REACT_xtabTableClean(), dttype, EV_REACT_varsXAlias())
        } else if (dttype %in% c("estimate_with_MOE")) {
          dt <- FUN_xtab.tblMOE.join.samplecnt(REACT_xtabTableClean.DT.EstMOE(), REACT_xtabTableClean(), dttype, EV_REACT_varsXAlias())
        }

        moe.colnms <- str_subset(colnames(dt)[2:ncol(dt)], "_MOE")
        sc.colnms <- str_subset(colnames(dt)[2:ncol(dt)], "_sc.*")
        cols.fmt <- setdiff(colnames(dt)[2:ncol(dt)], c(moe.colnms, sc.colnms))
        sc.cols <- str_which(colnames(dt), "_sc.*")
        sc.idx <- sc.cols - 1
        disp.col.max <- length(setdiff(colnames(dt), str_subset(colnames(dt), "_sc.*")))
      }

      sketch.dtstyle <- FUN_dt.container.dtstyle(dt, EV_REACT_varsXAlias(), EV_REACT_varsYAlias())

      if (dttype == "share") {
        FUN_xtab.create.DT(dt, moe = F, sketch.dtstyle, sc.idx, disp.col.max, sc.cols) %>%
          formatPercentage(colnames(dt)[2:disp.col.max], 1)
      } else if (dttype == "estimate") {
        FUN_xtab.create.DT(dt, moe = F, sketch.dtstyle, sc.idx, disp.col.max, sc.cols) %>%
          formatRound(colnames(dt)[2:disp.col.max], 0)
      } else if (dttype == "sample_count") {
        FUN_xtab.create.DT(dt, moe = F, sketch.dtstyle, sc.idx, disp.col.max, sc.cols) %>%
          formatRound(colnames(dt)[2:disp.col.max], 0)
      } else if (dttype == "share_with_MOE") {
        sketch.dtstyle.exp <- FUN_dt.container.tblMOE.dtstyle(dt, EV_REACT_varsXAlias(), EV_REACT_varsYAlias(), "share")
        FUN_xtab.create.DT(dt, moe = T, sketch.dtstyle.exp, sc.idx, disp.col.max, sc.cols) %>%
          formatPercentage(cols.fmt, 1)
      } else if (dttype == "estimate_with_MOE") {
        sketch.dtstyle.exp <- FUN_dt.container.tblMOE.dtstyle(dt, EV_REACT_varsXAlias(), EV_REACT_varsYAlias(), "estimate")
        FUN_xtab.create.DT(dt, moe = T, sketch.dtstyle.exp, sc.idx, disp.col.max, sc.cols) %>%
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

        dt <- FUN_xtab.join.samplecnt(REACT_xtabTableClean(), dttype, EV_REACT_varsXAlias())
        sc.cols <- str_which(colnames(dt), "_sc")
        sc.idx <- sc.cols - 1
        disp.col.max <- length(setdiff(colnames(dt), str_subset(colnames(dt), "_sc")))
      } else {
        if (dttype %in% c("mean_with_MOE")) {
          dt <- FUN_xtab.tblMOE.join.samplecnt(REACT_xtabTableClean.DT.MeanMOE(), REACT_xtabTableClean(), dttype, EV_REACT_varsXAlias())
        }
        moe.colnms <- str_subset(colnames(dt)[2:ncol(dt)], "MOE")
        sc.colnms <- str_subset(colnames(dt)[2:ncol(dt)], "_sc.*")
        cols.fmt <- setdiff(colnames(dt)[2:ncol(dt)], c(moe.colnms, sc.colnms))
        sc.cols <- str_which(colnames(dt), "_sc.*")
        sc.idx <- sc.cols - 1
        disp.col.max <- length(setdiff(colnames(dt), str_subset(colnames(dt), "_sc.*")))
      }

      sketch.dtstyle <- FUN_dt.container.dtstyle(dt, EV_REACT_varsXAlias(), EV_REACT_varsYAlias())

      if (dttype == "mean") {
        FUN_xtab.create.DT(dt, moe = F, sketch.dtstyle, sc.idx, disp.col.max, sc.cols) %>%
          formatRound(colnames(dt)[2:disp.col.max], 2)
      } else if (dttype == "sample_count") {
        FUN_xtab.create.DT(dt, moe = F, sketch.dtstyle, sc.idx, disp.col.max, sc.cols) %>%
          formatRound(colnames(dt)[2:disp.col.max], 0)
      } else if (dttype == "mean_with_MOE") {
        sketch.dtstyle.exp <- FUN_dt.container.tblMOE.dtstyle(dt, EV_REACT_varsXAlias(), EV_REACT_varsYAlias(), "mean")
        FUN_xtab.create.DT(dt, moe = T, sketch.dtstyle.exp, sc.idx, disp.col.max, sc.cols) %>%
          formatRound(cols.fmt, 2)
      }
    }
  })



  output$ui_xtab_tbl <- renderUI({
    div(DT::dataTableOutput("xtab_tbl"), style = "font-size: 95%; width: 85%", class = "visual-display", )
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


  # Crosstab Generator Download ---------------------------------------------

  # Enable/Disable download button
  v <- reactiveValues(
    xtabxcol = NULL,
    xtabycol = NULL,
    xtabgo = 0,
    xtabfltrsea = F
  )

  observeEvent(input$xtab_go, {
    v$xtabxcol <- input$xtab_xcol
    v$xtabycol <- input$xtab_ycol
    v$xtabgo <- v$xtabgo + 1
    v$xtabfltrsea <- input$xtab_fltr_sea
  })

  observe({
    if (v$xtabgo == 0 || (v$xtabycol != input$xtab_ycol) || (v$xtabxcol != input$xtab_xcol) || (v$xtabfltrsea != input$xtab_fltr_sea)) {
      disable("xtab_download")
    } else if (v$xtabgo > 0) {
      enable("xtab_download")
    }
  })


  output$xtab_download <- downloadHandler(
    filename = function() {
      paste0("HHSurvey2017_19_", EV_REACT_varsXAlias(), "_by_", EV_REACT_varsYAlias(), "_", EV_REACT_xtabCaption(), ".xlsx")
    },
    content = function(file) {
      write.xlsx(REACT_xtabDownloadOutput(), file)
    }
  )

  # Simple Table ------------------------------------------------------------

  # show/hide vars definition
  observe({
    onclick(
      "stabXtoggleAdvanced",
      toggle(id = "stabXAdvanced", anim = TRUE)
    )
  })

  output$stab_xcol_det <- renderText({
    xvar.det <- variables.lu[variable %in% input$stab_xcol, .(detail)]
    unique(xvar.det$detail)
  })

  # variable X alias


  output$ui_stab_xcol <- renderUI({
    selectInput(
      "stab_xcol",
      "Variable",
      REACT_varsListX()
    )
  })



  output$ui_stab_res_type_title <- renderUI({
    h4(EV_REACT_stabCaption())
  })

  # Simple Table Data Wrangling ---------------------------------------------







  output$stab_tbl <- DT::renderDataTable({
    colors <- list(ltgrey = "#bdbdc3", dkgrey = "#343439")
    dt <- REACT_EV_REACT_stabTable.DT()

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

  # Simple Table Visuals -----------------------------------------------------




  output$stab_vis <- renderPlotly({
    xlabel <- EV_REACT_stab.varsXAlias() # first dim
    dttype <- input$stab_dtype_rbtns
    selection <- names(dtype.choice[dtype.choice %in% dttype])
    geog.caption <- EV_REACT_stabCaption()

    if (dttype %in% col.headers) {
      dt <- REACT_stabVisTable()[type %in% selection, ]
    } else {
      if (dttype == "share_with_MOE") dt <- REACT_stabVisTable.shareMOE()
      if (dttype == "estimate_with_MOE") dt <- REACT_stabVisTable.estMOE()
    }

    l <- length(stabXValues()$ValueText)
    if (l == 0) l <- length(unique(dt$value)) # evaluate if values are not in lookup (length 0)

    if (dttype == "share") {
      ifelse(l < 10, p <- stab.plot.bar(dt, "percent", xlabel, geog.caption), p <- stab.plot.bar2(dt, "percent", xlabel, geog.caption))
      return(p)
    } else if (dttype %in% c("estimate", "sample_count", "N_HH")) {
      ifelse(l < 10, p <- stab.plot.bar(dt, "nominal", xlabel, geog.caption), p <- stab.plot.bar2(dt, "nominal", xlabel, geog.caption))
      return(p)
    } else if (dttype == "share_with_MOE") {
      ifelse(l < 10, p <- stab.plot.bar.moe(dt, "percent", xlabel, geog.caption), p <- stab.plot.bar2.moe(dt, "percent", xlabel, geog.caption))
      return(p)
    } else if (dttype == "estimate_with_MOE") {
      ifelse(l < 10, p <- stab.plot.bar.moe(dt, "nominal", xlabel, geog.caption), p <- stab.plot.bar2.moe(dt, "nominal", xlabel, geog.caption))
      return(p)
    } else {
      return(NULL)
    }
  })

  output$ui_stab_tbl <- renderUI({
    # if (EV_REACT_stabTableType()$Type == 'dimension') {
    div(DT::dataTableOutput("stab_tbl"), style = "font-size: 95%; width: 85%")
    # } #else {
    #   #div(p('Tabular results not available. This functionality is in progress.'),
    #    style = 'display: flex; justify-content: center; align-items: center; margin-top: 5em;')
    #  }
  })



  # return values associated with category selected
  output$ui_xtab_xcol <- renderUI({
    selectInput(
      "xtab_xcol",
      "Variable",
      REACT_varsListX()
    )
  })

  # return values associated with category selected
  output$ui_xtab_ycol <- renderUI({
    selectInput("xtab_ycol",
                "Variable",
                REACT_varsListY(),
                selected = REACT_varsListY()[[2]]
    )
  })
  # Simple Table Map --------------------------------------------------------


  # stabToMap <- eventReactive(input$stab_go, {
  #   # TEST!!!!!!!!!!!!!!!!!!!!!!
  #   ifelse(str_detect(input$stab_xcol, "puma10$"), TRUE, FALSE)
  # })

  # map.colorBins <- function(diffcolumn){
  #   rng <- range(diffcolumn)
  #   if (rng[1] < 0 & rng[2] > 0){
  #     diff.range <- "both"
  #     bins.from.positive <- abs(rng[2]) > abs(rng[1])
  #   } else if (rng[1] >=0 & rng[2] > 0){
  #     diff.range <- "pos"
  #   } else if (rng[1] < 0 & rng[2] < 0){
  #     diff.range <- "neg"
  #   } else {
  #     diff.range <- "none"
  #   }
  #   max.bin <- max(abs(rng))
  #   round.to <- 10^floor(log10(max.bin))
  #   # round maximum to the nearest 100 or 1000 or whatever is appropriate (determined by the log10)
  #   max.bin <- ceiling(max.bin/round.to)*round.to
  #   absbreaks <- (sqrt(max.bin)*c(0.1, 0.2,0.4, 0.6, 0.8, 1))^2 # breaks on sqrt scale
  #
  #   if (diff.range == "both"){
  #     color <- c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#ffffff", "#f7f7f7",
  #                "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f")
  #     bin <- c(-rev(absbreaks), absbreaks)
  #   } else if (diff.range == "pos"){
  #     color <- "Reds"
  #     bin <- c(0, absbreaks)
  #   } else if (diff.range == "neg"){
  #     color <- "Blues"
  #     bin <- c(-rev(absbreaks), 0)
  #   } else if (diff.range == "none"){
  #     color <- "transparent"
  #     bin <- c(0, 1)
  #   }
  #   return(list(color=color, bin=bin))
  # }

  # output$stab_map <- renderLeaflet({
  #   # TEST!!!!!!!!!!!!!!!!!!!!!!!!
  #   if (stabToMap()) {
  #     xlabel <- stab.varsXAlias() # first dim
  #     dttype <- input$stab_dtype_rbtns
  #     selection <- names(dtype.choice[dtype.choice %in% dttype])
  #     geog.caption <- stabCaption()
  #
  #     if (dttype %in% col.headers) {
  #       dt <- stabVisTable()[type %in% selection, ]
  #     } else {
  #       if (dttype == "share_with_MOE") dt <- stabVisTable.shareMOE()
  #       if (dttype == "estimate_with_MOE") dt <- stabVisTable.estMOE()
  #     }
  #
  #     # join data to shapefile (remove extra cols)
  #     shp <- sp::merge(puma.shape, dt, by.x = "PUMACE10", by.y = "value")
  #
  #     # put into leaflet
  #     colorBinResult <- map.colorBins(shp$result)
  #     pal <- colorBin(palette = colorBinResult$color, bins = colorBinResult$bin)
  #
  #     m <- leaflet(data = shp) %>%
  #       addProviderTiles("CartoDB.Positron") %>%
  #       addPolygons(fillColor = ~pal(result),
  #                   fillOpacity = 0.7,
  #                   stroke = T,
  #                   color = "#8a8a95",
  #                   weight = 2) %>%
  #       addLegend("topright",
  #                 pal = pal,
  #                 values = ~result,
  #                 title = paste0(geog.caption, ": <br>", selection, " of ", xlabel),
  #                 opacity = 1)
  #   } else {
  #     m <- NULL
  #   }
  #
  #   return(m)
  # })

  output$ui_stab_vis <- renderUI({
    # ORIGINAL!!!!!!!!!!!!!!!!!!!!!!
    plotlyOutput("stab_vis", width = "85%")

    # if (stabToMap()) {
    #   # TEST!!!!!!!!!!!!!!!!!!!!!!!!
    #   tabsetPanel(type = "tabs",
    #               tabPanel("Chart", plotlyOutput("stab_vis", width = "85%")), # end tabPanel
    #               tabPanel("Map",
    #                        leafletOutput("stab_map")#,
    #                        # div(
    #                        #
    #                        # style = 'margin-top: 2rem;'
    #                        # )
    #                        )
    #               ) # end tabsetPanel
    # } else {
    #   plotlyOutput("stab_vis", width = "85%")
    # }
  })


  # Simple Table Download ---------------------------------------------------

  # Enable/Disable Download button
  vs <- reactiveValues(
    stabxcol = NULL,
    stabgo = 0,
    stabfltrsea = F
  )

  observeEvent(input$stab_go, {
    vs$stabxcol <- input$stab_xcol
    vs$stabgo <- vs$stabgo + 1
    vs$stabfltrsea <- input$stab_fltr_sea
  })

  observe({
    if (vs$stabgo == 0 || (vs$stabxcol != input$stab_xcol) || (vs$stabfltrsea != input$stab_fltr_sea)) {
      disable("stab_download")
    } else if (vs$stabgo > 0) {
      enable("stab_download")
    }
  })


  output$stab_download <- downloadHandler(
    filename = function() {
      paste0("HHSurvey2017_19_", EV_REACT_stab.varsXAlias(), "_", EV_REACT_stabCaption(), ".xlsx")
    },
    content = function(file) {
      # write.xlsx(EV_REACT_stabTable(), file)
      write.xlsx(REACT_stabDownloadOutput(), file)
    }
  )
}
