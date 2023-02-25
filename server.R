server <- function(input, output, session) {

  run_choice_server('runChoice_multi', root_dir = rund)
  
  paths <- eventReactive(input$`runChoice_multi-go`, {
    # return absolute paths and names for all runs of interest
    
    runs <- input$`runChoice_multi-allRuns`
    runnames <- get_runnames(runs)
    names(runs) <- runnames
    return(runs)
  })
  
  runChoiceModal <- modalDialog(
    run_choice_ui('runChoice_multi', 'Run Choices', 'Select Runs', TRUE),
    title = 'Select Runs',
    footer = modalButton('Exit', icon = icon('remove')),
    easyClose = TRUE
  )
  
  # Show the model on start up ...
  showModal(runChoiceModal)
  
  observeEvent(input$modal, {
    # user selects all runs of interest via modal pop-up
    # updateSelectizeInput(session, "runChoice_multi", choices = input$`runChoice_multi-allRuns`, selected = input$`runChoice_multi-allRuns`, server = TRUE)
    showModal(runChoiceModal)
  })
  
  # One-Run ----
  
  
  observeEvent(input$`runChoice_multi-go`, {
    # widgets
    one_run_widgets_server('mismatch', paths())
    one_run_widgets_server('spPlaces', paths())
    dec_widgets_server('dec', paths())
    growth_widgets_server('growth', paths(), baseyears(), alldt())
  })
  
  observeEvent(input$`mismatch-go`, {
    ct_mismatch_server('mismatchContent', input$`mismatch-run`, paths(), alldt(), baseyears(), input$`mismatch-year`)
  })
  
  observeEvent(input$`spPlaces-go`, {
    sp_places_server('spPlacesContent', input$`spPlaces-run`, paths(), alldt(), baseyears(), input$`spPlaces-year`)
  })
  
  observeEvent(input$`dec-go`, {
    dec_server('decContent', input$`dec-run`, paths(), alldt(), baseyears(), input$`dec-year`, input$`dec-abs`, input$`dec-per`)
  })
  
  observeEvent(input$`growth-go`, {
    growth_plot_map_tbl_server('growthContent',
                               input$`growth-run`,
                               input$`growth-geography`,
                               input$`growth-structure`,
                               input$`growth-indicator`,
                               input$`growth-years`,
                               input$`growth-go`,
                               alldt(), 
                               strdt(),
                               paths(),
                               baseyears()
                               )
  })

  # Multi-Run ----
  
  
  observeEvent(input$`runChoice_multi-go`, {
    # widgets
    topsheet_widgets_server('topSheet', paths())
    runcomp_widgets_server('runComp', paths())
  })
  
  observeEvent(input$`topSheet-go`, {
    topsheet_server('topSheetContent', tsTable(), alldt(), input$`topSheet-runs`, input$`topSheet-year`,  baseyears(), paths())
  })
  
  ## Run Comparison ----
  

  observeEvent(input$`runComp-go`, {
    # return Run Comparison content
    
    runcomp_plot_map_tbl_server('runCompContent', 
                                input$`runComp-runs`,
                                input$`runComp-geography`,
                                input$`runComp-structure`,
                                input$`runComp-indicator`,
                                input$`runComp-year`,
                                input$`runComp-go`,
                                alldt(), 
                                strdt(),
                                paths(),
                                baseyears()
    ) 
  })
  
  # Time Series ----
  
  observeEvent(input$`runChoice_multi-go`, {
    # widgets
    timeseries_widgets_server('ts', paths())
  })
  
  observeEvent(input$`ts-go`,{
    # return time series content
    timeseries_plot_server('tsContent', 
                           runs = input$`ts-runs`,
                           geog = input$`ts-geog`,
                           largearea = input$`ts-largeArea`,
                           go = input$`ts-go`, 
                           alldata = alldt(), 
                           ctrlhctdata = ctrlhctdt(),
                           paths = paths()
    )
  })
  
 
  # Data ----
  
  baseyears <- reactive({
    # returns a data frame of runs and their baseyears

    # for each run, find its baseyear
    a <- alldt()[, lapply(.SD, sum), .SDcols = patterns("^yr"), by = .(run)]
    b.yrs <- names(a[,2:ncol(a)])[max.col(a[,2:ncol(a)] != 0, ties.method = 'first')]

    # return a df and subset for chosen runs
    b <- a[, .(run)][, baseyear := b.yrs]
    b[run %in% names(paths())]
  })
  
  tsTable <- eventReactive(input$`topSheet-go`, {
    # create county level base table for Top Sheet
    
    runs <- get_runnames(input$`topSheet-runs`)
    yrs.cols <- c(unique(baseyears()$baseyear), paste0("yr",  input$`topSheet-year`))
    
    # gather zone level data and aggregate by county
    t <- merge(alldt()[geography == 'zone' & (run %in% runs)], zone.lookup, by.x = "name_id", by.y = "zone_id")
    t.cnty <- t[, lapply(.SD, sum), by = list(County, indicator, run), .SDcols = yrs.cols]
    
    # create regional summary table and append to county data
    t.reg <- t.cnty[, lapply(.SD, sum), by = list(indicator, run), .SDcols = yrs.cols][, County := "Sub-Total: Region"]
    rbindlist(list(t.cnty, t.reg), use.names = TRUE)

  })
  
  strdt <- eventReactive(input$`runChoice_multi-go`, {
    # build structure type (sf/mf) indicators source table
    
    runs <- paths()
    stypedt <- NULL
    for (r in 1:length(runs)){
      structure.files <- as.list(list.files(file.path(runs[r], "indicators"),
                                            pattern = 'dataset_table__DU_and_HH_by_bld_type_by(_)*(\\w+)*(_)*(\\d+)*\\.tab'))
      if (length(structure.files) > 0){
        for (f in 1:length(structure.files)){
          geo <- str_extract(structure.files[f], "^(\\w+)__dataset") %>% strsplit("__") %>% unlist()
          
          yr <- str_extract(structure.files[f], "(\\d+)")
          dt0 <- read.table(file.path(runs[r], 'indicators', structure.files[f]), header = T, sep = "\t", fill = TRUE) 
          
          setDT(dt0)
          
          setnames(dt0, colnames(dt0), str_match(colnames(dt0), "(\\w+_\\w+)[^_^\\d+]")[,1])
          setnames(dt0, colnames(dt0)[1], "name_id")
          dt <- melt.data.table(dt0, id.vars = colnames(dt0)[1], measure.vars = colnames(dt0)[2:ncol(dt0)], variable.name = "description", value.name = "estimate")
          dt[, `:=` (run = names(runs)[r], geography = geo[1], year = yr, indicator = str_extract(description, "^\\w{2}"), type = str_match(description, "\\w+_(\\w+$)")[,2])]
          t0 <- dcast.data.table(dt, name_id + run + geography + year + indicator ~ type, value.var = 'estimate')
          ifelse(is.null(stypedt), stypedt <- t0, stypedt <- rbind(stypedt, t0))
        } # end structure.files loop
      } else if (length(structure.files) == 0) {
        next
      } # end conditional
    } # end runs loop

    dt1 <- stypedt[, multifamily := MF + CO][, singlefamily := Total - multifamily]
    dt2 <- melt.data.table(dt1, id.vars = colnames(dt1)[1:5], measure.vars = colnames(dt1)[(ncol(dt1)-1):ncol(dt1)], variable.name = 'strtype', value.name = "estimate")
    ind.name <- c("HH" = "Households", "DU" = "Residential Units")
    dt2$indicator <- ind.name[dt2$indicator]

    return(dt2)
  })
  
  
  alldt <- eventReactive(input$`runChoice_multi-go`,{
    # build general attributes source table
    
    # extract runs from abs paths
    runs <- paths()
    
    # initialize main table
    df <- data.frame(matrix(ncol = length(years) + 4, nrow = 0)) 
    df.colnames <- c("name_id", paste0("yr", years), "indicator", "geography", "run") 
    colnames(df) <- df.colnames
    setDT(df)
    
    for (r in 1:length(runs)) {
      for (a in 1:length(geography)){
        for (i in 1:length(attribute)){
          filename <- paste0(geography[a],'__',"table",'__',attribute[i], '.csv')
          dt <- read.csv(file.path(runs[r], "indicators",filename), header = TRUE, sep = ",")
          colnames(dt)[2: ncol(dt)] <- str_replace(colnames(dt)[2: ncol(dt)], '\\w+_', 'yr') # rename columns
          colnames(dt)[1] <- str_replace(colnames(dt)[1], '\\w+_', 'name_')
          dt$indicator <- switch(attribute[i],
                                        "population"="Total Population",
                                        "households"="Households",
                                        "employment"="Employment",
                                        "residential_units"="Residential Units")
          
          dt$geography <- geography[a]
          dt$run <- names(runs)[r]
          # dt$run <- runnames[r]
          setDT(dt)
          df <- rbindlist(list(df, dt), use.names = TRUE, fill = TRUE)
        } # end of attribute loop
      } # end of geography loop
    } # end of runs loop

    df[is.na(df)] <- 0
    
    return(df)
  })
  
  ctrlhctdt <- eventReactive(input$`runChoice_multi-go`,{
    # build control HCT source table
    
    # extract runs from abs paths
    runs <- paths()
    attribute <- attribute[attribute != 'residential_units'] # res units doesn't exist yet
    
    d <- NULL

    for (r in 1:length(runs)) {
      for (i in 1:length(attribute)){
        filename <- paste0('control__',"table",'__',attribute[i], 'An.csv')
        
        dt <- fread(file.path(runs[r], "indicators", filename), header = TRUE, sep = ",")
        dt <- melt(dt, id.vars = 'control_id', variable.name = 'attribute', value.name = 'estimate')
        dt[, run := names(runs)[r]]
        ifelse(is.null(dt), d <- dt, d <- rbindlist(list(d, dt)))
      }
    }
    
    d[, `:=`(year = str_extract(attribute, "\\d+"),
              attribute = str_extract(attribute, '\\w+(?=A)'))]
    
  })
  
  
  # output$strdtavail <- reactive({
  #   # Check if runs 1 & 2 exist in strdt(), if not conditional panel disabled
  #   strdt <- strdt()
  # 
  #   runnames <- map(input$`runComp-runs`, ~str_split(.x, '/')) %>% flatten() %>% map(., ~pluck(.x, length(.x))) %>% unlist()
  #   c1 <- runnames[1] %in% strdt[, run]
  #   c2 <- runnames[2] %in% strdt[, run]
  #   v <- c1 == c2
  #   return(v)
  # })
  # outputOptions(output, 'strdtavail', suspendWhenHidden = FALSE)
  # condition = "(input.indicator == 'Residential Units' | input.indicator == 'Households') && output.strdtavail &&
  #                                       input.geography == 'faz'",
}