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
    equity_widgets_server('equity', paths(), baseyears(), alldt())
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
                               ctrldt(),
                               paths(),
                               baseyears()
                               )
  })
  
  observeEvent(input$`equity-go`, {
    equity_server('equityContent',
                               input$`equity-run`,
                               input$`equity-geography`,
                               input$`equity-indicator`,
                               input$`equity-valtype`,
                               input$`equity-go`,
                               eqtdt(),
                               #alldt(), 
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
                                ctrldt(),
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
                           tsyears = input$`ts-years`,
                           largearea = input$`ts-largeArea`,
                           largeareahct = input$`ts-largeAreaHct`,
                           largeareafaz = input$`ts-largeAreaFaz`,
                           go = input$`ts-go`, 
                           alldata = alldt(), 
                           ctrldata = ctrldt(),
                           ctrlhctdata = ctrlhctdt(),
                           #cities_an_data = cities_an_dt(),
                           paths = paths()
    )
  })
  
  # Development Capacity ----
  
  observeEvent(input$`runChoice_multi-go`, {
    # widgets
    dev_cap_widgets_server('devCap', 
                           paths = paths()#, 
                           # capdt = capdt(), 
                           # devdt = devdt()
                           )

  })
  
  observeEvent(input$`devCap-go`,{
    dev_cap_server('devCapContent', 
                   run = input$`devCap-run`, 
                   geog = input$`devCap-geography`, 
                   inputyear = input$`devCap-year`, 
                   go = input$`devCap-go`, 
                   paths = paths(), 
                   devdata = devdt(), 
                   capdata = capdt(),
                   centers = centers.shape
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
  
  ## alldt ----
  
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
          full.filename <- get_full_table_file_name(geography[a], attribute[i], runs[r])
          if(!file.exists(full.filename)) next
          dt <- fread(full.filename, header = TRUE, sep = ",")
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
          #setDT(dt)
          df <- rbindlist(list(df, dt), use.names = TRUE, fill = TRUE)
        } # end of attribute loop
      } # end of geography loop
    } # end of runs loop

    df[is.na(df)] <- 0
    
    return(df)
  })
  
  ctrldt <- eventReactive(input$`runChoice_multi-go`,{
    # build control source table
    
    # extract runs from abs paths
    runs <- paths()
    #attribute <- attribute[attribute != 'residential_units'] # res units doesn't exist yet
    
    d <- NULL

    for (r in 1:length(runs)) {
      for (i in 1:length(attribute)){
        filename <- get_full_table_file_name("control", attribute[i], runs[r])
        dt <- fread(filename, header = TRUE, sep = ",")
        dt <- melt(dt, id.vars = 'control_id', variable.name = 'indicator', value.name = 'estimate')
        dt[, run := names(runs)[r]]
        
        ifelse(is.null(dt), d <- dt, d <- rbindlist(list(d, dt)))
      }
    }
    
    d[, `:=`(year = as.integer(str_extract(indicator, "\\d+")),
             indicator = str_extract(indicator, '\\w+(?=A)'),
             geography = 'Control')]
    d[, indicator := fcase(indicator == "population", "Total Population",
                           indicator == "households", "Households",
                           indicator == "employment", "Employment",
                           indicator == "residential_units", "Residential Units")]
    setnames(d, 'control_id', 'name_id')
    return(d)
    
  })
  
  ctrlhctdt <- eventReactive(input$`runChoice_multi-go`,{
    # build control HCT source table
    
    # extract runs from abs paths
    runs <- paths()
    #attribute <- attribute[attribute != 'residential_units'] # res units doesn't exist yet
    
    d <- NULL
    
    for (r in 1:length(runs)) {
      for (i in 1:length(attribute)){
        filename <- get_full_table_file_name("control_hct", attribute[i], runs[r])
        dt <- fread(filename, header = TRUE, sep = ",")
        dt <- melt(dt, id.vars = 'control_hct_id', variable.name = 'indicator', value.name = 'estimate')
        dt[, run := names(runs)[r]]
        
        ifelse(is.null(dt), d <- dt, d <- rbindlist(list(d, dt)))
      }
    }
    
    d[, `:=`(year = as.integer(str_extract(indicator, "\\d+")),
             indicator = str_extract(indicator, '\\w+(?=A)'),
             geography = 'Control HCT')]
    d[, indicator := fcase(indicator == "population", "Total Population",
                           indicator == "households", "Households",
                           indicator == "employment", "Employment",
                           indicator == "residential_units", "Residential Units")]
    setnames(d, 'control_hct_id', 'name_id')
    return(d)
    
  })
  
  # Development Capacity tables ----
  
  capdt <- eventReactive(input$`runChoice_multi-go`, {
    # extract runs from abs paths
    runs <- paths()

    cap.geography <- c(geography, "growth_center")
    cap.type <- c("max_dev", "max_dev_nonresidential", "max_dev_residential")
    
    cap.table <- NULL

    for (r in 1:length(runs)){
      cap.files <- as.list(list.files(file.path(runs[r], "indicators"), pattern = paste0("max_dev(_\\w+)*", ".csv")))
      if (length(cap.files) >= 1){
        for (g in 1:length(cap.geography)){
          for (c in 1:length(cap.type)){
            cap.tbl <- NULL
            cap.file <- file.path(runs[r],"indicators", paste0(cap.geography[g], '__table__', cap.type[c], "_capacity", ".csv"))
            if(!file.exists(cap.file)) next
            cap.tbl <- read.csv(cap.file, header = TRUE, sep = ",")
            cap.tbl$captype <- switch(cap.type[c],
                                      "max_dev" = "Total",
                                      "max_dev_nonresidential" = "Non-Residential",
                                      "max_dev_residential" = "Residential")
            cap.tbl$geography <- cap.geography[g]
            cap.tbl$year <- str_sub(names(cap.tbl)[2], -4)
            cap.tbl$run <- runs[r]
            colnames(cap.tbl)[1] <- "name_id"
            colnames(cap.tbl)[2] <- "capacity"
            ifelse(is.null(cap.table),
                   cap.table <- cap.tbl,
                   cap.table <- rbind(cap.table, cap.tbl))
          } # end of cap.type loop
        } # end of cap.geography loop
      } else if (length(cap.files) < 1){
        next
      } # end conditional
    } # end of runnames loop

    return(as.data.table(cap.table))
  })
  
  devdt <- eventReactive(input$`runChoice_multi-go`, {
    # extract runs from abs paths
    runs <- paths()

    cap.geography <- c(geography, "growth_center")
    dev.type <- c("residential_units", "building_sqft", "nonres_sqft")

    dev.table <- NULL

    for (r in 1:length(runs)){
      dev.files <- as.list(list.files(file.path(runs[r], "indicators"), pattern = paste0("sqft", ".csv")))
      if (length(dev.files) >= 1){
        for (g in 1:length(cap.geography)){
          for (d in 1:length(dev.type)){
            dev.tbl <- NULL
            dev.file <- file.path(runs[r],"indicators", paste0(cap.geography[g], '__table__', dev.type[d], ".csv"))
            if(!file.exists(dev.file)) next
            dev.tbl <- fread(dev.file, header = TRUE)
            dev.tbl.m <- melt(dev.tbl, id.vars = c(paste0(cap.geography[g], "_id")), measure.vars = names(dev.tbl)[2:ncol(dev.tbl)])
            dev.tbl.m[, `:=` (devtype = switch(dev.type[d],
                                               "residential_units" = "Residential Units",
                                               "building_sqft" = "Building Sqft",
                                               "nonres_sqft" = "Non-Residential Sqft"),
                              year = str_sub(variable, -4),
                              geography = cap.geography[g],
                              run = runs[r])]
            setnames(dev.tbl.m, paste0(cap.geography[g], "_id"), "name_id")
            setnames(dev.tbl.m, "value", "estimate")
            ifelse(is.null(dev.table),
                   dev.table <- dev.tbl.m,
                   dev.table <- rbind(dev.table, dev.tbl.m))
          } # end of dev.type loop
        } # end cap.geography loop
      } else if (length(dev.files) < 1){
        next
      } # end conditional
    } # end of runnames loop
    
    return(dev.table)
  })
  
  eqtdt <- eventReactive(input$`runChoice_multi-go`, {
    # equity data
    runs <- paths()
    alldata <- NULL
    for (r in 1:length(runs)){
      hh.files <- as.list(list.files(file.path(runs[r], "indicators"),
                                            pattern = 'dataset_table__(households|persons)_characteristics(_)*(\\w+)*(_)*(\\d+)*\\.tab'))
      #pers.files <- as.list(list.files(file.path(runs[r], "indicators"),
      #                               pattern = 'dataset_table__persons_characteristics(_)*(\\w+)*(_)*(\\d+)*\\.tab'))
      
      if (length(hh.files) == 0) next

      for (f in 1:length(hh.files)){
          geo <- str_extract(hh.files[f], "^(\\w+)__dataset") %>% strsplit("__") %>% unlist()
          
          yr <- str_extract(hh.files[f], "(\\d+)")
          dt0 <- fread(file.path(runs[r], 'indicators', hh.files[f])) 
          
          
          setnames(dt0, colnames(dt0), str_match(colnames(dt0), "(\\w+_\\w+)[^_^\\d+]")[,1])
          setnames(dt0, colnames(dt0)[1], "name_id")
          
          # convert HH measures to percentage
          if("low_income" %in% colnames(dt0) && "hh_total" %in% colnames(dt0))
            dt0[, low_income := round(low_income / hh_total * 100, 1)]
          if("high_income" %in% colnames(dt0) && "hh_total" %in% colnames(dt0))
            dt0[, high_income := round(high_income / hh_total * 100, 1)]
          if("hh_total" %in% colnames(dt0)) dt0[, hh_total := NULL]
          
          # convert persons measures to percentage
          if("pop_white" %in% colnames(dt0) && "pop_total" %in% colnames(dt0)) {
            dt0[, pop_non_white := round((pop_total - pop_white) / pop_total * 100, 1)]
            dt0[, pop_white := round(pop_white / pop_total * 100, 1)]
          }
          if("pop_black" %in% colnames(dt0) && "pop_total" %in% colnames(dt0))
            dt0[, pop_black := round(pop_black / pop_total * 100, 1)]
          if("pop_asian" %in% colnames(dt0) && "pop_total" %in% colnames(dt0))
            dt0[, pop_asian := round(pop_asian / pop_total * 100, 1)]
          if("pop_hsp" %in% colnames(dt0) && "pop_total" %in% colnames(dt0))
            dt0[, pop_hsp := round(pop_hsp / pop_total * 100, 1)]
          if("pop_total" %in% colnames(dt0)) dt0[, pop_total := NULL]
          
          dt <- melt.data.table(dt0, id.vars = colnames(dt0)[1], measure.vars = colnames(dt0)[2:ncol(dt0)], variable.name = "indicator", value.name = "estimate")
          dt[, `:=` (run = names(runs)[r], geography = geo[1], year = yr)]
          alldata <- rbind(alldata, dt)
        } # end hh.files loop

    } # end runs loop
    
    #dt1 <- stypedt[, multifamily := MF + CO][, singlefamily := Total - multifamily]
    #dt2 <- melt.data.table(dt1, id.vars = colnames(dt1)[1:5], measure.vars = colnames(dt1)[(ncol(dt1)-1):ncol(dt1)], variable.name = 'strtype', value.name = "estimate")
    #browser()
    #ind.name <- c("HH" = "Households", "DU" = "Residential Units")
    #dt2$indicator <- ind.name[dt2$indicator]
    
    return(alldata)
  })
  
  # cities_an_dt <- eventReactive(input$`runChoice_multi-go`,{
  #   # build annual cities source table
  #   
  #   # extract runs from abs paths
  #   runs <- paths()
  #   
  #   d <- NULL
  #   
  #   for (r in 1:length(runs)) {
  #     for (i in 1:length(attribute)){
  #       filename <- paste0('city__',"table",'__',attribute[i], 'An.csv')
  #       
  #       dt <- fread(file.path(runs[r], "indicators", filename), header = TRUE, sep = ",")
  #       dt <- melt(dt, id.vars = 'city_id', variable.name = 'indicator', value.name = 'estimate')
  #       dt[, run := names(runs)[r]]
  #       ifelse(is.null(dt), d <- dt, d <- rbindlist(list(d, dt)))
  #     }
  #   }
  #   
  #   d[, `:=`(year = str_extract(indicator, "\\d+"),
  #            indicator = str_extract(indicator, '\\w+(?=A)'))]
  # })

}