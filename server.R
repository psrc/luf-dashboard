# Define server logic required
server <- function(input, output, session) {

  run_choice_server('runChoice_one', root_dir = rund)
  run_choice_server('runChoice_multi', root_dir = rund)
  
  # display absolute paths
  paths <- eventReactive(input$`runChoice_multi-go`, {
    runs <- input$`runChoice_multi-allRuns`
    runnames <- map(runs, ~str_split(.x, '/')) %>% flatten() %>% map(., ~pluck(.x, length(.x))) %>% unlist() 
    names(runs) <- runnames
    return(runs)
  })
  
  output$disp_multi <- renderDT({
    # run_comparison()
    # strdt()
    cTable()
    # datatable(alldt())
  })

  # return Run Comparison sidebar controls
  multi_scat_map_data_server('runComp', alldata = alldt(), paths())
 
  run_comparison <- eventReactive(input$`runComp-go`,{
    year <- paste0("yr", input$`runComp-year`)
    # browser()
    # for each run, find its baseyear
    a <- alldt()[, lapply(.SD, sum), .SDcols = patterns("^yr"), by = .(run)]
    b.yrs <- names(a[,2:ncol(a)])[max.col(a[,2:ncol(a)] != 0, ties.method = 'first')]
    
    # return a df and subset for chosen runs
    b <- a[, .(run)][, baseyear := b.yrs]
    b[run %in% names(paths())]
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
  
  
 
  strdt <- eventReactive(input$`runComp-go`, {
    # build structure type (sf/mf) indicators source table
    
    attribute <- c("population", "households","employment", "residential_units")
    geography <- c( "zone", "faz", "city")
    years <- seq(2014, 2050)
    luv.years <- c(2014, 2015, 2020, 2025, 2030, 2035, 2040)
    
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
  
  # build general attributes source table
  alldt <- eventReactive(input$`runComp-go`,{ ########may need to change the trigger to input$`runChoice-multi`

    attribute <- c("population", "households","employment", "residential_units")
    geography <- c( "zone", "faz", "city")
    years <- seq(2014, 2050)
    luv.years <- c(2014, 2015, 2020, 2025, 2030, 2035, 2040)
    
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
  
  cTable <- eventReactive(input$`runComp-go`, {
    strdt <- strdt()
    alldt <- alldt()
    byears <- run_comparison()
    
    runnames <- get_runnames(input$`runComp-runs`)
    # runnames <- map(input$`runComp-runs`, ~str_split(.x, '/')) %>% flatten() %>% map(., ~pluck(.x, length(.x))) %>% unlist()
    
    if (is.null(input$`runComp-structure`) | input$`runComp-structure` == "All" | (input$`runComp-indicator` %in% c("Total Population", "Employment")) |
        (input$`runComp-indicator` %in% c("Households", "Residential Units") & input$`runComp-geography` %in% c("zone", "city")) ){

      # run 1
      b1 <- byears[run == runnames[1],][['baseyear']]
      dt1 <- alldt[run == runnames[1] & geography == input$`runComp-geography` & indicator == input$`runComp-indicator`,
                   .(name_id, geography, indicator, get(b1), get(paste0('yr',input$`runComp-year`)))]
      setnames(dt1, dt1[,c((ncol(dt1)-1), ncol(dt1))], c('base_estrun1', 'estrun1'))

      # run 2
      b2 <- byears[run == runnames[2],][['baseyear']]
      dt2 <- alldt[run == runnames[2] & geography == input$`runComp-geography` & indicator == input$`runComp-indicator`,
                   .(name_id, get(b2),get(paste0('yr', input$`runComp-year`)))]
      setnames(dt2, dt2[,c((ncol(dt2)-1), ncol(dt2))], c('base_estrun2', 'estrun2'))

      dt <- merge(dt1, dt2, by = 'name_id')
    } else {
      # run 1

      b1 <- str_extract(byears[run == runnames[1],][['baseyear']], "\\d+")
      dt1 <- strdt[run == runnames[1] & geography == input$`runComp-geography` & (year == b1 | year == input$`runComp-year`) & indicator == input$`runComp-indicator` & strtype == input$`runComp-structure`]
      dt1.cast <- dcast.data.table(dt1, name_id + indicator + geography ~ year, value.var = "estimate")
      setnames(dt1.cast, colnames(dt1.cast)[4:5], c('base_estrun1', 'estrun1'))

      # run 2
      b2 <- str_extract(byears[run == runnames[2],][['baseyear']], "\\d+")
      dt2 <- strdt[run == runnames[2] & geography == input$`runComp-geography` & (year == b2 | year == input$`runComp-year`)  & indicator == input$`runComp-indicator` & strtype == input$`runComp-structure`]
      dt2.cast <- dcast.data.table(dt2, name_id ~ year, value.var = "estimate")
      setnames(dt2.cast, colnames(dt2.cast)[2:3], c('base_estrun2', 'estrun2'))
      dt <- merge(dt1.cast, dt2.cast, by = 'name_id')
    }
    dt[,"diff" := (estrun1-estrun2)]

    # switch(as.integer(input$`runComp-geography`),
    #        merge(dt, zone.lookup, by.x = "name_id", by.y = "zone_id") %>% merge(faz.lookup, by = c("faz_id", "County")),
    #        merge(dt, faz.lookup, by.x = "name_id", by.y = "faz_id"),
    #        merge(dt, city.lookup, by.x = "name_id", by.y = "city_id") %>% setnames("city_name", "Name")
    # )
    return(dt)
  })

  # outputOptions(output, 'strdtavail', suspendWhenHidden = FALSE)
}