# This module returns DT formatted tables with table header for 
# the RGs in the Top Sheet

dt_rgs_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('uiTables'))
  )
}

dt_rgs_server <- function(id, paths, runs, tsyear, baseyear) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$uiTables <- renderUI({
      if(is.null(rgsdt())) return(NULL)
      tagList(
        h2('Regional Geographies'),
        DTOutput(ns('table_a')),
        br(),
        h2('Regional Geographies by County'),
        DTOutput(ns('table_b'))
      )
    })
    
    rgsdt <- reactive({
      # compile data for rgs
      
      # gather basic run info  
      #runs <- get_runnames(runs)
      runs <- names(paths)
      runnames <- get_trim_runnames(runs)
      
      # initialize rgs.tables
      rgs.dt <- data.frame(matrix(ncol = length(years) + 3, nrow = 0)) 
      new.colnames <- c("name_id", paste0("yr", years), "indicator", "run") 
      colnames(rgs.dt) <- new.colnames
      setDT(rgs.dt)

      for (r in 1:length(runnames)) {
        for (i in 1:length(attribute)){
          filename <- get_full_table_file_name("city", attribute[i], paths[r])
          datatable <- fread(filename, header = TRUE, sep = ",")
          # aggregate
          cols <- setdiff(colnames(datatable), "city_id")
          datatable[city.lookup, `:=`(rgs_id = i.rgs_id, county_id = i.county_id, county = i.county), on = "city_id"]
          
          datatable.rgs <- datatable[, lapply(.SD, sum), by = "rgs_id", .SDcols = cols]
          colnames(datatable.rgs)[2:ncol(datatable.rgs)] <- str_replace(colnames(datatable.rgs)[2: ncol(datatable.rgs)], '\\w+_', 'yr') # rename columns
          colnames(datatable.rgs)[1] <- str_replace(colnames(datatable.rgs)[1], '\\w+_', 'name_')
          datatable.rgs$indicator <- switch(attribute[i],
                                                 "population"="Total Population",
                                                 "households"="Households",
                                                 "employment"="Employment",
                                                 "residential_units"="Residential Units")
          
          datatable.rgs$run <- runs[r]
          rgs.dt <- rbindlist(list(rgs.dt, datatable.rgs), use.names = TRUE, fill = TRUE)
        } # end of attribute loop
      } # end of runnames loop

      dt <- merge(rgs.dt, rgs.lookup[, .(rgs_id, name = geography_name)], by.x = "name_id", by.y = "rgs_id")
      
      dt[is.na(dt)] <- 0

      return(dt)
    })
    
    cntyrgsdt <- reactive({
      # compile data for rgs by county
      
      # gather basic run info  
      #runs <- get_runnames(runs)
      runs <- names(paths)
      runnames <- get_trim_runnames(runs)
      
      # initialize rgs.table
      cntyrgs.dt <- data.frame(matrix(ncol = length(years) + 4, nrow = 0)) 
      new.colnames <- c("county", "name_id", paste0("yr", years), "indicator", "run") 
      colnames(cntyrgs.dt) <- new.colnames
      setDT(cntyrgs.dt)
      
      for (r in 1:length(runnames)) {
        for (i in 1:length(attribute)){
          filename <- get_full_table_file_name("city", attribute[i], paths[r])
          datatable <- fread(filename, header = TRUE, sep = ",")
          # aggregate
          cols <- setdiff(colnames(datatable), "city_id")
          datatable[city.lookup, `:=`(rgs_id = i.rgs_id, county_id = i.county_id, county = i.county), on = "city_id"]
          
          datatable.cnty.rgs <- datatable[, lapply(.SD, sum), by = c("county", "rgs_id"), .SDcols = cols]
          colnames(datatable.cnty.rgs)[3:ncol(datatable.cnty.rgs)] <- str_replace(colnames(datatable.cnty.rgs)[3: ncol(datatable.cnty.rgs)], '\\w+_', 'yr') # rename columns
          colnames(datatable.cnty.rgs)[2] <- str_replace(colnames(datatable.cnty.rgs)[2], '\\w+_', 'name_')
          datatable.cnty.rgs$indicator <- switch(attribute[i],
                                                 "population"="Total Population",
                                                 "households"="Households",
                                                 "employment"="Employment",
                                                 "residential_units"="Residential Units")
          
          datatable.cnty.rgs$run <- runs[r]
          cntyrgs.dt <- rbindlist(list(cntyrgs.dt, datatable.cnty.rgs), use.names = TRUE, fill = TRUE)
        } # end of attribute loop
      } # end of runnames loop
      
      dt <- merge(cntyrgs.dt, rgs.lookup[, .(rgs_id, name = geography_name)], by.x = "name_id", by.y = "rgs_id")
      
      dt[is.na(dt)] <- 0
      
      return(dt)
    })
    
    tsRGs <- reactive({
      # Filter table and calculate totals
      
      rgsdt <- rgsdt()

      runs <- get_runnames(runs)
      
      b1 <- baseyear[run == runs[1], .(baseyear)][[1]] |> str_extract("\\d+")
      b2 <- baseyear[run == runs[2], .(baseyear)][[1]] |> str_extract("\\d+")
      cols <- paste0("yr", c(b1, b2, tsyear))
      
      t <- rgsdt[(indicator == 'Total Population' | indicator == 'Employment')
                     ][indicator == 'Total Population', indicator := 'Population']
      
      tm <- melt.data.table(t,
                            id.vars = c("name_id", "name", "indicator", "run"),
                            measure.vars = str_subset(colnames(t), "^yr.*"),
                            variable.name = "year")

      tm[, year := str_extract(year, "\\d+")]
      t1 <- tm[((run %in% runs[1]) & (year %in% c(b1, tsyear))) | ((run %in% runs[2]) & (year %in% c(b2, tsyear)))]
    })
    
    tscntyRGs <- reactive({
      # Filter table and calculate totals
      
      rgsdt <- cntyrgsdt()
      
      runs <- get_runnames(runs)
      
      b1 <- baseyear[run == runs[1], .(baseyear)][[1]] |> str_extract("\\d+")
      b2 <- baseyear[run == runs[2], .(baseyear)][[1]] |> str_extract("\\d+")
      cols <- paste0("yr", c(b1, b2, tsyear))
      
      t <- rgsdt[(indicator == 'Total Population' | indicator == 'Employment')
      ][indicator == 'Total Population', indicator := 'Population']
      
      #t[, name := paste(county, name, sep = ": ")][, county := NULL]
      tm <- melt.data.table(t,
                            id.vars = c("name_id", "county", "name", "indicator", "run"),
                            measure.vars = str_subset(colnames(t), "^yr.*"),
                            variable.name = "year")
      tm[, year := str_extract(year, "\\d+")]
      t1 <- tm[((run %in% runs[1]) & (year %in% c(b1, tsyear))) | ((run %in% runs[2]) & (year %in% c(b2, tsyear)))]
    })
    
    output$table_a <- renderDT({
      # Display RGs summary table
      
      tsRGs <- tsRGs()
      tsRGs[, run := get_trim_runnames(run)]
      
      runs <- get_runnames(runs)
      runnames <- get_trim_runnames(runs)

      b1 <- baseyear[run == runs[1], .(baseyear)][[1]] |> str_extract("\\d+")
      b2 <- baseyear[run == runs[2], .(baseyear)][[1]] |> str_extract("\\d+")
      cols <- paste0("yr", c(b1, b2, tsyear))
      
      t <- dcast.data.table(tsRGs, name_id + name ~ year + indicator + run, value.var = "value")[, name_id := NULL]
      setnames(t, "name", "Name")
      t1 <- create.exp.tsTable(t, runs, tsyear, baseyear)
      sketch <- sketch.expanded(colnames(t1)[1], b1, b2, tsyear, runnames[1], runnames[2])
      create.DT.expanded(t1, sketch)
    })
    
    output$table_b <- renderDT({
      # Display RGs by county
      
      tsRGs <- tscntyRGs()
      tsRGs[, run := get_trim_runnames(run)]
      
      runs <- get_runnames(runs)
      runnames <- get_trim_runnames(runs)
      
      b1 <- baseyear[run == runs[1], .(baseyear)][[1]] |> str_extract("\\d+")
      b2 <- baseyear[run == runs[2], .(baseyear)][[1]] |> str_extract("\\d+")
      cols <- paste0("yr", c(b1, b2, tsyear))
      
      t <- dcast.data.table(tsRGs, county + name_id + name ~ year + indicator + run, value.var = "value")
      t[, Name := paste(county, name, sep = ": ")][, `:=`(county = NULL, name_id = NULL, name = NULL)]
      t1 <- create.exp.tsTable(t, runs, tsyear, baseyear)
      sketch <- sketch.expanded(colnames(t1)[1], b1, b2, tsyear, runnames[1], runnames[2])
      create.DT.expanded(t1, sketch)
    })
    
  })
}
