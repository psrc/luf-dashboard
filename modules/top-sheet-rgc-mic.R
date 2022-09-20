# This module returns DT formatted tables with table header for 
# the Largest RGC and MICs in the Top Sheet

dt_centers_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('uiTables'))
  )
}

dt_centers_server <- function(id, paths, runs, tsyear, baseyear) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$uiTables <- renderUI({
      if(is.null(growctrdt())) return(NULL)
      tagList(
        h2('Largest RGCs'),
        DTOutput(ns('table_a')),
        br(),
        h2('MIC'),
        DTOutput(ns('table_b'))
      )
    })
    
    growctrdt <- reactive({
      # compile data for rgcs and mics
      
      # gather basic run info  
      runs <- get_runnames(runs)
      runnames <- get_trim_runnames(runs)
      sel.yrs.col <- c(unique(baseyear$baseyear), paste0("yr", tsyear))
      sel.yr.fl <- str_extract(sel.yrs.col, "\\d+")
      
      # initialize growctr.table
      ctr.dt <- data.frame(matrix(ncol = length(years) + 3, nrow = 0)) #30
      new.colnames <- c("name_id", paste0("yr", years), "indicator", "run") 
      colnames(ctr.dt) <- new.colnames
      setDT(ctr.dt)
 
      for (r in 1:length(runnames)) {
        for (i in 1:length(attribute)){
          filename <- paste0('growth_center__table','__',attribute[i], '.csv')
          datatable <- read.csv(file.path(paths[r],"indicators",filename), header = TRUE, sep = ",")
          colnames(datatable)[2:ncol(datatable)] <- str_replace(colnames(datatable)[2: ncol(datatable)], '\\w+_', 'yr') # rename columns
          colnames(datatable)[1] <- str_replace(colnames(datatable)[1], '\\w+_', 'name_')
          datatable$indicator <- switch(attribute[i],
                                        "population"="Total Population",
                                        "households"="Households",
                                        "employment"="Employment",
                                        "residential_units"="Residential Units")
          
          
          datatable$run <- runs[r]
          setDT(datatable)
          ctr.dt <- rbindlist(list(ctr.dt, datatable), use.names = TRUE, fill = TRUE)
        } # end of attribute loop
      } # end of runnames loop

      dt <- merge(ctr.dt, rgc.lookup[,c("growth_center_id", "name")], by.x = "name_id", by.y = "growth_center_id")
      dt[is.na(dt)] <- 0

      return(dt)
    })
    
    tsGrowthCtr <- reactive({
      # Filter table and calculate totals for largest RGCs
      
      growctrdt <- growctrdt()

      runs <- get_runnames(runs)
      sel.yrs.col <- c(unique(baseyear$baseyear), paste0("yr", tsyear))
      sel.yr.fl <- str_extract(sel.yrs.col, "\\d+")
      
      t <- growctrdt[(indicator == 'Total Population' | indicator == 'Employment') & (run %in% runs)]
      t[indicator == 'Total Population', indicator := 'Population']
      t1 <- t[, lapply(.SD, sum), by = list(name, indicator, run), .SDcols = sel.yrs.col]
    })
    
    output$table_a <- renderDT({
      # Display largest RGCs summary table
      
      tsGrowthCtr <- tsGrowthCtr()
      tsGrowthCtr[, run := get_trim_runnames(run)]
      
      runs <- get_runnames(runs)
      runnames <- get_trim_runnames(runs)
      sel.yrs.col <- c(unique(baseyear$baseyear), paste0("yr", tsyear))
      sel.yr.fl <- str_extract(sel.yrs.col, "\\d+")
      
      lg.rgc <- c("Bellevue", "Everett", "SeaTac", "Seattle Downtown", "Seattle First Hill/Capitol Hill", "Seattle South Lake Union",
                  "Seattle University Community", "Tacoma Downtown")
      
      t0 <- dcast.data.table(tsGrowthCtr, name ~ indicator + run, value.var = sel.yrs.col)
      t <- t0[name %in% lg.rgc,]
      setnames(t, "name", "Name")
      t1 <- create.exp.tsTable(t, runs, tsyear, baseyear)
      
      sketch <- sketch.expanded(colnames(t1)[1], sel.yr.fl[1], sel.yr.fl[2], runnames[1], runnames[2])
      create.DT.expanded(t1, sketch)
    })
    
    output$table_b<- renderDT({
      # Display MICs summary table
      
      tsGrowthCtr <- tsGrowthCtr()
      tsGrowthCtr[, run := get_trim_runnames(run)]

      runs <- get_runnames(runs)
      runnames <- get_trim_runnames(runs)
      sel.yrs.col <- c(unique(baseyear$baseyear), paste0("yr", tsyear))
      sel.yr.fl <- str_extract(sel.yrs.col, "\\d+")

      rgclu <- copy(setDT(rgc.lookup))

      # identify names of mics from growth_center lu and store in vector
      mics <- rgclu[growth_center_id >= 600][, name := as.character(name)][['name']]
      t0 <- dcast.data.table(tsGrowthCtr, name ~ indicator + run, value.var = sel.yrs.col)
      t <- t0[name %in% mics,]
      setnames(t, "name", "Name")

      t1 <- create.exp.tsTable(t, runs, tsyear, baseyear)

      sketch <- sketch.expanded(colnames(t1)[1], sel.yr.fl[1], sel.yr.fl[2], runnames[1], runnames[2])
      create.DT.expanded(t1, sketch)
    })
    
  })
}