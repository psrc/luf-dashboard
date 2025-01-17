# This module returns a DT formatted table of Key Locations for the Top Sheet

dt_key_loc_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('uiTable'))
  )
}

dt_key_loc_server <- function(id, paths, runs, tsyear, baseyear, alldata) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$uiTable <- renderUI({
      if(is.null(keyloc())) return(NULL)
      
      tagList(
        h2('Key Locations'),
        DTOutput(ns('table'))
      )
    })
    
    
    keyloc <- reactive({
      # Filter table and calculate totals for Key Locations
      
      alldt <- alldata
      
      # gather basic run info  
      runs <- get_runnames(runs)
      runnames <- get_trim_runnames(runs)
      
      b1 <- baseyear[run == runs[1], .(baseyear)][[1]] |> str_extract("\\d+")
      b2 <- baseyear[run == runs[2], .(baseyear)][[1]] |> str_extract("\\d+")
      cols <- paste0("yr", c(b1, b2, tsyear))
      
      key.loc <- c("UW", "Amazon", "SeaTac Airport", "Microsoft Overlake", "Paine Field", "JBLM", "Bangor")
      t <- merge(alldt[geography == 'zone' & (run %in% runs) & (indicator == 'Total Population' | indicator == 'Employment')],
                 splaces.lookup, by.x = "name_id", by.y = "zone_id")
      tm <- melt.data.table(t,
                            id.vars = c("name", "indicator", "run"),
                            measure.vars = str_subset(colnames(t), "^yr.*"),
                            variable.name = "year")
      tm[, year := str_extract(year, "\\d+")]
      t1 <- tm[((run %in% runs[1]) & (year %in% c(b1, tsyear))) | ((run %in% runs[2]) & (year %in% c(b2, tsyear)))]
      t1 <- t1[, lapply(.SD, sum), by = list(Name = name, year, indicator, run), .SDcols = "value"][Name %in% key.loc, ]
      t1[indicator == "Total Population", indicator := "Population"]
    })
    
    
    output$table <- renderDT({
      # Display Key Location summary table
      
      keyloc <- keyloc()
      keyloc[, run := get_trim_runnames(run)]
      
      # gather basic run info  
      runs <- get_runnames(runs)
      runnames <- get_trim_runnames(runs)
      
      b1 <- baseyear[run == runs[1], .(baseyear)][[1]] |> str_extract("\\d+")
      b2 <- baseyear[run == runs[2], .(baseyear)][[1]] |> str_extract("\\d+")
      cols <- paste0("yr", c(b1, b2, tsyear))
      
      t <- dcast.data.table(keyloc, Name ~ year + indicator + run, value.var = "value")
      t1 <- create.exp.tsTable(t, runs, tsyear, baseyear)
      sketch <- sketch.expanded(colnames(t1)[1], b1, b2, tsyear, runnames[1], runnames[2])
      create.DT.expanded(t1, sketch)
    })
    
  })
}