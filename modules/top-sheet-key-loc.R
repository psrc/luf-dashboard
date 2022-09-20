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
      sel.yrs.col <- c(unique(baseyear$baseyear), paste0("yr", tsyear))
      sel.yr.fl <- str_extract(sel.yrs.col, "\\d+")

      key.loc <- c("UW", "Amazon", "SeaTac Airport", "Microsoft Overlake", "Paine Field", "JBLM", "Bangor")
      t <- merge(alldt[geography == 'zone' & (run %in% runs) & (indicator == 'Total Population' | indicator == 'Employment')], 
                 splaces.lookup, by.x = "name_id", by.y = "zone_id")
      t1 <- t[, lapply(.SD, sum), by = list(Name = name, indicator, run), .SDcols = sel.yrs.col][Name %in% key.loc, ]
      t1[indicator == "Total Population", indicator := "Population"]
    })
    
    
    output$table <- renderDT({
      # Display Key Location summary table
      
      keyloc <- keyloc()
      keyloc[, run := get_trim_runnames(run)]
      
      # gather basic run info  
      runs <- get_runnames(runs)
      runnames <- get_trim_runnames(runs)
      sel.yrs.col <- c(unique(baseyear$baseyear), paste0("yr", tsyear))
      sel.yr.fl <- str_extract(sel.yrs.col, "\\d+")
      
      t <- dcast.data.table(keyloc, Name ~ indicator + run, value.var = sel.yrs.col)
      t1 <- create.exp.tsTable(t, runs, tsyear, baseyear)
      sketch <- sketch.expanded(colnames(t1)[1], sel.yr.fl[1], sel.yr.fl[2], runnames[1], runnames[2])
      create.DT.expanded(t1, sketch)
    })
    
  })
}