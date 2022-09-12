# This module returns a DT formatted table with table header for the Special Places tab

sp_places_table_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('header')),
    DTOutput(ns('table'))
  )
  
}

sp_places_table_server <- function(id, title, aindicator, runs, paths, alldata, baseyears, inputyear) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$header <- renderUI({
      h2(title)
    })
    
    spdt <- reactive({
      alldt <- alldata
      runnames <- get_runnames(runs)
      path <- paths[names(paths) %in% runnames]

      # baseyear for run
      byr <- str_extract(baseyears[run == runnames, baseyear], "\\d+")
      yrs <- c(byr, inputyear)
      sel.yrs.col <- paste0('yr', yrs)
      
      spcols <- c("name_id", "indicator", sel.yrs.col)
      zone.values <- merge(alldt[geography == 'zone' & run == runnames, .SD, .SDcols = spcols], splaces.lookup[,c('zone_id', 'name')], by.x = 'name_id', by.y ='zone_id')
      place.data <- zone.values[, lapply(.SD, sum), .SDcols = spcols[3:4], by = list(indicator, name)
                                ][, `:=`(change = get(eval(sel.yrs.col[2])) - get(eval(sel.yrs.col[1])))
                                  ][, `:=`(percent = round((change/get(eval(sel.yrs.col[1])))*100, 1) )
                                    ][, name := as.character(name)]
      # place.data$name <- as.character(place.data$name)
      colnames(place.data)[2:ncol(place.data)] <- str_to_title(c('special place', yrs[1], yrs[2], paste("change", yrs[1], "-", yrs[2]), "percent change"))
      return(place.data)
    })
 
    output$table <- renderDT({
      dt <- spdt()[indicator == aindicator, 2:ncol(spdt())]
      
      datatable(dt,
                rownames = FALSE,
                options = list(pageLength = nrow(spdt()), dom = 'tip')
      )
    })
 
  })
}