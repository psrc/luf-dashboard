# This module returns a DT formatted table with table header for either Households, Population, and Jobs by Sector for the Top Sheet

dt_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('header')),
    DTOutput(ns('table'))
  )
  
}

dt_server <- function(id, dttable, aindicator, idname, runs, tsyear, baseyear, title) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$header <- renderUI({
        h2(title)
    })
    
    # gather basic run info  
    runs <- get_runnames(runs)
    runnames <- get_trim_runnames(runs)
    sel.yrs.col <- c(unique(baseyear$baseyear), paste0("yr", tsyear))
    sel.yr.fl <- str_extract(sel.yrs.col, "\\d+")
    
    # filter and create table; create a DT container
    d <- dttable[indicator == aindicator, ]
    t1 <- create.tsTable(d, idname, runs, tsyear, baseyear) %>% select(1:3, 10:12, 4, 13:15, 5:6)
    sketch <- sketch.basic(colnames(t1)[1],  sel.yr.fl[1], sel.yr.fl[2], runnames[1], runnames[2])
    
    output$table <- renderDT({
      create.DT.basic(t1, sketch)
      })
    
  })
}