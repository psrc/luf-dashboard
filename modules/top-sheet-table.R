# This module returns a DT formatted table for either Households, Population, and Jobs by Sector for the Top Sheet

dt_ui <- function(id, title) {
  ns <- NS(id)
  
  tagList(
    h2(title), # make conditional
    DTOutput(ns('table'))
  )
  
}

dt_server <- function(id, dttable, aindicator, idname, runs, tsyear, baseyear) {
  moduleServer(id, function(input, output, session) {

    # gather basic run info  
    runs <- get_runnames(runs)
    sel.yrs.col <- c(unique(baseyear$baseyear), paste0("yr", tsyear))
    sel.yr.fl <- str_extract(sel.yrs.col, "\\d+")
    
    # filter and create table; create a DT container
    d <- dttable[indicator == aindicator, ]
    t1 <- create.tsTable(d, idname, runs, tsyear, baseyear) %>% select(1:3, 10:12, 4, 13:15, 5:6)
    sketch <- sketch.basic(colnames(t1)[1],  sel.yr.fl[1], sel.yr.fl[2], runs[1], runs[2])
    
    output$table <- renderDT({
      create.DT.basic(t1, sketch)
      })
    
  })
}