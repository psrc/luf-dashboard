dt_ui <- function(id, title) {
  ns <- NS(id)
  
  tagList(
    h2(title),
    DTOutput(ns('atable'))
  )
  
}

dt_server <- function(id, dttable) { #baseyear, runs, topsheetyear
  moduleServer(id, function(input, output, session) {
    
    output$atable <- renderDT({
      datatable(dttable)
      })
    
    # runs <- runs()
    # tsYear <- tsYear()
    # sel.yr.fl <- c(years[1], tsYear) #change to baseyear
    # sel.yrs.col <- paste0("yr", sel.yr.fl)
    
    # create.tsTable(table, idname)
    
    # t1 <- create.tsTable(t, "County") %>% select(1:3, 10:12, 4, 13:15, 5:6)
    # sketch <- sketch.basic(colnames(t1)[1],  sel.yr.fl[1],  sel.yr.fl[2], runs[1], runs[2])
    # create.DT.basic(t1, sketch)
  })
}