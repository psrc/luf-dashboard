# This module returns a DT formatted table with table header for either Households, Population, and Employment for the Top Sheet

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

    # filter and create table; create a DT container
    d <- dttable[indicator == aindicator, ]
    t1 <- create.tsTable(d, idname, runs, tsyear, baseyear) %>% select(1:2, 4, 3, 10:12, 5, 13:15, 6:7)
    
    b1 <- baseyear[run == runs[1], .(baseyear)][[1]] |> str_extract("\\d+")
    b2 <- baseyear[run == runs[2], .(baseyear)][[1]] |> str_extract("\\d+")
    sketch <- sketch.basic(colnames(t1)[1], b1, b2, tsyear, runnames[1], runnames[2])
    
    output$table <- renderDT({
      create.DT.basic(t1, sketch)
      })
    
  })
}