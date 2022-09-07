# Generate linked scatterplot and leaflet map with underlying table within a tabsetPanel (as seen in Run Comparison
# and Growth tabs).

runcomp_plot_map_tbl_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    tabsetPanel(id = ns('tabset'),
                type = 'pills',
                tabPanel('Visual',
                         fluidRow(
                           column(width = 6,
                             plotlyOutput(ns("plot"), 
                                          height = "725px")
                           ),
                           column(width = 6,
                             leafletOutput(ns("map"), 
                                           height = "725px")
                           )
                         )
                         ),
                tabPanel('Table',
                         br(),
                         DTOutput(ns('dtTable'))
                )
    )
  )
  
}

runcomp_plot_map_tbl_server <- function(id, runs, geog, struc, ind, inputyear, go, alldata, strdata, paths, baseyears) {
  moduleServer(id, function(input, output, session) {

    table <- reactive({
      # returns underlying data table for all visuals

      strdt <- strdata
      alldt <- alldata
      byears <- baseyears
      # byears <- baseyears()
    
      runnames <- get_runnames(runs)

      if (is.null(struc) | struc == "All" | (ind %in% c("Total Population", "Employment")) |
          (ind %in% c("Households", "Residential Units") & geog %in% c("zone", "city")) ){

        # run 1
        b1 <- byears[run == runnames[1],][['baseyear']]
        dt1 <- alldt[run == runnames[1] & geography == geog & indicator == ind,
                     .(name_id, geography, indicator, get(b1), get(paste0('yr',inputyear)))]
        setnames(dt1, dt1[,c((ncol(dt1)-1), ncol(dt1))], c('base_estrun1', 'estrun1'))

        # run 2
        b2 <- byears[run == runnames[2],][['baseyear']]
        dt2 <- alldt[run == runnames[2] & geography == geog & indicator == ind,
                     .(name_id, get(b2),get(paste0('yr', inputyear)))]
        setnames(dt2, dt2[,c((ncol(dt2)-1), ncol(dt2))], c('base_estrun2', 'estrun2'))

        dt <- merge(dt1, dt2, by = 'name_id')
      } else {

        # run 1
        b1 <- str_extract(byears[run == runnames[1],][['baseyear']], "\\d+")
        dt1 <- strdt[run == runnames[1] & geography == geog & (year == b1 | year == inputyear) & indicator == ind & strtype == struc]
        dt1.cast <- dcast.data.table(dt1, name_id + indicator + geography ~ year, value.var = "estimate")
        setnames(dt1.cast, colnames(dt1.cast)[4:5], c('base_estrun1', 'estrun1'))

        # run 2
        b2 <- str_extract(byears[run == runnames[2],][['baseyear']], "\\d+")
        dt2 <- strdt[run == runnames[2] & geography == geog & (year == b2 | year == inputyear)  & indicator == ind & strtype == struc]
        dt2.cast <- dcast.data.table(dt2, name_id ~ year, value.var = "estimate")
        setnames(dt2.cast, colnames(dt2.cast)[2:3], c('base_estrun2', 'estrun2'))
        dt <- merge(dt1.cast, dt2.cast, by = 'name_id')
      }
      dt[,"diff" := (estrun1-estrun2)]

      dt <- switch(geog,
             zone = merge(dt, zone.lookup, by.x = "name_id", by.y = "zone_id") %>% merge(faz.lookup, by = c("faz_id", "County")),
             faz = merge(dt, faz.lookup, by.x = "name_id", by.y = "faz_id")#,
             # merge(dt, city.lookup, by.x = "name_id", by.y = "city_id") %>% setnames("city_name", "Name")
      )
      return(dt)
    })

    dttable <- reactive({
      # clean up column names of table() to render to DT

      runnames <- get_runnames(runs)
      runnames.trim <- get_trim_runnames(runnames)
      baseyears <- baseyears
      # baseyears <- baseyears()

      b <- baseyears[run %in% runnames, ]

      dt <- table()[, .(County, indicator, geography, name_id, Name, base_estrun1, estrun1, base_estrun2, estrun2, diff)]
      setnames(dt, c("County", "Indicator", "Geography", "ID", "Name",
                     paste("Base", runnames.trim[1], str_extract(b[1, ][['baseyear']], "\\d+")),
                     paste(runnames.trim[1], inputyear),
                     paste("Base", runnames.trim[2], str_extract(b[2, ][['baseyear']], "\\d+")),
                     paste(runnames.trim[2], inputyear),
                     "Difference"))
      return(dt)
    })

    runnames <- reactive(get_runnames(runs))
    
    geo <- reactive({
      switch(geog,
             taz = "TAZ",
             faz = "FAZ",
             city = "City")
    })
    
    shape <- reactive({
      # join shapefile to table for visualization

      joinShp2Tbl(geog, table())
    })
    
    output$dtTable <- renderDT({
      display.datatable <- function(table) {
        datatable(table,
                  extensions = 'Buttons', 
                  options = list(
                    dom = 'l<"sep">Bfrtip',
                    buttons = c('csv', 'excel', 'copy'),
                    pageLength = 10,
                    lengthMenu = c(10, 15, 20, nrow(table))
                  )
        )
      }
      
      eventdata <- event_data(event = "plotly_selected", source = 'runcomp')
      
      if(is.null(eventdata)) { 
        # if none are selected, return all records
        
        d <- display.datatable(dttable())
      } else { 
        # else display subset
        
        key <- eventdata[['key']]
        subdata <- dttable()[dttable()$ID %in% key, ]
        d <- display.datatable(subdata)
      }
      
      return(d)
    })
    
    output$plot <- renderPlotly({
      t <- table()
      scatterplot(t, "runcomp", t$estrun1, t$estrun2, runnames()[1], runnames()[2])
    })
    
    
    output$map <- renderLeaflet({
      s <- shape()

      colorBinResult <- map.colorBins(s$diff)
      pal <- colorBin(palette = colorBinResult$color, 
                      bins = colorBinResult$bin, 
                      domain=s$diff, 
                      pretty = FALSE)

      # popup setup
      geo.popup1 <- map.popup(s, baseyears, 'estrun1','estrun2', geo(), runnames()[1], runnames()[2])
      # geo.popup1 <- map.popup(s, baseyears(), 'estrun1','estrun2', geo(), runnames()[1], runnames()[2])
      # geo.popup3 <- paste0("<strong>Center: </strong>", centers$name_id)
      # 
      # Draw the map without selected geographies
      map <- map.layers(s, geo(), paste0("Run difference by ", geo()), geo.popup1, "", pal)
    
      # Re-draw the map with selected geographies
      # Drag event for the scatterplot: will grab ids of selected points
      subdata <- select.items("runcomp", s)
      if(length(subdata) > 0) {
        map <- map %>% addSelectedGeo(subdata, geo()) %>% map.settings(geo())}
      
      return(map)
    })
  })
  
}

