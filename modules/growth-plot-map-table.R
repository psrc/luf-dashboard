# Generate linked scatterplot and leaflet map with underlying table within a tabsetPanel for Growth tab

growth_plot_map_tbl_ui <- function(id) {
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

growth_plot_map_tbl_server <- function(id, run, geog, struc, ind, inputyears, go, alldata, strdata, paths, baseyears) {
  moduleServer(id, function(input, output, session) {
    
    table <- reactive({
      # returns underlying data table for all visuals

      strdt <- strdata
      alldt <- alldata
      byears <- baseyears
      gYear <- inputyears
    
      runnames <- get_runnames(run)
      
      if (is.null(struc) | struc == "All" | (ind %in% c("Total Population", "Employment")) |
          (ind %in% c("Households", "Residential Units") & geog %in% c("zone", "city")) ){

        dt <- alldt[run == runnames & geography == geog & indicator == ind,
                    .(name_id, geography, run, indicator, yr1 = get(paste0('yr',gYear[1])), yr2 = get(paste0('yr', gYear[2])))]
      } else {
        dt1 <- strdt[run == runnames & geography == geog & indicator == ind & strtype == struc & (year == gYear[1] | year == gYear[2]),
                     .(name_id, geography, run, indicator, strtype, year, estimate)]

        dt <- dcast.data.table(dt1, name_id + geography + run + indicator ~ year, value.var = "estimate")
        setnames(dt, colnames(dt)[(ncol(dt)-1):ncol(dt)], c('yr1', 'yr2'))

      }
      dt[,"diff" := (yr2-yr1)]

      # merge with lookup tables for names
      dt <- switch(geog,
             zone = merge(dt, zone.lookup, by.x = "name_id", by.y = "zone_id") %>% merge(faz.lookup, by = c("faz_id", "County")),
             faz = merge(dt, faz.lookup, by.x = "name_id", by.y = "faz_id"),
             city = merge(dt, city.lookup, by.x = "name_id", by.y = "city_id") %>% setnames(c("city_name", "county"), c("Name", "County")))
      
      return(dt)
    })

    dttable <- reactive({
      # clean up column names of table() to render to DT
      runnames <- get_runnames(run)
      runnames.trim <- get_trim_runnames(runnames)

      b <- baseyears[run %in% runnames, ]

      dt <- table()[, .(run, County, indicator, geography, name_id, Name, yr1, yr2, diff)]
      setnames(dt, c("Run", "County", "Indicator", "Geography", "ID", "Name",
                         inputyears[1],
                         inputyears[2],
                         "Growth"))
      return(dt)
    })

    runnames <- reactive(get_runnames(run))
    
    geo <- reactive({
      switch(geog,
             zone = "TAZ",
             faz = "FAZ",
             city = "City")
    })
    
    shape <- reactive({
      # join shapefile to table for visualization
      
      joinShp2Tbl(geog, table())
    })
    
    output$dtTable <- renderDT({
      if(is.null(table())| all(table()$yr1 == 0) | all(table()$yr2 == 0)) return(NULL)
      
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
      
      eventdata <- event_data(event = "plotly_selected", source = 'growth')
      
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
      if(is.null(table())| all(table()$yr1 == 0) | all(table()$yr2 == 0)) return(NULL)
      
      t <- table()
      scatterplot(t, "growth", t$yr1, t$yr2, inputyears[1], inputyears[2])
    })
    
    
    output$map <- renderLeaflet({
      if(is.null(table()) || is.null(shape())) return(NULL)
      if (is.null(shape()$diff) | all(shape()$yr1 == 0) | all(shape()$yr2 == 0)) return(NULL)
      
      s <- shape()

      colorBinResult <- map.colorBins(s$diff)
      pal <- colorBin(palette = colorBinResult$color, 
                      bins = colorBinResult$bin, 
                      domain=s$diff, 
                      pretty = FALSE)

      # popup setup
      geo.popup1 <- map.popup(s, baseyears, 'yr1','yr2', geo(), inputyears[1], inputyears[2], tab = 'growth')
      # geo.popup3 <- paste0("<strong>Center: </strong>", centers$name_id)

      # Draw the map without selected geographies
      map <- map.layers(s, geo(), paste0("Year difference by ", geo()), geo.popup1, "", pal)
    
      # Re-draw the map with selected geographies
      # Drag event for the scatterplot: will grab ids of selected points
      subdata <- select.items("growth", s)
      if(length(subdata) > 0) {
        map <- map %>% addSelectedGeo(subdata, geo()) %>% map.settings(geo())}
      
      return(map)
    })
  })
  
}

