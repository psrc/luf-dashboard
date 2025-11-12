# Generate linked scatterplot and leaflet map with underlying table within a tabsetPanel for Growth tab

equity_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    br(),
    fluidRow(
      column(width = 4,
             leafletOutput(ns("map_base"), height = "725px")
              ),
      column(width = 4,
             leafletOutput(ns("map_end"), height = "725px")
              ),
      column(width = 4,
              leafletOutput(ns("map_dif"), height = "725px")
              )
          )
    )
}

equity_server <- function(id, run, geog, ind, valtype, go, eqtdata, paths, baseyears) {

  moduleServer(id, function(input, output, session) {
    
    inputyears <- reactiveValues(start = NA, end = NA)
    
    table <- eventReactive(go, {
      # returns underlying data table for all visuals

      eqdt <- eqtdata
      byears <- baseyears
    
      runnames <- get_runnames(run)
      
      inputyears$start <- as.integer(str_extract(byears[run == runnames[1], baseyear], "\\d+"))
      inputyears$end <- years[length(years)]
      
      dt1 <- eqdt[run == runnames & geography == geog & indicator == ind & 
                    (year == inputyears$start | year == inputyears$end),]

      dt <- dcast.data.table(dt1, name_id + geography + run + indicator ~ year, value.var = "estimate")
      setnames(dt, colnames(dt)[(ncol(dt)-1):ncol(dt)], c('yr1', 'yr2'))
      dt[,"diff" := (yr2-yr1)]

      # merge with lookup tables for names
      dt <- switch(geog,
             zone = merge(dt, zone.lookup, by.x = "name_id", by.y = "zone_id") %>% merge(faz.lookup, by = c("faz_id", "County")),
             faz = merge(dt, faz.lookup, by.x = "name_id", by.y = "faz_id"),
             city = merge(dt, city.lookup, by.x = "name_id", by.y = "city_id") %>% setnames(c("city_name", "county"), c("Name", "County")),
             control = merge(dt, ctrl.lookup, by.x = "name_id", by.y = "control_id") %>% setnames(c("control_name", "county"), c("Name", "County"))
             )
      
      return(dt)
    })

    dttable <- reactive({
      print("dttable")
      # clean up column names of table() to render to DT
      runnames <- get_runnames(run)
      runnames.trim <- get_trim_runnames(runnames)

      b <- baseyears[run %in% runnames, ]

      dt <- table()[, .(run, County, indicator, geography, name_id, Name, yr1, yr2, diff)]
      setnames(dt, c("Run", "County", "Indicator", "Geography", "ID", "Name",
                         inputyears$start,
                         inputyears$end,
                         "Growth"))
      return(dt)
    })

    runnames <- reactive(get_runnames(run))
    
    geo <- reactive({
      switch(geog,
             zone = "TAZ",
             faz = "FAZ",
             city = "City",
             control = "Control")
    })
    
    shape <- reactive({
      # join shapefile to table for visualization
      
      joinShp2Tbl(geog, table())
    })
    
    output$map_dif <- renderLeaflet({
      if(is.null(table()) || is.null(shape())) return(NULL)
      if (is.null(shape()$diff) | all(shape()$yr1 == 0) | all(shape()$yr2 == 0)) return(NULL)

      s <- shape()

      colorBinResult <- map.colorBins(s$diff)
      pal <- colorBin(palette = colorBinResult$color, 
                      bins = colorBinResult$bin, 
                      domain=s$diff, 
                      pretty = FALSE)

      # popup setup
      geo.popup1 <- map.popup(s, baseyears, 'yr1','yr2', geo(), 
                              inputyears$start, inputyears$end, tab = 'growth')

      # Draw the map without selected geographies
      map <- map.layers(s, geo(), paste0("Difference by ", geo()), geo.popup1, "", pal)
    
      return(map)
    })
    
    render_equity_map <- function(dt, shp, domain_col, geo, yr){
      if(is.null(dt) || is.null(shp)) return(NULL)
      if (all(shp[[domain_col]] == 0)) return(NULL)
      
      shp$diff <- shp[[domain_col]]
      
      colorBinResult <- map.colorBins(c(shp$yr1, shp$yr2))
      pal <- colorBin(palette = colorBinResult$color, 
                      bins = colorBinResult$bin, 
                      domain=shp$diff, 
                      pretty = FALSE)
      
      # popup setup
      geo.popup <- map.popup(shp, baseyears, domain_col, NA, geo, yr, NA, tab = 'equity_total')
      
      # Draw the map without selected geographies
      map <- map.layers(shp, geo, paste0("Totals by ", geo, " in ", yr), 
                        geo.popup, "", pal)
      return(map)
    }
    
    output$map_base <- renderLeaflet({
      render_equity_map(table(), shape(), "yr1", geo(), inputyears$start)
    })
    
    output$map_end <- renderLeaflet({
      render_equity_map(table(), shape(), "yr2", geo(), inputyears$end)
    })
  })
  
}

