# Generate linked scatterplot and leaflet map with underlying table within a tabsetPanel (as seen in Run Comparison
# and Growth tabs).

plot_map_tbl_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
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
                         DTOutput(ns('dtTable'))
                )
    )
  )
  
}

plot_map_tbl_server <- function(id, data, dttable, baseyears, geog, runs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    runnames <- reactive(get_runnames(runs))
    
    geo <- reactive({
      switch(geog,
             taz = "TAZ",
             faz = "FAZ",
             city = "City")
    })
    
    shape <- reactive({
      # shapefile for visualization
      
      joinShp2Tbl(geog, data)
    })
    
    output$dtTable <- renderDT({
      datatable(dttable)
    })
    
    output$plot <- renderPlotly({
      scatterplot(data, "compare", data$estrun1, data$estrun2, runnames()[1], runnames()[2])
    })
    
    
    output$map <- renderLeaflet({
      
      s <- shape()

      colorBinResult <- map.colorBins(s$diff)
      pal <- colorBin(palette = colorBinResult$color, 
                      bins = colorBinResult$bin, 
                      domain=s$diff, 
                      pretty = FALSE)

      # popup setup
      geo.popup1 <- map.popup(shape(), baseyears, 'estrun1','estrun2', geo(), runnames()[1], runnames()[2])
      # geo.popup3 <- paste0("<strong>Center: </strong>", centers$name_id)
      # 
      # Draw the map without selected geographies
      map <- map.layers(shape(), geo(), paste0("Run difference by ", geo()), geo.popup1, "", pal)
    })
  })
  
}

