get_runnames <- function(runs) {
  # splices and extracts the run directory name from a path
  
  map(runs, ~str_split(.x, '/')) %>% 
    flatten() %>% 
    map(., ~pluck(.x, length(.x))) %>% 
    unlist()
}

get_trim_runnames <- function(runnames) {
  # splices runname at the period for pretty labeling purposes
  
  map(runnames, ~str_split(.x, '\\.')[[1]][1]) %>% flatten() %>% unlist()
}


# leaflet -----------------------------------------------------------------


joinShp2Tbl <- function(geog, table){
  # Joins reactive tables to respective shapefiles.

  switch(geog,
         zone = merge(zone.shape, table, by = "name_id"),
         faz = merge(faz.shape, table, by = "name_id")#,
         
         # zone = merge(zone.shape, table, by.x = "taz", by.y = "name_id"),
         # faz = merge(faz.shape, table, by.x = "faz10", by.y = "name_id")#,
         # merge(city.shape, table, by.x = "city_id", by.y = "name_id"),
         # {centers.shape <- centers[centers$name_id != 0,];
         # merge(centers.shape, table, by.x = "name_id", by.y = "name_id")}
  )
}

map.colorBins <- function(diffcolumn){
  # Sets Leaflet color scheme and numeric bins.
  
  rng <- range(diffcolumn)
  if (rng[1] < 0 & rng[2] > 0){
    diff.range <- "both"
    bins.from.positive <- abs(rng[2]) > abs(rng[1])
  } else if (rng[1] >=0 & rng[2] > 0){
    diff.range <- "pos"
  } else if (rng[1] < 0 & rng[2] < 0){
    diff.range <- "neg"
  } else {
    diff.range <- "none"
  }
  max.bin <- max(abs(rng))
  round.to <- 10^floor(log10(max.bin))
  # round maximum to the nearest 100 or 1000 or whatever is appropriate (determined by the log10)
  max.bin <- ceiling(max.bin/round.to)*round.to
  absbreaks <- (sqrt(max.bin)*c(0.1, 0.2,0.4, 0.6, 0.8, 1))^2 # breaks on sqrt scale
  
  if (diff.range == "both"){
    color <- c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#ffffff", "#f7f7f7",
               "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f")
    bin <- c(-rev(absbreaks), absbreaks)
  } else if (diff.range == "pos"){
    color <- "Reds"
    bin <- c(0, absbreaks)
  } else if (diff.range == "neg"){
    color <- "Blues"
    bin <- c(-rev(absbreaks), 0)
  } else if (diff.range == "none"){
    color <- "transparent"
    bin <- c(0, 1)
  }
  return(list(color=color, bin=bin))
}


map.popup <- function(shapefile, baseyear.df, xcolumn, ycolumn, layerctrl, xtitle, ytitle){
  # Writes Leaflet popup text for non-centers shapefiles. Requires reactive shapefile, string x&y axis titles.

  base.x <- paste(str_extract(baseyear.df[1, ][['baseyear']], '\\d+'), baseyear.df[1, ][['run']])
  base.y <- paste(str_extract(baseyear.df[2, ][['baseyear']], '\\d+'), baseyear.df[2, ][['run']])
  paste0("<strong>ID: </strong>", shapefile$name_id,
         "<br><strong>", layerctrl, " Name: </strong>", shapefile$Name,
         "<br><strong>", paste('Base', base.x)," estimate: </strong>", prettyNum(round(shapefile[['base_estrun1']], 0), big.mark = ","),
         "<br><strong>", paste('Base', base.y)," estimate: </strong>", prettyNum(round(shapefile[['base_estrun2']], 0), big.mark = ","),
         "<br><strong>", xtitle," estimate: </strong>", prettyNum(round(shapefile[[xcolumn]], 0), big.mark = ","),
         "<br><strong>", ytitle," estimate: </strong>", prettyNum(round(shapefile[[ycolumn]], 0), big.mark = ","),
         "<br><strong>Difference: </strong>", prettyNum(round(shapefile$diff, 0), big.mark = ","))
}


map.layers <- function(shapefile, layerctrl, legendtitle, popupgeo, popupctr, mappalette){
  # Creates Leaflet baselayers. Requires reactive shapefile, string legend title.

  map <- leaflet(data=shapefile)%>%
    addProviderTiles("CartoDB.Positron", group = "Street Map")%>%
    addProviderTiles("Esri.WorldImagery", group = "Imagery")%>%
    addPolygons(fillColor = ~mappalette(shapefile$diff),
                fillOpacity = 0.7,
                stroke = TRUE,
                color = "#8a8a95",
                weight = 1,
                group = layerctrl,
                popup = popupgeo)%>%
    addLegend("bottomright",
              pal = mappalette,
              values = mappalette(shapefile$diff),
              title = legendtitle,
              opacity =1,
              labFormat = labelFormat(digits = 0, big.mark = ","))%>%
    setView(lng = -122.008546, lat = 47.549390, zoom = 9)%>%
    addEasyButton(
      easyButton(
        icon="fa-globe",
        title="Zoom to Region",
        onClick=JS("function(btn, map){
                     map.setView([47.549390, -122.008546],9);}"))
    )%>%
    addLayersControl(baseGroups = c("Street Map", "Imagery"),
                     # overlayGroups = c("Centers",layerctrl),
                     options = layersControlOptions(collapsed = FALSE))
  
  # addPolygons(data=centers,
  #             stroke = TRUE,
  #             color = "#a9a9b1",
  #             dashArray = "5",
  #             weight = 2,
  #             group = "Centers",
  #             popup = popupctr)%>%
  
  return(map)
}


# plots -------------------------------------------------------------------


scatterplot <- function(table, sourcename, xcolumn, ycolumn, xtitle, ytitle) {
  # Creates a Plotly scatterplot. Requires reactive table, string source name and string x&y axis titles.
  
  data <- table
  key <- data$name_id # uniquely identify geo for Plotly
  p <- plot_ly(data,
               type = 'scatter',
               x = ~xcolumn,
               y = ~ycolumn,
               name = "",
               source = sourcename,
               text = ~paste0("ID: ", name_id,
                              "<br>Name: ", Name),
               key = key, # will appear in 'eventdata'
               mode = 'markers',
               showlegend = F)%>%
    add_trace(x=c(0,~max(xcolumn)),
              y=c(0,~max(xcolumn)),
              color= I("grey"),
              opacity = .6,
              mode = "lines",
              showlegend = F)%>%
    layout(font = list(family="Segoe UI", size = 13.5),
           title = " ",
           xaxis = list(title = xtitle),
           yaxis = list(title = ytitle),
           margin = list(l=100, b=100, t=90, r=100)
    )
  p
}


# DT ----------------------------------------------------------------------


# # function creating tables on Run Comparison and Growth tabs
# create.DT.generic <- function(table) {
#   datatable(table,
#             extensions = 'Buttons',
#             class = 'cell-border stripe',
#             options = list(dom = 'Bfrtip',
#                            buttons = list('copy',
#                                           list(extend = 'excel',
#                                                buttons = 'excel',
#                                                filename = 'LUVQCDashboard')),
#                            initComplete = JS(
#                              "function(settings, json) {",
#                              "$(this.api().table().header()).css({'font-size': '15px'});",
#                              "}"),
#                            paging = TRUE,
#                            pageLength = 150,
#                            searching = TRUE,
#                            scrollY = '450px'
#             )
#   ) %>%
#     formatStyle(colnames(table)[1:ncol(table)],
#                 `font-size` = '13px')
# }