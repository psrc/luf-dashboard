# Generate maps for development capacity

dev_cap_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow(
        column(width = 4,
               leafletOutput(ns("total_map"), height = "800px")
        ), # end column
        column(width = 4,
               leafletOutput(ns("res_map"), height = "800px")
        ), # end column
        column(width = 4,
               leafletOutput(ns("nonres_map"), height = "800px")
        ) # end column
      ) # end fluidRow
      
    ) # end fluidPage
 
  )
}

dev_cap_server <- function(id, run, geog, year, go, paths, devdata, capdata) {
  moduleServer(id, function(input, output, session) {
    
    create_dcap_tbl <- function(captype_val, devtype_val) {
      if (is.null(capdata) || is.null(devdata)) return(NULL)
      if (is.null(run) || is.null(geog) || is.null(year)) return(NULL)
      
      t1 <- capdata[run == run & geography == geog & captype == captype_val,][,.(name_id, capacity, captype)]
      t2 <- devdata[run == run & geography == geog & year == year & devtype == devtype_val,]
      
      if (nrow(t1) == 0 | nrow(t2) == 0) {
        return(NULL)
      } else if (geog == 'zone' & (nrow(t1) < 3700) | geog == 'city' & (nrow(t1) < 140)){
        return(NULL)
      } else {
        browser()
        
        t <- merge(t1, t2, by = c("name_id"))
        t0 <- t[, diff := capacity-estimate]
        
        return(switch(as.integer(geog),
                      merge(t0, zone.lookup, by.x = "name_id", by.y = "zone_id") %>% merge(faz.lookup, by = "faz_id"),
                      merge(t0, faz.lookup, by.x = "name_id", by.y = "faz_id"),
                      merge(t0, city.lookup, by.x = "name_id", by.y = "city_id") %>% setnames("city_name", "Name"),
                      t0
        ))
      }
    }
    
    total <- eventReactive(go, {
       create_dcap_tbl(captype_val = "Total", devtype_val = "Building Sqft")
      # if (is.null(capdata) || is.null(devdata)) return(NULL)
      # if (is.null(run) || is.null(geog) || is.null(year)) return(NULL)
      # 
      # t1 <- capdata[run == run & geography == geog & captype == "Total",][,.(name_id, capacity, captype)]
      # t2 <- devdata[run == run & geography == geog & year == year & devtype == "Building Sqft",]
      # 
      # if (nrow(t1) == 0 | nrow(t2) == 0) {
      #   return(NULL)
      # } else if (geog == 'zone' & (nrow(t1) < 3700) | geog == 'city' & (nrow(t1) < 140)){
      #   return(NULL)
      # } else {
      #   t <- merge(t1, t2, by = c("name_id"))
      #   t0 <- t[, diff := capacity-estimate]
      #   return(switch(as.integer(geography),
      #                 merge(t0, zone.lookup, by.x = "name_id", by.y = "zone_id") %>% merge(faz.lookup, by = "faz_id"),
      #                 merge(t0, faz.lookup, by.x = "name_id", by.y = "faz_id"),
      #                 merge(t0, city.lookup, by.x = "name_id", by.y = "city_id") %>% setnames("city_name", "Name"),
      #                 t0
      #   ))
      # }

    })

    res <- eventReactive(go, {
      create_dcap_tbl(captype_val = "Residential", devtype_val = "Residential Units")
    })

    nonres <- eventReactive(go, {
      create_dcap_tbl(captype_val = "Non-Residential", devtype_val = "Non-Residential Sqft")
    })

    # # Total shapefile ready for visualization
    # dcapShape_total <- reactive({
    #   joinShp2Tbl(geog, total())
    # })
    # 
    # # Residential shapefile ready for visualization
    # dcapShape_res <- reactive({
    #   joinShp2Tbl(geog, res())
    # })
    # 
    # # Non-Residential shapefile ready for visualization
    # dcapShape_nonres <- reactive({
    #   joinShp2Tbl(geog, nonres())
    # })
    # 
    # # leaflet layer control
    # dcapGeo <- reactive({
    #   switch(as.integer(geog),
    #          "TAZ",
    #          "FAZ",
    #          "City",
    #          "Growth Center"
    #   )
    # })

    # renderings ----
    
    # Total Dev Capacity map
    output$total_map <- renderLeaflet({
      # capdata
      # devdata
      total()
      # dcapshapetot <- dcapShape_total()
      # if (is.null(dcapshapetot$diff)) return(NULL)
      # 
      # # Set up symbology and categorization
      # colorBinResult <- map.colorBins(dcapshapetot$diff, geog)
      # pal <- colorBin(palette = colorBinResult$color, bins = colorBinResult$bin, domain=dcapshapetot$diff, pretty = FALSE)
      # 
      # # popup setup
      # geo.popup1 <- map.shp.popup(dcapshapetot,'capacity','estimate',dcapGeo(), 'Total Max Development Capacity', 'Building Sqft')
      # geo.popup3 <- paste0("<strong>Center: </strong>", centers$name_id)
      # 
      # if (as.integer(geog) == 4){
      #   map <- map.layers.basic(dcapshapetot, dcapGeo(), "Total Development Capacity", geo.popup1, pal)
      # } else {
      #   map <- map.layers(dcapshapetot, dcapGeo(), "Total Development Capacity", geo.popup1, geo.popup3, pal)
      # }
      # 
      # map

    })

    # Residential Dev Capacity map
    output$res_map <- renderLeaflet({
      # dcapshaperes <- dcapShape_res()
      # if (is.null(dcapshaperes$diff)) return(NULL)
      # 
      # # Set up symbology and categorization
      # colorBinResult <- map.colorBins(dcapshaperes$diff, geog)
      # pal <- colorBin(palette = colorBinResult$color, bins = colorBinResult$bin, domain=dcapshaperes$diff, pretty = FALSE)
      # 
      # # popup setup
      # geo.popup1 <- map.shp.popup(dcapshaperes,'capacity','estimate', dcapGeo(), 'Residential Max Development Capacity', 'Residential Units')
      # geo.popup3 <- paste0("<strong>Center: </strong>", centers$name_id)
      # 
      # if (as.integer(input$dcap_select_geography) == 4){
      #   map <- map.layers.basic(dcapshaperes, dcapGeo(), "Residential Development Capacity", geo.popup1, pal)
      # } else {
      #   map <- map.layers(dcapshaperes, dcapGeo(), "Residential Development Capacity", geo.popup1, geo.popup3, pal)
      # }
      # 
      # map

    })

    # Non-Residential Dev Capacity map
    output$nonres_map <- renderLeaflet({
      # dcapshapenonres <- dcapShape_nonres()
      # if (is.null(dcapshapenonres$diff)) return(NULL)
      # 
      # # Set up symbology and categorization
      # colorBinResult <- map.colorBins(dcapshapenonres$diff, geog)
      # pal <- colorBin(palette = colorBinResult$color, bins = colorBinResult$bin, domain=dcapshapenonres$diff, pretty = FALSE)
      # 
      # # popup setup
      # geo.popup1 <- map.shp.popup(dcapshapenonres,'capacity','estimate',dcapGeo(), 'Non-Residential Max Development Capacity', 'Non-Residential Sqft')
      # geo.popup3 <- paste0("<strong>Center: </strong>", centers$name_id)
      # 
      # if (as.integer(geog) == 4){
      #   map <- map.layers.basic(dcapshapenonres, dcapGeo(), "Non-Residential Development Capacity", geo.popup1, pal)
      # } else {
      #   map <- map.layers(dcapshapenonres, dcapGeo(), "Non-Residential Development Capacity", geo.popup1, geo.popup3, pal)
      # }
      # 
      # map

    })
    

  })
  
}

