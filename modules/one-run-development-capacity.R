# Generate maps for development capacity

dev_cap_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow(style = "margin: 1rem;",
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

dev_cap_server <- function(id, run, geog, inputyear, go, paths, devdata, capdata, centers) {
  moduleServer(id, function(input, output, session) {
    
    create_dcap_tbl <- function(captype_val, devtype_val) {
      if (is.null(capdata) || is.null(devdata)) return(NULL)
      if (is.null(run) || is.null(geog) || is.null(year)) return(NULL)
      
      t1 <- capdata[run == run & geography == geog & captype == captype_val,][,.(name_id, capacity, captype)]
      t2 <- devdata[run == run & geography == geog & year == inputyear & devtype == devtype_val,]
      
      if (nrow(t1) == 0 | nrow(t2) == 0) {
        return(NULL)
      } else if (geog == 'zone' & (nrow(t1) < 3700) | geog == 'city' & (nrow(t1) < 140)){
        return(NULL)
      } else {
        
        
        t <- merge(t1, t2, by = c("name_id"))
        t0 <- t[, diff := capacity-estimate]
        
        return(switch(geog,
                      "zone" = merge(t0, zone.lookup, by.x = "name_id", by.y = "zone_id") %>% merge(faz.lookup, by = "faz_id"),
                      "faz" = merge(t0, faz.lookup, by.x = "name_id", by.y = "faz_id"),
                      "city" = merge(t0, city.lookup, by.x = "name_id", by.y = "city_id") %>% setnames("city_name", "Name"),
                      "growth_center" = t0
        ))
      }
    }
    
    total <- eventReactive(go, {
       create_dcap_tbl(captype_val = "Total", devtype_val = "Building Sqft")
    })

    res <- eventReactive(go, {
      create_dcap_tbl(captype_val = "Residential", devtype_val = "Residential Units")
    })

    nonres <- eventReactive(go, {
      create_dcap_tbl(captype_val = "Non-Residential", devtype_val = "Non-Residential Sqft")
    })

    # Total shapefile ready for visualization
    shape_total <- reactive({
      joinShp2Tbl(geog, total())
    })

    # Residential shapefile ready for visualization
    shape_res <- reactive({
      joinShp2Tbl(geog, res())
    })

    # Non-Residential shapefile ready for visualization
    shape_nonres <- reactive({
      joinShp2Tbl(geog, nonres())
    })

    # leaflet layer control
    geo <- reactive({
      switch(geog,
             "taz" = "TAZ",
             "faz" = "FAZ",
             "city" = "City",
             "growth_center" = "Growth Center"
      )
    })

    # renderings ----
    
    render_capacity_map <- function(shape, x, y, popup_label) {
      
      s <- shape
      if (is.null(s$diff)) return(NULL)
      
      # Set up symbology and categorization
      colorBinResult <- map.colorBins(s$diff)
      pal <- colorBin(palette = colorBinResult$color, 
                      bins = colorBinResult$bin, 
                      domain = s$diff, 
                      pretty = FALSE)
      
      # popup setup
      geo.popup1 <- map.basic.popup(s,
                                    'capacity',
                                    'estimate', 
                                    geo(), 
                                    x, 
                                    y)
      
      geo.popup3 <- paste0("<strong>Center: </strong>", centers[["name_id"]])
      
      if (geog == "growth_center"){
        map <- map.layers(s, geo(), popup_label, geo.popup1, pal)
      } else {
        map <- map.layers(s, geo(), popup_label, geo.popup1, geo.popup3, pal)
      }
      
      map
      
    }
    
    ## Total Dev Capacity map ----
    output$total_map <- renderLeaflet({
      render_capacity_map(shape = shape_total(), 
                          x = 'Total Max Development Capacity', 
                          y = 'Building Sqft', 
                          popup_label = "Total Development Capacity")
    })

    ## Residential Dev Capacity map ----
    output$res_map <- renderLeaflet({
      
      render_capacity_map(shape = shape_res(), 
                          x = 'Residential Max Development Capacity', 
                          y = 'Residential Units', 
                          popup_label = "Residential Development Capacity")
    })

    ## Non-Residential Dev Capacity map ----
    output$nonres_map <- renderLeaflet({
      
      render_capacity_map(shape = shape_nonres(), 
                          x = 'Non-Residential Max Development Capacity', 
                          y = 'Non-Residential Sqft', 
                          popup_label = "Non-Residential Development Capacity")
    })
    

  })
  
}

