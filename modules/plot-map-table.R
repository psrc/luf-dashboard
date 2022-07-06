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

plot_map_tbl_server <- function(id, data, dttable, runs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    runnames <- get_runnames(runs)
    
    # take data
    # merge with resp geog shapefile 
    
    output$dtTable <- renderDT({
      datatable(dttable)
    })
    
    output$plot <- renderPlotly({
      scatterplot(data, "compare", data$estrun1, data$estrun2, runnames[1], runnames[2])
    })
    
    # render to leaflet
  })
  
}

