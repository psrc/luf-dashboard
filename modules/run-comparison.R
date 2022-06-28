multi_scat_map_data_ui <- function(id) {
  ns <- NS(id)
 
  tagList(
    uiOutput(ns('uiRunComp'))
  )
  
}

multi_scat_map_data_server <- function(id, alldata, paths) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$uiRunComp <- renderUI({ #if Enter clicked and on Run Comparison tab
      
      geogs <- c("TAZ"='zone', "FAZ"='faz', "City"='city')
      inds <- c("Total Population", "Households", "Employment", "Residential Units")
      years <- 2014:2050
      
      tagList(
        selectInput(session$ns('runs'),
                    label = 'Compare two runs',
                    choices = paths,
                    multiple = TRUE),
        selectInput(ns('geography'),
                    label = 'Geography',
                    choices = geogs,
                    selected = 'faz'),
        selectInput(ns('indicator'),
                    label = 'Indicator',
                    choices = inds),
        
        conditionalPanel(condition = "(input.indicator == 'Residential Units' | input.indicator == 'Households') &&
                                                input.geography == 'faz'",
          
          # condition = "(input.indicator == 'Residential Units' | input.indicator == 'Households') && output.strdtavail &&
          #                                       input.geography == 'faz'",
                         ns = ns,
                         radioButtons(ns("structure"),
                                      label = h5("Categories"),
                                      choices = list("All" = "All", "Single Family" = "singlefamily", "Multi-Family" = "multifamily"),
                                      selected = "All")
        ),
        
        selectInput(ns('year'),
                    label = 'Year',
                    choices = years,
                    selected = max(years)),
        helpText("Use the 'Box Select' or 'Lasso Select' option in the scatterplot to select points and view its location on the map."),
        actionButton(ns('go'),
                     label = 'Enter')
      )
      
    })

    
    })
  }