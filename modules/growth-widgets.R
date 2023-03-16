# This module returns the selection widgets on the Growth tab. Similar to Run Comparison except for only one run and utilizes a time slider.

growth_widgets_ui <- function(id) {
  ns <- NS(id)
 
  geogs <- c("TAZ"='zone', "FAZ"='faz', "Cities" = 'city')
  inds <- c("Total Population", "Households", "Employment", "Residential Units")
  
  tagList(
        wellPanel(
          uiOutput(ns('uiRun')),
          selectInput(ns('geography'),
                      label = 'Geography',
                      choices = geogs,
                      selected = 'faz'),
          selectInput(ns('indicator'),
                      label = 'Indicator',
                      choices = inds),
          
          conditionalPanel(condition = "(input.indicator == 'Residential Units' | input.indicator == 'Households') &&
                                                input.geography == 'faz'",
                           ns = ns,
                           radioButtons(ns("structure"),
                                        label = h5("Categories"),
                                        choices = list("All" = "All", "Single Family" = "singlefamily", "Multi-Family" = "multifamily"),
                                        selected = "All")
          ),
          
          uiOutput(ns('uiGrowthTimeSlider')),
          actionButton(ns('go'),
                       label = 'Enter'),
          div(style = "margin-top: 3rem;",
              helpText("Use the 'Box Select' or 'Lasso Select' option in the scatterplot to select points, view its location on the map, 
                 and filter records in the table. Double-click on plot to de-select.")
          )
        ) # end wellPanel
  ) # end taglist
  
}

growth_widgets_server <- function(id, paths, baseyears, alldata) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$uiRun <- renderUI({
      selectInput(ns('run'),
                  label = 'Select a run',
                  choices = paths
                  )
    })
  
    output$uiGrowthTimeSlider <- renderUI({
      sliderInput(ns('years'),
                  label = "Time Period",
                  min = years[1],
                  max = years[length(years)],
                  value = c(as.numeric(time_slider()), years[length(years)]),
                  step = 1,
                  sep = "")

    }) # end renderUI
    
    time_slider <- reactive({
      # for selected run, find its baseyear
      
      r <- get_runnames(input$run)
      str_extract(baseyears[run %in% r, baseyear], "\\d+")
    })
    
  }) # end server
}