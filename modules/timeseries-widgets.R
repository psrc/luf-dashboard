timeseries_widgets_ui <- function(id) {
  ns <- NS(id)
  
  faz_lg_areas <- c("Eastside King (1)",
                    "Eastside King (2)",
                    "Green River",
                    "Seattle and Shoreline",
                    "SE King and King Other",
                    "SW King",
                    "Central, North, and South Kitsap",
                    "Peninsula and Tacoma",
                    "Pierce Other (1)",
                    "Pierce Other (2)",
                    "SW Pierce",
                    "Everett",
                    "NW Snohomish",
                    "Snohomish Other",
                    "SW Snohomish (1)",
                    "SW Snohomish (2)")
  
  tagList(
    wellPanel(
      uiOutput(ns('uiRun')),
      selectInput(ns('geog'),
                  label = 'Geography',
                  choices = c('County & Region' = 'county', 
                              'Control HCT' = 'hct',  
                              'Cities' = 'cities', 
                              'FAZ' = 'Faz')),
      
      # conditional panel for cities
      conditionalPanel(
        condition = "input.geog == 'cities' | input.geog == 'Faz' | input.geog == 'hct' ",
        ns = ns,
        uiOutput(ns('uiLgAreaText')),
        selectInput(ns('largeArea'),
                    label = 'FAZ Large Area Groups',
                    choices = faz_lg_areas
        )
      ), # end conditional panel
      actionButton(ns('go'),
                   label = 'Enter')
    ) # end wellPanel
  ) # end tagList
  
}

timeseries_widgets_server <- function(id, paths) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$uiLgAreaText <- renderUI({
      geog <- switch(input$geog,
             'cities' = 'cities',
             'Faz' = 'FAZs',
             'hct' = 'Control HCTs'
             )
      div(class = 'notes', paste('View', geog, 'within a FAZ Large Area'))
    })
    
    output$uiRun <- renderUI({
      selectInput(ns('runs'),
                  label = 'Select run(s)',
                  choices = paths,
                  multiple = TRUE)
    })

  })
}