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
  
  faz_lg_areas_long <- c("Eastside King",
                         "Green River",
                         "Seattle",
                         "Shoreline",
                         "SE King",
                         "SW King",
                         "King Other",
                         "Central Kitsap",
                         "North Kitsap",
                         "South Kitsap",
                         "Peninsula",
                         "Tacoma",
                         "Pierce Other",
                         "SW Pierce",
                         "Everett",
                         "NW Snohomish",
                         "Snohomish Other",
                         "SW Snohomish")
  
  tagList(
    wellPanel(
      uiOutput(ns('uiRun')),
      selectInput(ns('geog'),
                  label = 'Geography',
                  choices = c('County & Region' = 'county', 
                              'Control' = 'control',  
                              'Control HCT' = 'control_hct',
                              'Cities' = 'cities', 
                              'FAZ' = 'Faz')),
      # conditional panel for cities
      conditionalPanel(
        condition = "input.geog == 'cities'",
        ns = ns,
        div(class = 'notes', 'View Cities within a FAZ Large Area'),
        selectInput(ns('largeArea'),
                    label = 'FAZ Large Area Groups',
                    choices = faz_lg_areas
        ),
      ), # end conditional panel
      # conditional panel for Control
      conditionalPanel(
        condition = "input.geog == 'control' || input.geog == 'control_hct'",
        ns = ns,
        div(class = 'notes', 'View Controls within a FAZ Large Area'),
        selectInput(ns('largeAreaHct'),
                    label = 'FAZ Large Area Groups',
                    choices = faz_lg_areas
        )
      ), # end conditional panel
      # conditional panel for FAZ
      conditionalPanel(
        condition = "input.geog == 'Faz'",
        ns = ns,
        div(class = 'notes', 'View FAZs within a FAZ Large Area'),
        selectInput(ns('largeAreaFaz'),
                    label = 'FAZ Large Areas',
                    choices = faz_lg_areas_long
        )
      ), # end conditional panel
      radioButtons(ns('years'),
                   label = 'Display Years',
                   choices = c('Limited', 'All'),
                   inline = TRUE
      ),
      actionButton(ns('go'),
                   label = 'Enter')
    ) # end wellPanel
  ) # end tagList
  
}

timeseries_widgets_server <- function(id, paths) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$uiRun <- renderUI({
      selectInput(ns('runs'),
                  label = 'Select run(s)',
                  choices = paths,
                  multiple = TRUE)
    })

  })
}