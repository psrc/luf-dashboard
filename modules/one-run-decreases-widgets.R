dec_widgets_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('uiDecWidgets'))
  )
  
}

dec_widgets_server <- function(id, paths) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$uiDecWidgets <- renderUI({
      tagList(
        wellPanel(
          selectInput(ns('run'),
                      label = 'Select Run',
                      choices = paths),
          selectInput(ns('year'),
                      label = 'Year',
                      choices = rev(years)),
          numericInput(ns('abs'), label = "Absolute Threshold", value = 10),
          numericInput(ns('per'), label = "Percent Threshold", value = 1),
          actionButton(ns('go'),
                       label = 'Enter')
        )
        
      )
      
    })
  })
}