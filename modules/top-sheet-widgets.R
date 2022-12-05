topsheet_widgets_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('uiTopsheet'))
  )
  
}

topsheet_widgets_server <- function(id, paths) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$uiTopsheet <- renderUI({
          tagList(
            wellPanel(
              selectInput(session$ns('runs'),
                          label = 'Compare two runs',
                          choices = paths,
                          multiple = TRUE),
              selectInput(ns('year'),
                          label = 'Year',
                          choices = rev(years)),
              actionButton(ns('go'),
                           label = 'Enter')
            ) # end wellPanel
          ) # end tagList
    })
  })
}