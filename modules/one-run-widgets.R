one_run_widgets_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('uiCtMismatch'))
  )
  
}

one_run_widgets_server <- function(id, paths) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$uiCtMismatch <- renderUI({
      tagList(
        wellPanel(
        selectInput(session$ns('run'),
                    label = 'Select Run',
                    choices = paths),
        selectInput(ns('year'),
                    label = 'Year',
                    choices = rev(years)),
        actionButton(ns('go'),
                     label = 'Enter')
        )

      )
    })
  })
}