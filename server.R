# Define server logic required
server <- function(input, output, session) {

  run_choice_server('runChoice_one', root_dir = rund)
  run_choice_server('runChoice_multi', root_dir = rund)
  
  # display absolute paths in main panel
  path <- eventReactive(input$`runChoice_multi-go`, {
    input$`runChoice_multi-allRuns`
  })
  
  output$disp_multi <- renderText({
    path()
  })
  
}