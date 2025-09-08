# This module returns the selection widgets on the development capacity tab. 

dev_cap_widgets_ui <- function(id) {
  ns <- NS(id)
  
  geogs <- c("TAZ"='zone', "FAZ"='faz', "Cities" = 'city', "Growth Center" = "growth_center")
  
  tagList(
    wellPanel(
      p("Select the following to view the remaining developable capacity"),
      
      uiOutput(ns("uiRun")), # dynamic, only runs with dev cap indicators will be listed
      
      selectInput(ns("year"),
                  label = "Year",
                  choices = years,
                  selected = tail(years, n=1)), #select the last element of years
      
      selectInput(ns("geography"),
                  label = "Geography",
                  choices = geogs,
                  selected = 'faz'),
      actionButton(ns('go'),
                   label = 'Enter'),
      # uiOutput(ns("uiMessage"))
    ) # end wellPanel
  )
  
}

dev_cap_widgets_server <- function(id, paths) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$uiRun <- renderUI({
      selectInput(ns('run'),
                  label = 'Select a run',
                  choices = paths
      )
    })
    
    # # Display graphs or text depending if Development Capacity indicators exist
    # output$uiMessage <- renderUI({
    #   if (is.null(devdt())){
    #     verbatimTextOutput(ns("message"))
    #   } else {
    #     return(NULL)
    #   }
    # })
    # 
    # output$message <- renderText({
    #   "Development indicators have not yet been generated for any of your selected runs"
    # })
     
    # # Returning a list of runs with DevCap indicators
    # output$dcap_select_run <- renderUI({
    #   if (is.null(devdt())) return(NULL)
    #   select.runs <- unique(devdt[, run])
    #   selectInput(inputId = "dcap_select_run",
    #               label = "Run",
    #               choices = select.runs)
    # })
    
    # dcapRun <- reactive({
    #   input$dcap_select_run
    # })
    
    # dcapGeog <- reactive({
    #   switch(as.integer(input$dcap_select_geography),
    #          "zone",
    #          "faz",
    #          "city",
    #          "growth_center")
    # })
    
    # dcapYear <- reactive({
    #   input$dcap_select_year
    # })
    
  }) # end server
}