# This module returns the selection widgets on the Equity tab. 

equity_widgets_ui <- function(id) {
  ns <- NS(id)
 
  geogs <- c("TAZ"='zone', "FAZ"='faz', "Cities" = 'city', "Control" = "control")
  inds <- c("Mean Income" = "mean_income", "% Low Income" = "low_income", "% High Income" = "high_income", 
            "% White" = "pop_white", "% Non-white" = "pop_non_white", 
            "% Black" = "pop_black", "% Asian" = "pop_asian", "% Hispanic" = "pop_hsp")
  
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
          
#          selectInput(ns('valtype'),
#                      label = 'Map values',
#                      choices = c("Difference", "Totals")),
          
          actionButton(ns('go'),
                       label = 'Enter'),
          
#          conditionalPanel(condition = "input.valtype == 'Difference'",
#                           ns = ns,
#                          div(style = "margin-top: 3rem;",
#                            helpText("Use the 'Box Select' or 'Lasso Select' option in the scatterplot to select points, view its location on the map, 
#                                      and filter records in the table. Double-click on plot to de-select.")
#                          )
#          )
        ) # end wellPanel
  ) # end taglist
  
}

equity_widgets_server <- function(id, paths, baseyears, alldata) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$uiRun <- renderUI({
      selectInput(ns('run'),
                  label = 'Select a run',
                  choices = paths
                  )
    })
  }) # end server
}