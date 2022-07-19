# This module returns the selection widgets on the Run Comparison tab

runcomp_widgets_ui <- function(id) {
  ns <- NS(id)
 
  tagList(
    uiOutput(ns('uiRunComp'))
  )
  
}

runcomp_widgets_server <- function(id, paths) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$uiRunComp <- renderUI({

      # geogs <- c("TAZ"='zone', "FAZ"='faz', "City"='city')
      geogs <- c("TAZ"='zone', "FAZ"='faz')
      inds <- c("Total Population", "Households", "Employment", "Residential Units")

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
        actionButton(ns('go'),
                     label = 'Enter'),
        div(style = "margin-top: 3rem;",
          helpText("Use the 'Box Select' or 'Lasso Select' option in the scatterplot to select points, view its location on the map, 
                 and filter records in the table. Double-click on plot to de-select.")
        )
       
      )

    }) # end renderUI
    
    }) # end server
  }