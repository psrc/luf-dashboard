fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  theme = shinytheme("flatly"),
  navbarPage("Land Use Forecast Dashboard",
             tabPanel("One-Run",
                      sidebarPanel(width = 3,
                                   run_choice_ui('runChoice_one', 'Run Choice', 'Select Run', FALSE)
                      )
             ), # end tabPanel
             
             # multi-run ---------------------------------------------------------------
             
             
             tabPanel("Multi-Run",
                      column(width = 3,
                             run_choice_ui('runChoice_multi', 'Run Choices', 'Select Runs', TRUE),
                             conditionalPanel(condition = "input.multiTab == 'runcomparison'",
                                              runcomp_widgets_ui('runComp')),
                             conditionalPanel(condition = "input.multiTab == 'topsheet'",
                                              topsheet_widgets_ui('topSheet')
                             )
                      ),
                      column(width = 9, 
                             tabsetPanel(id = 'multiTab',
                                         type = 'tabs',
                                         tabPanel('Top Sheet',
                                                  value = 'topsheet',
                                                  dt_ui('topSheetContent', 'My Table')
                                                  
                                                  
                                         ), # end tabPanel
                                         
                                         tabPanel('Run Comparison',
                                                  value = 'runcomparison',
                                                  runcomp_plot_map_tbl_ui('runCompContent')
                                         ), # end tabPanel
                                         
                                        
                                         
                                         tabPanel('Other',
                                                  value = 'o',
                                                  mainPanel(width = 9)
                                                  
                                         ) # end tabPanel
                             ) # end column
                      ) # end tabsetPanel
                      
             ) # end tabPanel
             
  ) # end navbarPage
) # end fluidPage

