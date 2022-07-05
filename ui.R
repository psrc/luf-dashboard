fluidPage(
  theme = shinytheme("flatly"),
  navbarPage("Land Use Forecast Dashboard",
             tabPanel("One-Run",
                      sidebarPanel(width = 3,
                                   run_choice_ui('runChoice_one', 'Run Choice', 'Select Run', FALSE)
                      )
             ), # end tabPanel

# multi-run ---------------------------------------------------------------

             
             tabPanel("Multi-Run",
                      sidebarPanel(width = 3,
                                   run_choice_ui('runChoice_multi', 'Run Choices', 'Select Runs', TRUE),
                                   conditionalPanel(condition = "input.multiTab == 'runcomparison'",
                                                    wellPanel(multi_scat_map_data_ui('runComp')))
                                   
                                   ),
                      mainPanel(
                      tabsetPanel(id = 'multiTab',
                                  type = 'tabs',
                                  tabPanel('Run Comparison',
                                           value = 'runcomparison',
                                           plot_map_tbl_ui('runCompContent')
                                  ), # end tabPanel
                                  
                                  tabPanel('Something Else',
                                           value = 'se',
                                           mainPanel(width = 9)
                                    
                                  ), # end tabPanel
                                  
                                  tabPanel('Other',
                                           value = 'o',
                                           mainPanel(width = 9)
                                           
                                  ) # end tabPanel
                        
                      ) # end tabsetPanel
                      )# end MainPanel
             ) # end tabPanel
  
  ) # end navbarPage
) # end fluidPage

