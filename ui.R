navbarPage(title = "Land Use Forecast Dashboard",
           id = 'navbar',
           tags$head(
             tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
           ),
           theme = bs_theme(version = 5),

           # one-run -----------------------------------------------------------------

           
           tabPanel("One-Run",

           ), # end tabPanel
           
           # multi-run ---------------------------------------------------------------
           
           
           tabPanel("Multi-Run",
                    fluidRow(
                    column(width = 3,
                           # run_choice_ui('runChoice_multi', 'Run Choices', 'Select Runs', TRUE),
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
                                                topsheet_ui('topSheetContent')
                                                
                                                
                                       ), # end tabPanel
                                       
                                       tabPanel('Run Comparison',
                                                value = 'runcomparison',
                                                runcomp_plot_map_tbl_ui('runCompContent')
                                       ) # end tabPanel
                                       
                           ) # end column
                    )
                    ) # end tabsetPanel
                    
           ), # end tabPanel

          # time series -------------------------------------------------------------

           
           tabPanel('Time Series'),
           
           nav_spacer(),
           nav_item(actionButton('modal', label = 'Select Runs', icon = icon('folder')))
           
) # end navbarPage


