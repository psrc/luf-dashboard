navbarPage(title = "Land Use Forecast Dashboard",
           id = 'navbar',
           tags$head(
             tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
           ),
           theme = bs_theme(version = 5),

           # one-run -----------------------------------------------------------------

           
           tabPanel("One-Run",
                    fluidRow(
                      column(width = 3,
                             conditionalPanel(condition = "input.oneTab == 'ct'",
                                              one_run_widgets_ui('mismatch')),
                             conditionalPanel(condition = "input.oneTab == 'sp'",
                                              one_run_widgets_ui('spPlaces')),
                             conditionalPanel(condition = "input.oneTab == 'dec'",
                                              dec_widgets_ui('dec')),
                             conditionalPanel(condition = "input.oneTab == 'gw'",
                                              growth_widgets_ui('growth'))
                      ),
                      column(width = 9,
                             tabsetPanel(id = 'oneTab',
                                         type = 'tabs',
                                         tabPanel('CT Mismatch',
                                                  value = 'ct',
                                                  ct_mismatch_ui('mismatchContent')
                                         ),
                                         tabPanel('Special Places',
                                                  value = 'sp',
                                                  sp_places_ui('spPlacesContent')
                                         ),
                                         tabPanel('Decreases',
                                                  value = 'dec',
                                                  dec_ui('decContent')
                                         ),
                                         tabPanel('Growth',
                                                  value = 'gw',
                                                  growth_plot_map_tbl_ui('growthContent')
                                                  )
                             ) # end tabsetPanel
                      )
                    )
                    
           ), # end tabPanel
           
           # multi-run ---------------------------------------------------------------
           
           
           tabPanel("Multi-Run",
                    fluidRow(
                    column(width = 3,
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

           
          tabPanel('Time Series',
                   fluidRow(
                     column(width = 3,
                            timeseries_widgets_ui('ts')
                            ),
                     column(width = 9,
                            timeseries_plot_ui('tsContent')
                            )
                   ) # end fluidRow
          ), # end tabPanel
           
           nav_spacer(),
           nav_item(actionButton('modal', label = 'Select Runs', icon = icon('folder')))
           
) # end navbarPage


