# This module calls top-sheet-table and creates a DT formatted table with header for many indicators on the top sheet

topsheet_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('uiTables'))
  )
}

topsheet_server <- function(id, dttable, alldata, runs, tsyear, baseyear, paths) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$uiTables <- renderUI({
      if(is.null(alldata)) return(NULL)
      tagList(
        br(),
        tabsetPanel(id = ns('tabsetTopSheet'),
                    type = 'pills',
                    tabPanel('Total Population',
                             dt_ui(ns('topSheetTp'))
                    ),
                    tabPanel('Households',
                             dt_ui(ns('topSheetHh'))
                    ),
                    tabPanel('Employment',
                             dt_ui(ns('topSheetEmp'))
                    ),
                    tabPanel('Jobs by Sector',
                             dt_jobs_sector_ui(ns('topSheetJobSect'))
                    ),
                    tabPanel('Centers',
                             dt_centers_ui(ns('topSheetCtr'))
                    ),
                    tabPanel('Key Locations',
                             dt_key_loc_ui(ns('topSheetKeyLoc'))
                    )
                    
        ) # end tabsetPanel
      ) # end tagList
    })
    
    dt_server('topSheetTp',
              dttable,
              'Total Population',
              'County',
              runs,
              tsyear,
              baseyear,
              'Total Population')
    
    dt_server('topSheetHh',
              dttable,
              'Households',
              'County',
              runs,
              tsyear,
              baseyear,
              'Households')
    
    dt_server('topSheetEmp',
              dttable,
              'Employment',
              'County',
              runs,
              tsyear,
              baseyear,
              'Employment')
    
    dt_jobs_sector_server('topSheetJobSect', 
                          paths, 
                          runs, 
                          tsyear,  
                          baseyear, 
                          'Jobs by Sector')
    
    dt_centers_server('topSheetCtr', 
                      paths, 
                      runs, 
                      tsyear,  
                      baseyear)
    
    dt_key_loc_server('topSheetKeyLoc', 
                      paths, 
                      runs, 
                      tsyear, 
                      baseyear, 
                      alldata)
    
    
  })
}