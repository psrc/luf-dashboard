# This module calls top-sheet-table and creates a DT formatted table with header for many indicators on the top sheet

topsheet_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    dt_ui(ns('topSheetTp')),
    dt_ui(ns('topSheetHh')),
    dt_ui(ns('topSheetEmp'))
  )
  
}

topsheet_server <- function(id, dttable, runs, tsyear, baseyear) {
  moduleServer(id, function(input, output, session) {
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
    
    
  })
}