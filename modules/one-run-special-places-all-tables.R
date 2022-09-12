# This module calls one-run-special-places-table and creates a DT formatted table with header for many indicators on the 
# Special Places tab

sp_places_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    sp_places_table_ui(ns('Tp')),
    sp_places_table_ui(ns('Hh')),
    sp_places_table_ui(ns('Emp'))
  )
  
}

sp_places_server <- function(id, runs, paths, alldata, baseyears, inputyear) {
  moduleServer(id, function(input, output, session) {
    sp_places_table_server('Tp', 
                           'Total Population', 
                           'Total Population', 
                           runs, 
                           paths, 
                           alldata, 
                           baseyears, 
                           inputyear)
    
    sp_places_table_server('Hh', 
                           'Household', 
                           'Households', 
                           runs, 
                           paths, 
                           alldata, 
                           baseyears, 
                           inputyear)
    
    sp_places_table_server('Emp', 
                           'Employment', 
                           'Employment', 
                           runs, 
                           paths, 
                           alldata, 
                           baseyears, 
                           inputyear)
  })
}