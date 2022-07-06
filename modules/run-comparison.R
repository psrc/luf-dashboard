# This module returns the selection widgets on the Run Comparison tab and returns a filtered table output
# for visuals under Run Comparison
# This module will also return multiple outputs 

multi_plot_map_data_ui <- function(id) {
  ns <- NS(id)
 
  tagList(
    uiOutput(ns('uiRunComp'))
  )
  
}

multi_plot_map_data_server <- function(id, alldata, strdata, paths) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$uiRunComp <- renderUI({ #if Enter clicked and on Run Comparison tab
      
      # geogs <- c("TAZ"='zone', "FAZ"='faz', "City"='city')
      geogs <- c("TAZ"='zone', "FAZ"='faz')
      inds <- c("Total Population", "Households", "Employment", "Residential Units")
      # years <- 2014:2050
      
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
          
          # condition = "(input.indicator == 'Residential Units' | input.indicator == 'Households') && output.strdtavail &&
          #                                       input.geography == 'faz'",
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
        helpText("Use the 'Box Select' or 'Lasso Select' option in the scatterplot to select points and view its location on the map."),
        actionButton(ns('go'),
                     label = 'Enter')
      )
      
    })
    
    baseyears <- eventReactive(input$go, {
      year <- paste0("yr", input$year)

      # for each run, find its baseyear
      a <- alldata[, lapply(.SD, sum), .SDcols = patterns("^yr"), by = .(run)]
      b.yrs <- names(a[,2:ncol(a)])[max.col(a[,2:ncol(a)] != 0, ties.method = 'first')]

      # return a df and subset for chosen runs
      b <- a[, .(run)][, baseyear := b.yrs]
      b[run %in% names(paths)]
    })
    
    table <- eventReactive(input$go, {
      strdt <- strdata
      alldt <- alldata
      byears <- baseyears()

      runnames <- get_runnames(input$runs)

      if (is.null(input$structure) | input$structure == "All" | (input$indicator %in% c("Total Population", "Employment")) |
          (input$indicator %in% c("Households", "Residential Units") & input$geography %in% c("zone", "city")) ){

        # run 1
        b1 <- byears[run == runnames[1],][['baseyear']]
        dt1 <- alldt[run == runnames[1] & geography == input$geography & indicator == input$indicator,
                     .(name_id, geography, indicator, get(b1), get(paste0('yr',input$year)))]
        setnames(dt1, dt1[,c((ncol(dt1)-1), ncol(dt1))], c('base_estrun1', 'estrun1'))

        # run 2
        b2 <- byears[run == runnames[2],][['baseyear']]
        dt2 <- alldt[run == runnames[2] & geography == input$geography & indicator == input$indicator,
                     .(name_id, get(b2),get(paste0('yr', input$year)))]
        setnames(dt2, dt2[,c((ncol(dt2)-1), ncol(dt2))], c('base_estrun2', 'estrun2'))

        dt <- merge(dt1, dt2, by = 'name_id')
      } else {
        
        # run 1
        b1 <- str_extract(byears[run == runnames[1],][['baseyear']], "\\d+")
        dt1 <- strdt[run == runnames[1] & geography == input$geography & (year == b1 | year == input$year) & indicator == input$indicator & strtype == input$structure]
        dt1.cast <- dcast.data.table(dt1, name_id + indicator + geography ~ year, value.var = "estimate")
        setnames(dt1.cast, colnames(dt1.cast)[4:5], c('base_estrun1', 'estrun1'))

        # run 2
        b2 <- str_extract(byears[run == runnames[2],][['baseyear']], "\\d+")
        dt2 <- strdt[run == runnames[2] & geography == input$geography & (year == b2 | year == input$year)  & indicator == input$indicator & strtype == input$structure]
        dt2.cast <- dcast.data.table(dt2, name_id ~ year, value.var = "estimate")
        setnames(dt2.cast, colnames(dt2.cast)[2:3], c('base_estrun2', 'estrun2'))
        dt <- merge(dt1.cast, dt2.cast, by = 'name_id')
      }
      dt[,"diff" := (estrun1-estrun2)]

      dt <- switch(input$geography,
             zone = merge(dt, zone.lookup, by.x = "name_id", by.y = "zone_id") %>% merge(faz.lookup, by = c("faz_id", "County")),
             faz = merge(dt, faz.lookup, by.x = "name_id", by.y = "faz_id")#,
             # merge(dt, city.lookup, by.x = "name_id", by.y = "city_id") %>% setnames("city_name", "Name")
      )
      return(dt)
    })
    
    dttable <- eventReactive(input$go, {
      # clean up column names of table() to render to DT
      
      runnames <- get_runnames(input$runs)
      runnames.trim <- get_trim_runnames(runnames)
      baseyears <- baseyears()

      dt <- table()[, .(County, indicator, geography, name_id, Name, base_estrun1, estrun1, base_estrun2, estrun2, diff)]
      setnames(dt, c("County", "Indicator", "Geography", "ID", "Name",
                     paste("Base", get_trim_runnames(baseyears[1, ][['run']]), str_extract(baseyears[1, ][['baseyear']], "\\d+")),
                     paste(runnames.trim[1], input$year),
                     paste("Base", get_trim_runnames(baseyears[2, ][['run']]), str_extract(baseyears[2, ][['baseyear']], "\\d+")),
                     paste(runnames.trim[2], input$year),
                     "Difference"))
      return(dt)
    })
    
    list(
      # this module will return multiple outputs 
      baseyears = reactive(baseyears()),
      table = reactive(table()),
      dttable = reactive(dttable())
    )
    
    })
  }