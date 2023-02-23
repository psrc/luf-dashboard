# Generate a matrix of timeseries charts.

timeseries_table_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    plotlyOutput(ns('plots'))
  )
  
}

timeseries_table_server <- function(id, runs, geog, largearea, go, alldata, ctrlhctdata, paths) {
  moduleServer(id, function(input, output, session) {
    
    table <- reactive({
      # returns underlying data table for all visuals

      alldt <- alldata
      runnames <- get_runnames(runs)

      # general data ----
      a <- melt(alldt, 
                id.vars = c('name_id', 'run', 'indicator', 'geography'),
                variable.name = 'year',
                value.name = 'estimate')
      a[, year := as.integer(str_extract(year, "\\d+"))]

      # alldata contains years with no estimates, filter out for cleaner graph
      y <- unique(a[estimate != 0, .(year)])
      y <- y[['year']]
      a <- a[year %in% y & run %in% runnames, ]
      
      ## County/Region ----
      if(geog == 'county') {
        a_cnty <- merge(a[geography == 'zone'], zone.lookup, by.x = "name_id", by.y = "zone_id")
        a_cnty <- a_cnty[, .(estimate = sum(estimate)),  by = c('run', 'indicator', 'year', 'County')]
        setnames(a_cnty, 'County', 'name')

        # regional totals
        a_reg <- a_cnty[, .(estimate = sum(estimate)),  by = c('run', 'indicator', 'year')]
        a_reg <- a_reg[, name := 'Region']
        
        t <- rbindlist(list(a_cnty, a_reg), use.names=TRUE)
        t[, name := factor(name, levels = c('King', 'Kitsap', 'Pierce', 'Snohomish', 'Region'))]
      
      } else if(geog == 'hct') {
        ch <- ctrlhctdata
        ch[run %in% runnames, ]
        
        # merge with lookup table
        t <- merge(ch, ctrlhct.lookup[, .(control_id, control_na, lgarea_group)], by = 'control_id')
        setnames(t, c('attribute', 'control_na'), c('indicator', 'name'))
        t <- t[lgarea_group == largearea]
        
      }
      
      return(t)
    })
    
    output$plots <- renderPlotly({
      t <- table()
      
      if(geog == 'county' ) {
        ggplotly_w <- 1400
        ggplotly_h <- 925
      } else if(geog == 'hct') {
        ggplotly_w <- 1400
        ggplotly_h <- 1000
      }
      
      g <- ggplot(t, aes(x = year, y = estimate, group = run, colour = run)) +
        geom_line() +
        geom_point(size = .75) +
        facet_wrap(~ interaction(indicator, name), scales = "free", shrink = FALSE, ncol = 4) +
        lemon::coord_capped_cart(bottom='both', left='both') +
        labs(x = " ", y = " ") +
        scale_y_continuous(labels = scales::comma) +
        theme(legend.key.size = unit(0.012, "npc"),
              legend.title = element_blank(),
              axis.text.x = element_text(size = 8, hjust = 1),
              text = element_text(family="Poppins"))
      
      if(geog == 'hct') {
       g <- g +
          scale_x_discrete(breaks = seq(2015, 2050, by = 5))
      }
      
      ggplotly(g, width = ggplotly_w, height = ggplotly_h)
    })
    
  
  })
  
}

