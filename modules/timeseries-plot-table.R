# Generate a matrix of timeseries charts.

timeseries_plot_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    plotlyOutput(ns('plots'))
  )
  
}

timeseries_plot_server <- function(id, runs, geog, tsyears, largearea, largeareahct, largeareafaz, go, alldata, 
                                   ctrldata, ctrlhctdata, paths) {
  moduleServer(id, function(input, output, session) {
    
    table <- eventReactive(go, {
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
      a <- a[run %in% runnames]
      y <- unique(a[estimate != 0, .(year)])
      y <- y[['year']]
      a <- a[year %in% y, ]
      if(tsyears == "Limited") a <- a[year %in% limited.years]
      
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
      
      } else if(geog == 'control') {
      ## Control ----  
        ch <- ctrldata
        ch <- ch[run %in% runnames, ]
        if(tsyears == "Limited") ch <- ch[year %in% limited.years]
        # merge with lookup table
        t <- merge(ch, ctrl.lookup[, .(control_id, control_name, lgarea_group)], by.x = 'name_id', by.y = 'control_id')
        setnames(t, c('control_name'), c('name'))
        t <- t[lgarea_group == largeareahct]
        
      } else if(geog == 'control_hct') {
        ## Control HCT ----  
        ch <- ctrlhctdata
        ch <- ch[run %in% runnames, ]
        if(tsyears == "Limited") ch <- ch[year %in% limited.years]
        # merge with lookup table
        t <- merge(ch, ctrlhct.lookup[, .(control_hct_id, control_hct_name, lgarea_group)], by.x = 'name_id', by.y = 'control_hct_id')
        setnames(t, c('control_hct_name'), c('name'))
        t <- t[lgarea_group == largeareafaz]
        
      } else if(geog == 'cities') {
      ## Cities ----
        # if(cityyears == 'All') {
        #   a_city <- cities_an_data
        #   
        #   # merge with lookup table
        #   t <- merge(a_city, city.lookup, by = 'city_id')
        # } else {
          a_city <- a[geography == 'city']
          
          # merge with lookup table
          t <- merge(a_city, city.lookup, by.x = 'name_id', by.y = 'city_id')
          
        #}
        setnames(t, c('city_name'), c('name'))
        t <- t[lgarea_group == largearea]

      } else if(geog == 'Faz') {
      ## FAZ ----
        a_faz <- a[geography == 'faz']
        t <- merge(a_faz, faz.lookup, by.x = 'name_id', by.y = 'faz_id')
        t[, name := paste0(Name, " (", name_id, ")")]
        t <- t[LARGE_AREA == largeareafaz]
      }
      
      return(t)
    })
    
    output$plots <- renderPlotly({
      t <- table()
      
      num_juris <- length(unique(t$name))
      num_indic <- length(unique(t$indicator[!is.na(t$indicator)]))
      ggplotly_w <- 1400
      
      if(geog == 'county') {
        ggplotly_h <- 1200
      } else if(geog %in% c('control', 'control_hct', 'cities', 'Faz')) {
        if(num_juris <= 5) {
          ggplotly_h <- 925
        } else if(num_juris > 5 & num_juris <= 10){
          ggplotly_h <- 2000
        } else {
          ggplotly_h <- 3000
        }
      }
      
      g <- ggplot(t, aes(x = year, y = estimate, group = run, colour = run)) +
        geom_line() +
        geom_point(size = .75) +
        facet_wrap(~ interaction(indicator, name), scales = "free", shrink = FALSE, ncol = num_indic) +
        lemon::coord_capped_cart(bottom='both', left='both') +
        labs(x = " ", y = " ") +
        scale_y_continuous(labels = scales::comma) +
        theme(legend.key.size = unit(0.012, "npc"),
              legend.title = element_blank(),
              axis.text.x = element_text(size = 8, hjust = 1),
              text = element_text(family="Poppins"))
      #if(geog == 'control') browser()
      #if(geog == 'control' | tsyears == 'All') {
      # g <- g +
      #    scale_x_discrete(breaks = intersect(seq(2015, 2050, by = 5), unique(t$year)))
      #}
      
      ggplotly(g, width = ggplotly_w, height = ggplotly_h)
    })
    
  
  })
  
}

