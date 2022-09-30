dec_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('uiTables'))
  )
  
}

dec_server <- function(id, runs, paths, alldata, baseyears, inputyear, abs_threshold, per_threshold) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$uiTables <- renderUI({
      if(is.null(decdt())) return(NULL)
      tagList(
        h2("Summary"),
        DTOutput(ns("summary")), # top level summary
        h2("Detail"),
        DTOutput(ns("records")) # data
      )
    })
    
    decdt <- reactive({
      alldt <- alldata
      runnames <- get_runnames(runs)
      path <- paths[names(paths) %in% runnames]

      # baseyear for run
      byr <- str_extract(baseyears[run == runnames, baseyear], "\\d+")
      yrs <- c(byr, inputyear)
      sel.yrs.col <- paste0('yr', yrs)
      
      absolute.threshold <- abs_threshold
      percent.threshold <- per_threshold
      
      indicator.names <- c('Households' = 'households', 'Total Population' = 'population', 'Employment' = 'employment')
      
      # Check decreases
      result <- report <- NULL
      for (ind in indicator.names) {
        for (geo in geography) {
          ind.values <- alldt[geography == eval(geo) & run == runnames & indicator == eval(names(grep(ind, indicator.names, value = TRUE))), ] 
          dif <- ind.values[, diff := get(eval(sel.yrs.col[2]))- get(eval(sel.yrs.col[1]))][['diff']]
          dif.percent <- -dif/ind.values[,get(eval(sel.yrs.col[1]))]*100
          negatives <- dif < -absolute.threshold
          if(percent.threshold > 0) negatives <- negatives & dif.percent < percent.threshold
          negatives <- which(negatives)
          lneg <- length(negatives)
          this.report <- data.frame(indicator=ind, geo=geo, total=nrow(ind.values), negat=lneg, remains=nrow(ind.values)-lneg, percent=round(lneg/nrow(ind.values)*100,2),
                                    max.neg=NA, max.loc=NA, median.neg=NA)
          if(lneg>0) {				
            this.result <- cbind(ind.values[negatives, name_id], dif[negatives], round(dif.percent[negatives],2))
            this.result <- data.frame(indicator=rep(ind, lneg), geography=rep(geo, lneg), this.result)
            result <- rbind(result, this.result)
            this.report[,'max.neg'] <- -min(dif[negatives])
            this.report[,'max.loc'] <- ind.values[which.min(dif), name_id]
            this.report[,'median.neg'] <- -median(dif[negatives])
          }
          report <- rbind(report, this.report)
        }
      }
      
      # output result table
      colnames(result)[3:ncol(result)] <- c('geo_id', 'difference', 'percent')
      
      dlist <- list("report" = report, "result" = result)
      return(dlist)
    })
    
    
    output$summary <- renderDT({
      new.colnames <- colnames(decdt()$report)
      new.colnames <- str_replace_all(new.colnames, 'geo', 'geography')
      new.colnames <- str_replace_all(new.colnames, 'neg(at)*', 'negative')
      new.colnames <- str_replace_all(new.colnames, '\\.', ' ')
      new.colnames <- str_to_title(new.colnames)
      
      
      datatable(decdt()$report,
                rownames = FALSE,
                colnames = new.colnames,
                options = list(dom = 't'))
    })
    
    output$records <- renderDT({
      new.colnames <- colnames(decdt()$result)
      new.colnames <- str_replace_all(new.colnames, 'geo_id', 'Geo ID')
      new.colnames <- str_to_title(new.colnames)

      datatable(decdt()$result,
                rownames = FALSE,
                colnames = new.colnames,
                filter = 'top')
    })
    
    

    
  })
}