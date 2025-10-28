ct_mismatch_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('uiTables'))
   )
  
}

ct_mismatch_server <- function(id, runs, paths, alldata, baseyears, inputyear) {
  moduleServer(id, function(input, output, session) {
  
    ns <- session$ns
    
    output$uiTables <- renderUI({
      if(is.null(mmdt())) return(NULL)
      tagList(
        fluidRow(
          column(width = 6,
            card(
              card_header(
                class = "bg-secondary",
                "Mismatch Summary"
              ),
              card_body(
                DTOutput(ns("summary"))
              )
              , class = 'ct-card') # end card
          
        ), # end column
        column(width = 6,
          card(
            card_header(
              class = "bg-secondary",
              "Regional Totals"
            ),
            card_body(
              DTOutput(ns("totals"))
            )
            , class = 'ct-card') # end card
        ) # end column
     
        ),
        
        h2("Detail"),
        DTOutput(ns("records")) # data
      )
    })
    
    mmdt <- reactive({
      runnames <- get_runnames(runs)
      path <- paths[names(paths) %in% runnames]

      # baseyear for run
      byr <- str_extract(baseyears[run == runnames, baseyear], "\\d+")
      yrs <- c(byr, inputyear)

      indicator.names <- c('Households' = 'households', 'Employment' = 'employment')
      indicator_settings <- list(households = c("total_number_of_households", "household"), employment = c("total_number_of_jobs", "employment"))

      result <- report <- totals <- NULL
      for (ind in indicator.names) {
        
        # Does control file exist in directory?
        ct.file.name <- paste0('annual_', indicator_settings[[ind]][2], '_control_totals.csv')
        p <- file.path(path, 'indicators', ct.file.name)
        does.file.exist <- file.exists(p)
        if(!does.file.exist)
          p <- file.path('data', ct.file.name)
        
        # Read control totals
        #if(does.file.exist == TRUE) {
          ct <- read.csv(p)
          by <- c()
          is.regional <- TRUE
          if('subreg_id' %in% colnames(ct)){
            # CT by subreg_id
            by <- c(by, "subreg_id")
            is.regional <- FALSE
          }
          ct.by.jur <- data.table(ct)[,list(ct = sum(get(indicator_settings[[ind]][1]))), by = c(by, "year")]
          ct.by.jur <- ct.by.jur[year == yrs[2],]
          if(is.regional){
            ct.by.jur[, subreg_id := 1]
            ind.values <- data.table(read.csv(file.path(path, 'indicators', paste0('alldata__table__', ind, '.csv')))) # regional values
            setnames(ind.values, "alldata_id", "subreg_id")
          } else {
            ind.values <- data.table(read.csv(file.path(path, 'indicators', paste0('subreg__table__', ind, '.csv')))) # values by subreg
          }
          cols <- c('subreg_id', str_subset(colnames(ind.values), paste0(".*", yrs[2], "$")))
          ind.values <- ind.values[, ..cols]
          ct.join <- merge(ct.by.jur, ind.values, by='subreg_id')
          no.match <- ct.join[ct != get(cols[2]),]
          this.report <- data.table(indicator = ind, total = nrow(no.match), max_percent = NA)
        # } else {
        #   alldt <- alldata
        #   ct <- read.table(file.path('data', paste0('annual_', indicator_settings[[ind]][2], '_control_totals.csv')), sep = ',', header=TRUE)
        #   ct.by.jur <- data.table(ct)[,list(ct = sum(get(indicator_settings[[ind]][1]))), by = c("city_id", "year")]
        #   ct.by.jur <- ct.by.jur[year == yrs[2],]
        #   ind.values <- alldt[geography == 'city' & run == runnames & indicator == eval(names(grep(ind, indicator.names, value = TRUE))), .SD, .SDcols = c("name_id", paste0("yr", yrs[2]))]
        #   setnames(ind.values, c("name_id", paste0("yr", yrs[2])), c("city_id", paste(ind, yrs[2], sep='_')))
        #   ct.join <- merge(ct.by.jur, ind.values, by = 'city_id')
        #   no.match <- ct.join[ct != get(paste(ind, yrs[2], sep = "_")),]
        #   this.report <- data.table(indicator = ind, total = nrow(no.match), max_percent = NA)
        # }
        
        # calculate ct, simulated, diff for regional total
        this.totals <- ct.join[, lapply(.SD, sum), .SDcols = c('ct', str_subset(colnames(ct.join), paste0(".*", yrs[2])))
                               ][, indicator := eval(ind)]

        setnames(this.totals,  str_subset(colnames(ct.join), paste0(".*", yrs[2])), paste('simulated', yrs[2]))
        this.totals <- setcolorder(this.totals, c('indicator', 'ct', paste('simulated', yrs[2])))
        this.totals[, difference := get(eval(paste('simulated', yrs[2])))-ct]
    
        if(nrow(no.match) > 0) {
          simcol <- paste0("simulated_", yrs[2])
          colnames(no.match)[colnames(no.match) == paste(ind, yrs[2], sep="_")] <- simcol
          dif <- no.match[[simcol]] - no.match$ct
          dif.percent <- dif/no.match[[simcol]] * 100
          this.result <- cbind(data.table(indicator = rep(ind, nrow(no.match)), no.match),
                               data.table(difference = dif, percent = round(dif.percent,1)))
          result <- rbind(result, this.result)
          this.report[,'max_percent'] <- max(abs(this.result$percent))
        }
        
        report <- rbind(report, this.report)
        totals <- rbind(totals, this.totals)
        
      }

      dlist <- list("report" = report, "result" = result, "totals" = totals)
      return(dlist)
    })
    
    output$summary <- renderDT({
      new.colnames <- str_to_title(str_replace_all(colnames(mmdt()$report), "_", " "))
      new.colnames <- replace(new.colnames, which(new.colnames %in% c('Total')), 'Number of IDs with Differences')
      new.colnames <- replace(new.colnames, which(new.colnames %in% c('Max Percent')), 'Max Percent Difference')
      
      datatable(mmdt()$report,
                colnames = new.colnames,
                rownames = FALSE,
                options = list(dom = 't'))
    })
    
    output$totals <- renderDT({
      # regional totals, with and without differences between simulated estimate and control total
      d <- mmdt()$totals

      new.colnames <- str_to_title(colnames(d))
      new.colnames <- replace(new.colnames, which(new.colnames %in% c('Ct')), c('Control Total'))
      new.colnames <- str_to_title(str_replace_all(new.colnames, "_", " "))
      
      datatable(d,
                colnames = new.colnames,
                rownames = FALSE,
                options = list(dom = 't'))
    })
    
    output$records <- renderDT({
      new.colnames <- str_to_title(str_replace_all(colnames(mmdt()$result), "_", " "))
      new.colnames <- replace(new.colnames, which(new.colnames %in% c('Subreg Id')), c('Subreg ID'))
      new.colnames <- replace(new.colnames, which(new.colnames %in% c('Ct')), c('Control Total'))

      datatable(mmdt()$result,
                colnames = new.colnames,
                rownames = FALSE,
                filter = 'top')
    })
  })
}