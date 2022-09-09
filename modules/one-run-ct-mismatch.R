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
      tagList(
        h2("Summary"),
        DTOutput(ns("summary")), # top level summary
        h2("Detail"),
        DTOutput(ns("records")) # data
      )
    })
    
    mmdt <- reactive({
      alldt <- alldata
      runnames <- get_runnames(runs)
      path <- paths[names(paths) %in% runnames]

      # baseyear for run
      byr <- str_extract(baseyears[run == runnames, baseyear], "\\d+")
      yrs <- c(byr, inputyear)

      indicator.names <- c('Households' = 'households', 'Employment' = 'employment')
      indicator_settings <- list(households = c("total_number_of_households", "household"), employment = c("total_number_of_jobs", "employment"))

      result <- report <- NULL
      for (ind in indicator.names) {
        
        # Does control file exist in directory?
        p <- file.path(path, 'indicators', paste0('annual_', indicator_settings[[ind]][2], '_control_totals.csv'))
        does.file.exist <- file.exists(p)
        
        # Read control totals
        if(does.file.exist == TRUE) {
          ct <- read.csv(p)
          ct.by.jur <- data.table(ct)[,list(ct = sum(get(indicator_settings[[ind]][1]))), by = c("subreg_id", "year")]
          ct.by.jur <- ct.by.jur[year == yrs[2],]
          ind.values <- data.table(read.csv(file.path(path, 'indicators', paste0('subreg__table__', ind, '.csv')))) # city by run by indicator
          cols <- c('subreg_id', str_subset(colnames(ind.values), paste0(".*", yrs[2], "$")))
          ind.values <- ind.values[, ..cols]
          ct.join <- merge(ct.by.jur, ind.values, by='subreg_id')
          no.match <- ct.join[ct != get(cols[2]),]
          this.report <- data.table(indicator = ind, total = nrow(no.match), max.percent = NA)
        } else {
          ct <- read.table(file.path('data', paste0('annual_', indicator_settings[[ind]][2], '_control_totals.csv')), sep = ',', header=TRUE)
          ct.by.jur <- data.table(ct)[,list(ct = sum(get(indicator_settings[[ind]][1]))), by = c("city_id", "year")]
          ct.by.jur <- ct.by.jur[year == yrs[2],]
          ind.values <- alldt[geography == 'city' & run == runnames & indicator == eval(names(grep(ind, indicator.names, value = TRUE))), .SD, .SDcols = c("name_id", paste0("yr", yrs[2]))]
          setnames(ind.values, c("name_id", paste0("yr", yrs[2])), c("city_id", paste(ind, yrs[2], sep='_')))
          ct.join <- merge(ct.by.jur, ind.values, by = 'city_id')
          no.match <- ct.join[ct != get(paste(ind, yrs[2], sep = "_")),]
          this.report <- data.table(indicator = ind, total = nrow(no.match), max.percent = NA)
        }
        
       
        if(nrow(no.match) > 0) {
          simcol <- paste0("simulated_", yrs[2])
          colnames(no.match)[colnames(no.match) == paste(ind, yrs[2], sep="_")] <- simcol
          dif <- no.match[[simcol]] - no.match$ct
          dif.percent <- dif/no.match[[simcol]] * 100
          this.result <- cbind(data.table(indicator = rep(ind, nrow(no.match)), no.match),
                               data.table(difference = dif, percent = round(dif.percent,1)))
          result <- rbind(result, this.result)
          this.report[,'max.percent'] <- max(abs(this.result$percent))
        }
        
        report <- rbind(report, this.report)
      
      }
      
      dlist <- list("report" = report, "result" = result)
      return(dlist)
    })
    
    output$summary <- renderDT({
      datatable(mmdt()$report,
                colnames = str_to_title(colnames(mmdt()$report)),
                rownames = FALSE,
                options = list(dom = 't'))
    })
    
    output$records <- renderDT({
      new.colnames <- str_to_title(str_replace_all(colnames(mmdt()$result), "_", " "))
      new.colnames <- replace(new.colnames, which(new.colnames %in% c('Subreg Id', 'Ct')), c('Subreg ID', 'Control Total'))

      datatable(mmdt()$result,
                colnames = new.colnames,
                rownames = FALSE,
                filter = 'top')
    })
  })
}