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
        # Read control totals
        CT <- read.table(file.path('data', paste0('annual_', indicator_settings[[ind]][2], '_control_totals.csv')), sep=',', header=TRUE)
        CT.by.jur <- data.table(CT)[,list(CT=sum(get(indicator_settings[[ind]][1]))), by=.(city_id, year)]
        CT.by.jur <- CT.by.jur[year == yrs[2],]
        ind.values <- alldt[geography == 'city' & run == runnames & indicator == eval(names(grep(ind, indicator.names, value = TRUE))), .SD, .SDcols = c("name_id", paste0("yr", yrs[2]))]
        setnames(ind.values, c("name_id", paste0("yr", yrs[2])), c("city_id", paste(ind, yrs[2], sep='_')))
        ct.join <- merge(CT.by.jur, ind.values, by='city_id')
        no.match <- ct.join[CT != get(paste(ind, yrs[2], sep="_")),]
        this.report <- data.frame(indicator=ind, total=nrow(no.match), max.percent=NA)
        if(nrow(no.match) > 0) {
          simcol <- paste0("simulated_", yrs[2])
          colnames(no.match)[colnames(no.match) == paste(ind, yrs[2], sep="_")] <- simcol
          dif <- no.match[[simcol]] - no.match$CT
          dif.percent <- dif/no.match[[simcol]] * 100
          this.result <- cbind(data.frame(indicator=rep(ind, nrow(no.match)),  no.match),
                               data.frame(difference=dif, percent=round(dif.percent,1)))
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
                rownames = FALSE,
                options = list(dom = 't'))
    })
    
    output$records <- renderDT({
      datatable(mmdt()$result,
                rownames = FALSE,
                filter = 'top')
    })
  })
}