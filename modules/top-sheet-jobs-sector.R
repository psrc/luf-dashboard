# This module returns a DT formatted table with table header for Jobs by Sector for the Top Sheet

dt_jobs_sector_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('header')),
    DTOutput(ns('table'))
  )
  
}

dt_jobs_sector_server <- function(id, paths, runs, tsyear, baseyear, title) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$header <- renderUI({
      h2(title)
    })
    
    jobsectdt <- reactive({
      # read all files and collate into one table
      
      # gather basic run info  
      runs <- get_runnames(runs)
      runnames <- get_trim_runnames(runs)
      
      pat <- "city__dataset_table__employment_by_aggr_sector__\\d+"

      # tabulate for luv years
      jobs.dt <- NULL
      for (r in 1:length(runnames)) {
        jobs.file <- list.files(file.path(paths[r], "indicators"), pattern = paste0(pat, ".tab"))
        for (f in 1:length(jobs.file)){
          t <- read.csv(file.path(paths[r], "indicators", jobs.file[f]), header = TRUE, sep = "\t")
          setDT(t)
          t1 <- t[, 1 := NULL][, lapply(.SD, sum)][, `:=` (run = runs[r], year = str_extract(jobs.file[f], "\\d+"))]
          ifelse(is.null(jobs.dt), jobs.dt <- t1, jobs.dt  <- rbind(jobs.dt, t1))
        } # end sectorJobs.file loop
      } # end runnames loop

      # create separate table for non-luv years
      my.dt <-NULL
      for (rn in runs){
        missing.yrs <- setdiff(years, jobs.dt[run == rn, year]) %>% as.vector
        if (length(missing.yrs) == 0) {
          next
        } else {
          missyr.df <- data.frame(matrix(ncol = 2, nrow=length(missing.yrs)))
          colnames(missyr.df) <- c("run", "year")
          missyr.df$year <- missing.yrs
          missyr.df$run <- rn
          ifelse(is.null(my.dt), my.dt <- missyr.df, my.dt <- rbind(my.dt, missyr.df))
        }
      }
      
      # combine both tables
      if (!is.null(my.dt)) {
        my.dt[colnames(jobs.dt)[1:(ncol(jobs.dt)-2)]] <- 0
        setDT(my.dt)
        jobs.dt <- rbindlist(list(jobs.dt, my.dt), use.names = TRUE, fill = TRUE)
      }

      # pivot
      sj <- melt.data.table(jobs.dt, id.vars = c("run", "year"), measure.vars = colnames(jobs.dt)[1:(ncol(jobs.dt)-2)])
      setnames(sj, colnames(sj), c("run", "year", "sector", "estimate"))
      return(sj)
    })
    
   
    tsSectorJobs <- reactive({
      # Filter table and calculate totals for Jobs by Sector table
      
      jobsectdt <- jobsectdt()
      runs <- get_runnames(runs)

      b1 <- baseyear[run == runs[1], .(baseyear)][[1]] |> str_extract("\\d+")
      b2 <- baseyear[run == runs[2], .(baseyear)][[1]] |> str_extract("\\d+")
      
      t <- jobsectdt[((run %in% runs[1]) & (year %in% c(b1, tsyear))) | ((run %in% runs[2]) & (year %in% c(b2, tsyear)))]  
      t.sum <- t[, .(estimate = sum(estimate)), by = list(run, year)][, sector := "Sub-Total: Jobs"]
      rbindlist(list(t, t.sum), use.names = TRUE)
    })
    
    
    output$table <- renderDT({
      # Display Jobs by sector summary table
      
      tsSectorJobs <- tsSectorJobs()
      runs <- get_runnames(runs)
      runnames <- get_trim_runnames(runs)

      b1 <- baseyear[run == runs[1], .(baseyear)][[1]] |> str_extract("\\d+")
      b2 <- baseyear[run == runs[2], .(baseyear)][[1]] |> str_extract("\\d+")
   
      t <- dcast.data.table(tsSectorJobs, sector ~ year + run, value.var = "estimate")
      setnames(t, "sector", "Sector")
    
      t <- calc.cols.tsTable(t, tsyear, runs, baseyear)

      setcolorder(t, c("Sector",
                       paste0(b1, "_", runnames[1]),
                       paste0(b2, "_", runnames[2]),
                       paste0(tsyear, "_", runnames[1]),
                       "r1.baseyr",
                       "r1.baseyr.per",
                       "r1.avgann",
                       paste0(tsyear, "_", runnames[2]),
                       "r2.baseyr",
                       "r2.baseyr.per",
                       "r2.avgann",
                       "Change",
                       "Per.Change"))
      t1 <- t[, c(2:5, 8:9, 12) := lapply(.SD, FUN=function(x) prettyNum(x, big.mark=",")), .SDcols = c(2:5, 8:9, 12)]
      
      sketch <- sketch.basic(colnames(t1)[1], b1, b2, tsyear, runnames[1], runnames[2])
      create.DT.basic(t1, sketch)
    })

  })
}