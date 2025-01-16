create.tsTable <- function(table, idname, runs, tsyear, baseyear){ #idname aka 'County' or 'Name'
  # Prepares generic topsheet table for main indicators (households, population, employment)
  target.year.col <- paste0("yr", tsyear)
  
  table_m <- melt.data.table(table, 
                             id.vars = c("County", "indicator", "run"), 
                             measure.vars = setdiff(colnames(table), c("County", "indicator", "run")),
                             variable.name = "variable", 
                             value.name = "value")
  
  # isolate respective baseyear values by run
  table_mg <- merge(table_m, baseyear, by.x = c("run", "variable"), by.y = c("run", "baseyear"))
  setnames(table_mg, c("value", "variable"), c("base", "base_year"))
  
  # add target (future) year
  cols <- c("County", "indicator", "run", target.year.col)
  table_f <- table[, ..cols]
  tbl <- merge(table_mg, table_f, by = c("County", "indicator", "run"))
  
  tbl[, run := get_trim_runnames(run)]
  
  t1 <- dcast.data.table(tbl, paste(idname, "~ run"), value.var = c("base", target.year.col))
  
  # column names
  base_run1 <- paste0("base_", get_trim_runnames(runs[1]))
  base_run2 <- paste0("base_", get_trim_runnames(runs[2]))
  target_run1 <- paste0(target.year.col, "_", get_trim_runnames(runs[1]))
  target_run2 <- paste0(target.year.col, "_", get_trim_runnames(runs[2]))
  
  baseyear_run1 <- str_extract(unique(table_mg[run %like% runs[1]][, base_year]), "\\d+")
  baseyear_run2 <- str_extract(unique(table_mg[run %like% runs[2]][, base_year]), "\\d+")
  target_year <- str_extract(target.year.col, "\\d+")
  
  setcolorder(t1, c(idname, base_run1, target_run1, base_run2, target_run2))
  
  t1[, Change := (t1[[target_run1]]-t1[[target_run2]])] # change in 2050(target years)
  t1[, Per.Change := round((Change/t1[[target_run2]])*100, 2)] # change/2050 run2
  t1[, r1.baseyr := (t1[[target_run1]]-t1[[base_run1]])] # run1 2050-baseyear (growth)
  t1[, r2.baseyr := (t1[[target_run2]]-t1[[base_run2]])] # run2 2050-baseyear (growth)
  t1[, r1.baseyr.per := round((r1.baseyr/t1[[base_run1]])*100, 2)] # run1/baseyear (growth %)
  t1[, r2.baseyr.per := round((r2.baseyr/t1[[base_run2]])*100, 2)] # run2/baseyear (growth %)
  t1[, r1.avgann := round(((t1[[target_run1]]/t1[[base_run1]])^(1/(as.numeric(target_year)-as.numeric(baseyear_run1)))-1)*100, 2)] # run1 2050/baseyear ^1/2050-baseyear
  t1[, r2.avgann := round(((t1[[target_run2]]/t1[[base_run2]])^(1/(as.numeric(target_year)-as.numeric(baseyear_run2)))-1)*100, 2)] # run2 2050/baseyear ^1/2050-baseyear
  t1[, `:=` (r1dist = round(r1.baseyr/(unlist(t1[like(get(eval(idname)), "Sub-Total"), .(r1.baseyr)])[[1]])*100, 2), # growth/regional growth percent
             r2dist = round(r2.baseyr/(unlist(t1[like(get(eval(idname)), "Sub-Total"), .(r2.baseyr)])[[1]])*100, 2))] # growth/regional growth percent
  t1[, `:=` (distdiff = round(r1dist - r2dist, 2))]
  
  setcolorder(t1, c(idname,
                    base_run1,
                    target_run1,
                    base_run2,
                    target_run2,
                    "Change",
                    "Per.Change",
                    "r1dist",
                    "r2dist",
                    "r1.baseyr",
                    "r1.baseyr.per",
                    "r1.avgann",
                    "r2.baseyr",
                    "r2.baseyr.per",
                    "r2.avgann",
                    "distdiff"
  ))
  
  setnames(t1, c(target_run1, target_run2), str_extract(c(target_run1, target_run2), "(?<=yr).*"))
  
  t1[, c(2:6, 10, 13) := lapply(.SD, FUN=function(x) prettyNum(x, big.mark=",")), .SDcols = c(2:6, 10, 13)]
  
  return(t1)
}

sketch.basic <- function(grpcol, baseyear1, baseyear2, year2, run1, run2){
  # Create basic table container
  
  htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 3, grpcol),
        th(class = 'dt-center', bgcolor='AliceBlue', colspan = 1, baseyear1),
        th(class = 'dt-center', bgcolor='AliceBlue', colspan = 1, baseyear2),
        th(class = 'dt-center', colspan = 10, year2)
      ),
      tr(
        th(class = 'dt-center', style="font-size:12px; font-style:italic; font-weight:normal;", bgcolor='AliceBlue', 'B1'),
        th(class = 'dt-center', style="font-size:12px; font-style:italic; font-weight:normal;", bgcolor='AliceBlue', 'B2'),
        lapply(list('F1'), function(x) th(class = 'dt-center', style="font-size:12px; font-style:italic; font-weight:normal;", x)),
        lapply(list('U = F1-B1', 'V = U/B1', ''), function(x) th(class = 'dt-center', style="font-size:12px; font-style:italic; font-weight:normal;", bgcolor='LightGoldenRodYellow', x)),
        lapply(list('F2'), function(x) th(class = 'dt-center', style="font-size:12px; font-style:italic; font-weight:normal;", x)),
        lapply(list('W = F2-B2', 'X = W/B2', ''), function(x) th(class = 'dt-center', style="font-size:12px; font-style:italic; font-weight:normal;", bgcolor='LightGoldenRodYellow', x)),
        lapply(list('Y = F1-F2', 'Z = Y/F2'), function(x) th(class = 'dt-center', style="font-size:12px; font-style:italic; font-weight:normal;", x))

      ),
      tr(
        th(style="font-size:14px;", bgcolor='AliceBlue', run1),
        th(style="font-size:14px;", bgcolor='AliceBlue', run2),
        th(style="font-size:14px;", run1),
        lapply(c('Growth', '% Growth', '% AvgAnn'), function(x) th(style="font-size:14px;",bgcolor='LightGoldenRodYellow', x)),
        th(style="font-size:14px;",run2),
        lapply(c('Growth', '% Growth', '% AvgAnn'), function(x) th(style="font-size:14px;",bgcolor='LightGoldenRodYellow', x)),
        lapply(c('Change', '% Change'), function(x) th(style="font-size:14px;",x))

      )
    ) # end thead
  )) # end withTags/table
  
}

create.DT.basic <- function(table, acontainer){
  # Create a basic DT
  
  DT::datatable(table,
                extensions = 'Buttons',
                class = 'cell-border stripe',
                options = list(columnDefs = list(list(className = 'dt-center', targets = 1:12), 
                                                 list(width = '20%', targets = 0)),
                               dom = 'Bfrtip',
                               buttons = list('copy',
                                              list(extend = 'excel',
                                                   buttons = 'excel',
                                                   filename = 'LUFDashboard')),
                               #autoWidth = TRUE,
                               paging = FALSE, 
                               searching = FALSE 
                ),
                container = acontainer, 
                rownames = FALSE
  ) %>% 
    formatStyle(colnames(table)[(ncol(table)-1):(ncol(table))],
                color = styleInterval(c(0), c('red', 'black'))) %>%
    formatStyle(colnames(table)[2:3],
                backgroundColor = 'AliceBlue') %>%
    formatStyle(colnames(table)[5:7],
                backgroundColor = 'LightGoldenRodYellow') %>%
    formatStyle(colnames(table)[9:11],
                backgroundColor = 'LightGoldenRodYellow')
}

calc.cols.tsTable <- function(table, tsyear, runs, baseyear){
  # Prepares generic series of calculation on topsheet
  
  cols <- colnames(table)[2:ncol(table)]
  setnames(table, cols, str_extract(cols, ".*(?=\\.)"))
  
  runnames <- get_trim_runnames(runs)
  # column names
  base_run1 <- paste0(str_extract(baseyear[run == runs[1], baseyear][[1]], "\\d+"),"_", runnames[1])
  base_run2 <- paste0(str_extract(baseyear[run == runs[2], baseyear][[1]], "\\d+"),"_", runnames[2])
  target_run1 <- paste0(tsyear, "_", get_trim_runnames(runs[1]))
  target_run2 <- paste0(tsyear, "_", get_trim_runnames(runs[2]))
  
  baseyear_run1 <- str_extract(baseyear[run == runs[1], baseyear][[1]], "\\d+")
  baseyear_run2 <- str_extract(baseyear[run == runs[2], baseyear][[1]], "\\d+")

  t1 <- table
  setcolorder(t1, c("Sector", base_run1, base_run2, target_run1, target_run2))
  
  t1[, Change := (t1[[target_run1]]-t1[[target_run2]])] # change in 2050(target years)
  t1[, Per.Change := round((Change/t1[[target_run2]])*100, 2)] # change/2050 run2
  t1[, r1.baseyr := (t1[[target_run1]]-t1[[base_run1]])] # run1 2050-baseyear (growth)
  t1[, r2.baseyr := (t1[[target_run2]]-t1[[base_run2]])] # run2 2050-baseyear (growth)
  t1[, r1.baseyr.per := round((r1.baseyr/t1[[base_run1]])*100, 2)] # run1/baseyear (growth %)
  t1[, r2.baseyr.per := round((r2.baseyr/t1[[base_run2]])*100, 2)] # run2/baseyear (growth %)
  t1[, r1.avgann := round(((t1[[target_run1]]/t1[[base_run1]])^(1/(as.numeric(tsyear)-as.numeric(baseyear_run1)))-1)*100, 2)] # run1 2050/baseyear ^1/2050-baseyear
  t1[, r2.avgann := round(((t1[[target_run2]]/t1[[base_run2]])^(1/(as.numeric(tsyear)-as.numeric(baseyear_run2)))-1)*100, 2)] # run2 2050/baseyear ^1/2050-baseyear
  
  return(t1)
}

create.exp.tsTable <- function(table, runs, tsyear, baseyear){
  # Prepares expanded topsheet table for RGCs & Key Locations
  
  runnames <- get_trim_runnames(runs)

  b1 <- baseyear[run == runs[1], .(baseyear)][[1]] |> str_extract("\\d+")
  b2 <- baseyear[run == runs[2], .(baseyear)][[1]] |> str_extract("\\d+")

  setcolorder(table,
              c("Name",
                paste0(b1, "_", "Population","_", runnames[1]), #b1 run1
                paste0(b2, "_", "Population","_", runnames[2]), #b2 run2
                paste0(tsyear, "_", "Population","_", runnames[1]), #f1 run1
                paste0(tsyear, "_", "Population","_", runnames[2]), #f2 run2
                
                paste0(b1, "_", "Employment","_", runnames[1]),
                paste0(b2, "_", "Employment","_", runnames[2]),
                paste0(tsyear, "_", "Employment","_", runnames[1]),
                paste0(tsyear, "_", "Employment","_", runnames[2])
              ))

  t1 <- table[, Pop.Change := (table[[4]]-table[[5]]) # target year change
  ][, Pop.Per.Change := round((Pop.Change/table[[5]])*100, 2)
  ][, Emp.Change := (table[[8]]-table[[9]])
  ][, Emp.Per.Change := round((Emp.Change/table[[9]])*100, 2)]
  
  setcolorder(t1,
              c("Name",
                paste0(b1, "_", "Population","_", runnames[1]),
                paste0(b2, "_", "Population","_", runnames[2]),
                paste0(tsyear, "_", "Population","_", runnames[1]),
                paste0(tsyear, "_", "Population","_", runnames[2]),
                "Pop.Change",
                "Pop.Per.Change",
                paste0(b1, "_", "Employment","_", runnames[1]),
                paste0(b2, "_", "Employment","_", runnames[2]),
                paste0(tsyear, "_", "Employment","_", runnames[1]),
                paste0(tsyear, "_", "Employment","_", runnames[2]),
                "Emp.Change",
                "Emp.Per.Change"))
  
  setnames(t1, colnames(t1), c("Name",
                               paste0(b1, "_", "Pop","_", runnames[1]),
                               paste0(b2, "_", "Pop","_", runnames[2]),
                               paste0(tsyear, "_", "Pop","_", runnames[1]),
                               paste0(tsyear, "_", "Pop","_", runnames[2]),
                               "Pop.Change",
                               "Pop.Per.Change",
                               paste0(b1, "_", "Emp","_", runnames[1]),
                               paste0(b2, "_", "Emp","_", runnames[2]),
                               paste0(tsyear, "_", "Emp","_", runnames[1]),
                               paste0(tsyear, "_", "Emp","_", runnames[2]),
                               "Emp.Change",
                               "Emp.Per.Change"))
  t1[, c(2:6,8:12) := lapply(.SD, FUN=function(x) prettyNum(x, big.mark=",")), .SDcols = c(2:6,8:12)]
}


create.DT.expanded <- function(table, acontainer){
  # Create an expanded DT
  
  DT::datatable(table,
                extensions = 'Buttons',
                class = 'cell-border stripe',
                options = list(columnDefs = list(list(className = 'dt-center', targets = 1:12),
                                                 list(width = '20%', targets = 0)),
                               dom = 'Bfrtip',
                               buttons = list('copy',
                                              list(extend = 'excel',
                                                   buttons = 'excel',
                                                   filename = 'LUFDashboard')),
                               paging = FALSE, 
                               searching = FALSE 
                ),
                container = acontainer, 
                rownames = FALSE
  ) %>% 
    formatStyle(colnames(table)[c(6:7, (ncol(table)-1):(ncol(table)))],
                color = styleInterval(c(0), c('red', 'black'))) %>%
    formatStyle(colnames(table)[c(2,3,8,9)],
                backgroundColor = 'AliceBlue')
}

sketch.expanded <- function(grpcol, baseyear1, baseyear2, year2, run1, run2){
  # Create expanded table container
  htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 4, grpcol),
        th(colspan = 6, 'Population'),
        th(colspan = 6, 'Employment')
      ),
      tr(
        rep(list(th(class = 'dt-center', bgcolor='AliceBlue', colspan = 1, baseyear1), th(class = 'dt-center', bgcolor='AliceBlue', colspan = 1, baseyear2), th(class = 'dt-center', colspan = 4, year2)), 2)
      ),
      tr(
        rep(
          list(th(class = 'dt-center', style="font-size:12px; font-style:italic; font-weight:normal;", bgcolor='AliceBlue', 'B1'),
               th(class = 'dt-center', style="font-size:12px; font-style:italic; font-weight:normal;", bgcolor='AliceBlue', 'B2'),
               lapply(list('F1', 'F2', 'U = F1-F2', 'U/F2'), function(x) th(class = 'dt-center', style="font-size:12px; font-style:italic; font-weight:normal;", x))),
          2)
      ),
      tr(
        rep(list(th(bgcolor='AliceBlue', run1), th(bgcolor='AliceBlue', run2), lapply(c(run1, run2, 'Change', '% Change'), th)), 2)
      )
    ) # end thead
  )) # end withTags/table
}
