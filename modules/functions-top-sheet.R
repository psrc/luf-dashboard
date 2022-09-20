create.tsTable <- function(table, idname, runs, tsyear, baseyear){ #idname aka 'County' or 'Name'
  # Prepares generic topsheet table for main indicators (households, population, employment)
  
  runs <- get_runnames(runs) %>% get_trim_runnames()
  
  sel.yrs.col <- c(unique(baseyear$baseyear), paste0("yr", tsyear))
  sel.yr.fl <- str_extract(sel.yrs.col, "\\d+")
  table[, run := get_trim_runnames(run)]
  
  t1 <- dcast.data.table(table, paste(idname, "~ run"), value.var = sel.yrs.col)
  setcolorder(t1, c(idname, paste0(sel.yrs.col[1],"_",runs[1]), paste0(sel.yrs.col[2],"_",runs[1]), paste0(sel.yrs.col[2],"_",runs[2]), paste0(sel.yrs.col[1],"_",runs[2])))
  t1[, ncol(t1) := NULL]
  t1[, Change := (t1[[ncol(t1)-1]]-t1[[ncol(t1)]])
  ][, Per.Change := round((Change/t1[[4]])*100, 2)
  ][, Per.Growth := round(Change/(t1[[4]]-t1[[2]])*100, 2)
  ][, r1.baseyr := (t1[[3]]-t1[[2]])
  ][, r2.baseyr := (t1[[4]]-t1[[2]])
  ][, r1.baseyr.per := round((r1.baseyr/t1[[2]])*100, 2)
  ][, r2.baseyr.per := round((r2.baseyr/t1[[2]])*100, 2)
  ][, r1.avgann := round(((t1[[3]]/t1[[2]])^(1/(as.numeric(sel.yr.fl[2])-as.numeric(sel.yr.fl[1])))-1)*100, 2)
  ][, r2.avgann := round(((t1[[4]]/t1[[2]])^(1/(as.numeric(sel.yr.fl[2])-as.numeric(sel.yr.fl[1])))-1)*100, 2)]
  setnames(t1, colnames(t1), c(idname,
                               paste0(sel.yr.fl[1], "_", runs[1]),
                               paste0(sel.yr.fl[2], "_", runs[1]),
                               paste0(sel.yr.fl[2], "_", runs[2]),
                               "Change",
                               "Per.Change",
                               "Per.Growth",
                               "r1.baseyr",
                               "r2.baseyr",
                               "r1.baseyr.per",
                               "r2.baseyr.per",
                               "r1.avgann",
                               "r2.avgann"))
  t1[, `:=` (r1dist = round(r1.baseyr/(unlist(t1[like(get(eval(idname)), "Sub-Total"), .(r1.baseyr)])[[1]])*100, 2), 
             r2dist = round(r2.baseyr/(unlist(t1[like(get(eval(idname)), "Sub-Total"), .(r2.baseyr)])[[1]])*100, 2))
  ][, `:=` (distdiff = round(r1dist - r2dist, 2))]
  setcolorder(t1, c(idname,
                    paste0(sel.yr.fl[1], "_", runs[1]),
                    paste0(sel.yr.fl[2], "_", runs[1]),
                    paste0(sel.yr.fl[2], "_", runs[2]),
                    "Change",
                    "Per.Change",
                    "r1dist",
                    "r2dist",
                    "Per.Growth",
                    "r1.baseyr",
                    "r1.baseyr.per",
                    "r1.avgann",
                    "r2.baseyr",
                    "r2.baseyr.per",
                    "r2.avgann",
                    "distdiff"
  ))
  
  t1[, c(2:4, 10, 13) := lapply(.SD, FUN=function(x) prettyNum(x, big.mark=",")), .SDcols = c(2:4, 10, 13)]
}

sketch.basic <- function(grpcol, year1, year2, run1, run2){
  # Create basic table container
  
  htmltools::withTags(table(
    class = 'display', 
    thead(
      tr(
        th(rowspan = 3, grpcol),
        th(class = 'dt-center', bgcolor='AliceBlue', colspan = 1, year1),
        th(class = 'dt-center', colspan = 10, year2)
      ),
      tr(
        th(class = 'dt-center', style="font-size:12px; font-style:italic; font-weight:normal;", bgcolor='AliceBlue', 'A'),
        lapply(list('B'), function(x) th(class = 'dt-center', style="font-size:12px; font-style:italic; font-weight:normal;", x)),
        lapply(list('C = B-A', 'D = C/A', ''), function(x) th(class = 'dt-center', style="font-size:12px; font-style:italic; font-weight:normal;", bgcolor='LightGoldenRodYellow', x)),
        lapply(list('E'), function(x) th(class = 'dt-center', style="font-size:12px; font-style:italic; font-weight:normal;", x)),
        lapply(list('F = E-A', 'G = F/A', ''), function(x) th(class = 'dt-center', style="font-size:12px; font-style:italic; font-weight:normal;", bgcolor='LightGoldenRodYellow', x)),
        lapply(list('H = B-E', 'I = H/E'), function(x) th(class = 'dt-center', style="font-size:12px; font-style:italic; font-weight:normal;", x))
      ),
      tr(
        th(style="font-size:14px;", bgcolor='AliceBlue', run1),
        th(style="font-size:14px;",run1),
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
                options = list(columnDefs = list(list(className = 'dt-center', targets = 1:11), 
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
    formatStyle(colnames(table)[2],
                backgroundColor = 'AliceBlue') %>%
    formatStyle(colnames(table)[4:6],
                backgroundColor = 'LightGoldenRodYellow') %>%
    formatStyle(colnames(table)[8:10],
                backgroundColor = 'LightGoldenRodYellow')
}

calc.cols.tsTable <- function(table, select.years){
  # Prepares generic series of calculation on Modellers topsheet
  
  table[, Change := (table[[ncol(table)-1]]-table[[ncol(table)]])
  ][, Per.Change := round((Change/table[[4]])*100, 2)
    # ][, Per.Growth := round(Change/(table[[4]]-table[[2]])*100, 2)
  ][, r1.baseyr := (table[[3]]-table[[2]])
  ][, r1.baseyr.per := round((r1.baseyr/table[[2]])*100, 2)
  ][, r1.avgann := round(((table[[3]]/table[[2]])^(1/(as.numeric(select.years[2])-as.numeric(select.years[1])))-1)*100, 2)
  ][, r2.baseyr := (table[[4]]-table[[2]])
  ][, r2.baseyr.per := round((r2.baseyr/table[[2]])*100, 2)
  ][, r2.avgann := round(((table[[4]]/table[[2]])^(1/(as.numeric(select.years[2])-as.numeric(select.years[1])))-1)*100, 2)]
  return(table)
}

create.exp.tsTable <- function(table, runs, tsyear, baseyear){
  # Prepares expanded topsheet table for RGCs & Key Locations
  
  runs <- get_runnames(runs) %>% get_trim_runnames()
  
  sel.yrs.col <- c(unique(baseyear$baseyear), paste0("yr", tsyear))
  sel.yr.fl <- str_extract(sel.yrs.col, "\\d+")

  setcolorder(table, 
              c("Name",
                paste0(sel.yrs.col[1], "_", "Population","_", runs[1]),
                paste0(sel.yrs.col[2], "_", "Population","_", runs[1]),
                paste0(sel.yrs.col[2], "_", "Population","_", runs[2]),
                paste0(sel.yrs.col[1], "_", "Employment","_", runs[1]),
                paste0(sel.yrs.col[2], "_", "Employment","_", runs[1]),
                paste0(sel.yrs.col[2], "_", "Employment","_", runs[2]),
                paste0(sel.yrs.col[1], "_", "Population","_", runs[2]),
                paste0(sel.yrs.col[1], "_", "Employment","_", runs[2])))
  table[, (ncol(table)-1):ncol(table) := NULL] 
  
  t1 <- table[, Pop.Change := (table[[3]]-table[[4]])
  ][, Pop.Per.Change := round((Pop.Change/table[[4]])*100, 2)
  ][, Emp.Change := (table[[6]]-table[[7]])
  ][, Emp.Per.Change := round((Emp.Change/table[[7]])*100, 2)]
  setcolorder(t1,
              c("Name",
                paste0(sel.yrs.col[1], "_", "Population","_", runs[1]),
                paste0(sel.yrs.col[2], "_", "Population","_", runs[1]),
                paste0(sel.yrs.col[2], "_", "Population","_", runs[2]),
                "Pop.Change",
                "Pop.Per.Change",
                paste0(sel.yrs.col[1], "_", "Employment","_", runs[1]),
                paste0(sel.yrs.col[2], "_", "Employment","_", runs[1]),
                paste0(sel.yrs.col[2], "_", "Employment","_", runs[2]),
                "Emp.Change",
                "Emp.Per.Change"))
  setnames(t1, colnames(t1), c("Name",
                               paste0(sel.yr.fl[1], "_", "Pop","_", runs[1]),
                               paste0(sel.yr.fl[2], "_", "Pop","_", runs[1]),
                               paste0(sel.yr.fl[2], "_", "Pop","_", runs[2]),
                               "Pop.Change",
                               "Pop.Per.Change",
                               paste0(sel.yr.fl[1], "_", "Emp","_", runs[1]),
                               paste0(sel.yr.fl[2], "_", "Emp","_", runs[1]),
                               paste0(sel.yr.fl[2], "_", "Emp","_", runs[2]),
                               "Emp.Change",
                               "Emp.Per.Change"))
  t1[, c(2:4,7:9) := lapply(.SD, FUN=function(x) prettyNum(x, big.mark=",")), .SDcols = c(2:4,7:9)]
}


create.DT.expanded <- function(table, acontainer){
  # Create an expanded DT
  
  DT::datatable(table,
                extensions = 'Buttons',
                class = 'cell-border stripe',
                options = list(columnDefs = list(list(className = 'dt-center', targets = 1:10),
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
    formatStyle(colnames(table)[c(5:6, (ncol(table)-1):(ncol(table)))],
                color = styleInterval(c(0), c('red', 'black'))) %>%
    formatStyle(colnames(table)[c(2,7)],
                backgroundColor = 'AliceBlue')
}

# Create expanded table container
sketch.expanded <- function(grpcol, year1, year2, run1, run2){
  htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 4, grpcol),
        th(colspan = 5, 'Population'),
        th(colspan = 5, 'Employment')
      ),
      tr(
        rep(list(th(bgcolor='AliceBlue', colspan = 1, year1), th(colspan = 4, year2)), 2)
      ),
      tr(
        rep(
          list(th(style="font-size:12px; font-style:italic; font-weight:normal;", bgcolor='AliceBlue', 'A'),
               lapply(list('B', 'C', 'D = B-C', 'D/C'), function(x) th(style="font-size:12px; font-style:italic; font-weight:normal;", x))),
          2)
      ),
      tr(
        rep(list(th(bgcolor='AliceBlue', run1), lapply(c(run1, run2, 'Change', '% Change'), th)), 2)
      )
    ) # end thead
  )) # end withTags/table
}
