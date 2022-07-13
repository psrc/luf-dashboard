get_runnames <- function(runs) {
  # splices and extracts the run directory name from a path
  
  map(runs, ~str_split(.x, '/')) %>% 
    flatten() %>% 
    map(., ~pluck(.x, length(.x))) %>% 
    unlist()
}

get_trim_runnames <- function(runnames) {
  # splices runname at the period for pretty labeling purposes
  
  map(runnames, ~str_split(.x, '\\.')[[1]][1]) %>% flatten() %>% unlist()
}



# plots -------------------------------------------------------------------


scatterplot <- function(table, sourcename, xcolumn, ycolumn, xtitle, ytitle) {
  # Creates a Plotly scatterplot. Requires reactive table, string source name and string x&y axis titles.
  
  data <- table
  key <- data$name_id # uniquely identify geo for Plotly
  p <- plot_ly(data,
               type = 'scatter',
               x = ~xcolumn,
               y = ~ycolumn,
               name = "",
               source = sourcename,
               text = ~paste0("ID: ", name_id,
                              "<br>Name: ", Name),
               key = key, # will appear in 'eventdata'
               mode = 'markers',
               showlegend = F)%>%
    add_trace(x=c(0,~max(xcolumn)),
              y=c(0,~max(xcolumn)),
              color= I("grey"),
              opacity = .6,
              mode = "lines",
              showlegend = F)%>%
    layout(font = list(family="Segoe UI", size = 13.5),
           title = " ",
           xaxis = list(title = xtitle),
           yaxis = list(title = ytitle),
           margin = list(l=100, b=100, t=90, r=100)
    )
  p
}


# DT ----------------------------------------------------------------------


# # function creating tables on Run Comparison and Growth tabs
# create.DT.generic <- function(table) {
#   datatable(table,
#             extensions = 'Buttons',
#             class = 'cell-border stripe',
#             options = list(dom = 'Bfrtip',
#                            buttons = list('copy',
#                                           list(extend = 'excel',
#                                                buttons = 'excel',
#                                                filename = 'LUVQCDashboard')),
#                            initComplete = JS(
#                              "function(settings, json) {",
#                              "$(this.api().table().header()).css({'font-size': '15px'});",
#                              "}"),
#                            paging = TRUE,
#                            pageLength = 150,
#                            searching = TRUE,
#                            scrollY = '450px'
#             )
#   ) %>%
#     formatStyle(colnames(table)[1:ncol(table)],
#                 `font-size` = '13px')
# }