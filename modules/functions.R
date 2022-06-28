get_runnames <- function(runs) {
  # splices and extracts the run directory name from a path
  map(runs, ~str_split(.x, '/')) %>% 
    flatten() %>% 
    map(., ~pluck(.x, length(.x))) %>% 
    unlist()
}