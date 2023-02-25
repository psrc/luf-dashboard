# This script will clean/categorize cross-walks created from ArcGIS for time series page.
# Cities are organized into FAZ large areas for the UI selection

library(data.table)
library(foreign)

dir <- 'J:\\Staff\\Christy\\luf-dashboard'

ct <- read.dbf(file.path(dir, 'cities18_fazlg.dbf'))
setDT(ct)

keep.cols <- c('county_id', 'county', 'city_id', 'city_name', 'large_area')
ct <- ct[, ..keep.cols]

# old cities
old.c <- read.csv('https://raw.githubusercontent.com/psrc/luv/master/QC/data/cities_faz_lgarea.csv', header = TRUE)
setDT(old.c)
old.c <- old.c[, .(county_id, city_name, large_area, lgarea_group)]
old.x <- unique(old.c[large_area != "" & !(large_area %in% c('Eastside King', 'Pierce Other', 'SW Snohomish')), .(large_area, lag = lgarea_group)])

ct.join <- merge(ct, old.c, by = c('city_name', 'county_id', 'large_area'), all.x = TRUE)
ct.join2 <- merge(ct.join, old.x, by = 'large_area', all.x = TRUE)
ct.join2[is.na(lgarea_group) & !is.na(lag), lgarea_group := lag]
ct.join2 <- ct.join2[city_id != 95, ] # GIS error
ct.join2[city_name %in% c('Pierce Rural', 'Uninc Urban Pierce'), lgarea_group := 'Pierce Other (2)']
ct.join2[city_name == 'Uninc Urban Snoh', lgarea_group := 'SW Snohomish (2)']
ct.join2[, lag := NULL]

fwrite(ct.join2, file.path(here::here('data'), 'cities18.csv'))
