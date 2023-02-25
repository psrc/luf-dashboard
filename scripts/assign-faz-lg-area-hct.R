# This script will clean/categorize cross-walks created from ArcGIS for time series page.
# Control HCTs are organized into FAZ large areas for the UI selection

library(data.table)
library(foreign)

dir <- 'J:\\Staff\\Christy\\luf-dashboard'

ch <- read.dbf(file.path(dir, 'ctrl18_fazlg.dbf'))

# Control HCT
old.ch <- read.csv('https://raw.githubusercontent.com/psrc/luv/master/QC/data/cities_faz_lgarea.csv', header = TRUE)
setDT(old.ch)
old.x <- unique(old.ch[,.(large_area, lgarea_group)])
old.y <- old.x[!(large_area %in% c('Eastside King', 'Pierce Other', 'SW Snohomish')) & large_area != "",]

# Keep: control_id, control_na (name), county_id, county, large_area
setDT(ch)
ch <- ch[, .(control_id, control_na, county_id, county, large_area)]
ch[, .(large_area = .N), by = 'large_area']
# create large_area_group
ch[!(large_area %in% c('Eastside King', 'Pierce Other', 'SW Snohomish')), large_area_group := large_area]

# Eastside King
ek1 <- c('Bothell', 'Clyde Hill', 'Hunts Point', 'Kenmore', 'Kirkland', 'Medina', 'Redmond', 	
         'Redmond PAA', 'Woodinville', 'Yarrow Point', 'King no growth UGA Bear Creek UPD', 'King Rural')
ek2 <- c('Beaux Arts', 'Bellevue', 'Bellevue PAA', 'Issaquah', 'Issaquah PAA', 'Mercer Island', 'Newcastle', 'Newcastle PAA', 'Sammamish', 'Sammamish PAA', 'King UGA')
ch[control_na %in% ek1 & is.na(large_area_group) & large_area == 'Eastside King', large_area_group := 'Eastside King (1)']
ch[control_na %in% ek2 & is.na(large_area_group) & large_area == 'Eastside King', large_area_group := 'Eastside King (2)']

# Pierce Other
po1 <- c('Auburn', 'Bonney Lake', 'Buckley', 'Edgewood', 'Milton', 'Pacific', 'Sumner', 'Pierce UGA')
po2 <- c('Carbonado', 'Eatonville', 'Orting', 'Puyallup', 'Roy', 'South Prairie', 'Wilkeson', 'Pierce Rural', 'Pierce Rural Ag & Resource', 'Mid-County, Parkland-Spanaway-Midland, South Hill')
ch[control_na %in% po1 & is.na(large_area_group) & large_area == 'Pierce Other', large_area_group := 'Pierce Other (1)']
ch[control_na %in% po2 & is.na(large_area_group) & large_area == 'Pierce Other', large_area_group := 'Pierce Other (2)']

# SW Snohomish
sws1 <- c('Edmonds', 'Edmonds MUGA', 'Lynnwood', 'Lynnwood MUGA', 'Meadowdale Gap MUGA', 'Mountlake Terrace', 'Mountlake Terrace MUGA', 'Mukilteo', 'Mukilteo MUGA','Woodway', 'Woodway MUGA', 'Larch Way Overlap')
sws2 <- c('Bothell', 'Bothell MUGA', 'Brier', 'Brier MUGA', 'Everett MUGA', 'Lake Stickney Gap', 'Larch Way Overlap MUGA', 'Maltby UGA', 'Mill Creek', 'Mill Creek MUGA', 'Silver Firs Gap', 'Silver Firs Gap MUGA')
ch[control_na %in% sws1 & is.na(large_area_group) & large_area == 'SW Snohomish', large_area_group := 'SW Snohomish (1)']
ch[control_na %in% sws2 & is.na(large_area_group) & large_area == 'SW Snohomish', large_area_group := 'SW Snohomish (2)']

# update with non-ek, po, sws groupings
ch <- merge(ch, old.y, by = 'large_area', all.x = TRUE)
ch[is.na(lgarea_group), lgarea_group := large_area_group]
setcolorder(ch, c('control_id', 'control_na', 'county_id', 'county', 'large_area', 'large_area_group', 'lgarea_group'))
fwrite(ch, file.path(here::here('data'), 'control_hct.csv'))

