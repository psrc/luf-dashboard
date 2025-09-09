library(shiny)
library(shinyFiles)
library(fs)
library(bslib)
library(sf)
library(plotly)
library(leaflet)
library(DT)
library(data.table)
library(tidyverse)
library(psrcelmer)

rund <- 'N:/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs' # When running locally (Christy's machine)
# rund <- '~/N/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs' # When running locally
# rund <- "/media/aws-prod-file01modeldata2/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs" # Shiny Server

attribute <- c("population", "households","employment", "residential_units")
geography <- c( "zone", "faz", "city")
years <- seq(2014, 2050)
limited.years <- sort(setdiff(c(2014, seq(2015, 2050, by = 5), 2023, 2044), 2045))

# lookups
faz.lookup <- fread(file.path('data', "faz_names.txt"))
zone.lookup <- fread(file.path('data', "zones.txt"))
splaces.lookup <- fread(file.path('data', 'SpecialPlaces.csv'))
rgc.lookup <- fread(file.path('data', "growth_centers.csv")) %>% subset(growth_center_id >= 500)
ctrlhct.lookup <- fread(file.path('data','control_hcts.csv'))
ctrl.lookup <- fread(file.path('data','controls.csv')) # varname should be "controls" formerly ctrlhct
city.lookup <- fread(file.path('data', "cities18.csv"))

# spatial features
arc.root <- 'https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services'
zone.link <- 'Transportation_Analysis_Zones_2010/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson'
faz.link <- 'FAZ_2010/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson'
centers.link <- 'Regional_Growth_Centers/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson'

zone.shape <- st_read(file.path(arc.root, zone.link)) %>%
  mutate(name_id = taz)
faz.shape <- st_read(file.path(arc.root, faz.link)) %>% 
  mutate(name_id = faz10)
centers.shape <- st_read(file.path(arc.root, centers.link)) |> 
  mutate(name_id = name)

# names of layers in ElmerGeo
cities.shape <- 'cities18_dashboard'
control.shape <- 'control18_dashboard'
subreg.shape <- 'subregs18_dashboard'
target.shape <- 'target18_dashboard'

# run all files in the modules sub-directory
module_files <- list.files('modules', full.names = TRUE)
sapply(module_files, source)