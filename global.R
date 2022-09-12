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

# rund <- 'L:/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs' # When running locally
rund <- "/media/aws-prod-file01modeldata/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs" # Shiny Server

attribute <- c("population", "households","employment", "residential_units")
geography <- c( "zone", "faz", "city")
years <- seq(2014, 2050)
luv.years <- c(2014, 2015, 2020, 2025, 2030, 2035, 2040)

# lookups
faz.lookup <- fread(file.path('data', "faz_names.txt"))
zone.lookup <- fread(file.path('data', "zones.txt"))
splaces.lookup <- fread(file.path('data', 'SpecialPlaces.csv'))
# city.lookup <- read.table(file.path(dsn, "cities.csv"), header =TRUE, sep = ",")

# spatial features
arc.root <- 'https://services6.arcgis.com/GWxg6t7KXELn1thE/arcgis/rest/services'
zone.link <- 'Transportation_Analysis_Zones_2010/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson'
faz.link <- 'FAZ_2010/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson'

zone.shape <- st_read(file.path(arc.root, zone.link)) %>%
  mutate(name_id = taz)
faz.shape <- st_read(file.path(arc.root, faz.link)) %>% 
  mutate(name_id = faz10)

source('modules/functions.R')
source('modules/functions-leaflet.R')
source('modules/functions-top-sheet.R')
source('modules/run-choice.R')
source('modules/run-comp-widgets.R')
source('modules/run-comp-plot-map-table.R')
source('modules/top-sheet-widgets.R')
source('modules/top-sheet-table.R')
source('modules/top-sheet-all-tables.R')
source('modules/one-run-ct-mismatch.R')
source('modules/one-run-widgets.R')
source('modules/one-run-special-places-table.R')
source('modules/one-run-special-places-all-tables.R')
