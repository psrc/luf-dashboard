library(shiny)
library(shinyFiles)
library(fs)
library(shinythemes)
library(plotly)
library(leaflet)
library(DT)
library(data.table)
library(tidyverse)

rund <- 'L:/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs' # When running locally
# rund <- "/media/aws-prod-file01modeldata/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs" # Shiny Server

attribute <- c("population", "households","employment", "residential_units")
geography <- c( "zone", "faz", "city")
years <- seq(2014, 2050)
luv.years <- c(2014, 2015, 2020, 2025, 2030, 2035, 2040)

source('modules/functions.R')
source('modules/run-choice.R')
source('modules/run-comparison.R')
