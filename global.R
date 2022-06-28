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

source('modules/functions.R')
source('modules/run-choice.R')
source('modules/run-comparison.R')
