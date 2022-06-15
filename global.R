library(shiny)
library(shinyFiles)
library(fs)
library(shinythemes)

rund <- 'L:/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs' # When running locally
# rund <- "/media/aws-prod-file01modeldata/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs" # Shiny Server

source('modules/run-choice.R')
