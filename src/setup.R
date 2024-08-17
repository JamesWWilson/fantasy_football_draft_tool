
# Notes: Start of season make sure to re-build package for updates on scrapers 
# require("remotes")
# remotes::install_github("FantasyFootballAnalytics/ffanalytics") 

# Load necessary libraries
require("ffanalytics")
require("googlesheets4")
require("dplyr")
require("readr")
require("tidyr")
require("openxlsx")
require("ggplot2")
require("stringr")# Load additional scripts
source("set_points.r")# Load configurations and functions
source("config.R")
source("functions.R")# Main function to run the entire process