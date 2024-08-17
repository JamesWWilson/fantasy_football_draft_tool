
rm(list = ls()) # clear workspace 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # source path 

source("setup.R") # load configurations and files 
source("config.R") # change config based on league settings 
source("functions.R") # load functions for Main 

main <-function(){
  
  validate_inputs(config) # validate inputs 
  
  main_projections <- scrape_ffanalytics(config) # scrape main proj
  main_projections <- manual_ChangesToPlayers(main_projections) 
  
  bchen_data <- scrape_BorisChen(config) # scrape additional sources 
  fantasypros_data <- scrape_FantasyPros(config) 
  manual_player_notes<- manual_PlayerNotes(config)
  
  matched_projections <- merge_all_projections(main_projections, bchen_data, fantasypros_data, manual_player_notes) # aggregate 
    
  

  

  generate_output(matched_projections, config)} # generate outputs 

main()

