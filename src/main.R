
rm(list = ls()) # clear workspace 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # source path 

source("setup.R") # load configurations and files 
source("config_2024_GL_Sleeper.R") # change config based on league settings 
source("functions.R") # load functions for Main 

main <-function(){
  
  validate_inputs(config) # validate inputs 
  
  main_projections <- scrape_ffanalytics(config) # scrape main proj
  main_projections <- manual_ChangesToPlayers(main_projections) 
  
  # NOTE: may need to drop 
  # bchen_data <- scrape_BorisChen(config) # scrape Boris Chen
  
  
  fantasypros_data <- scrape_FantasyPros(config) 
  
  # manual_player_notes<- manual_PlayerNotes(config) # enter manual notes 
  

  # matched_projections <- merge_projections(main_projections, bchen_data, c("first_name","last_name"))# Load and process Fantasy Pros data
  matched_projections <- merge_projections(main_projections, fantasypros_data, c("first_name","last_name"))# Load and merge personal notes
  # matched_projections <- merge_projections(matched_projections, manual_player_notes, c("id")) 
  
  
  rev_tiers <- matched_projections %>% 
    filter(!(pos %in% c('K','DEF'))) %>% 
    # select(first_name, last_name, adp, tier,bc_tier,fp_tier,points) %>% 
    rowwise() %>%
    mutate(master_tier = round(median(c_across(c('tier', 'fp_tier')), na.rm=TRUE))) %>% 
    arrange(master_tier,adp) %>% select(id, master_tier)
  
  
  matched_projections <- merge(matched_projections, rev_tiers, by = c("id"), all.x=TRUE)
  
  # expected round aka 1 for 14 rows then 2 for 14 rows etc 
  matched_projections <- matched_projections %>% rowwise() %>% mutate(league_round = ceiling(overall_ecr/config$team_count)) 
  
  model_output <- matched_projections %>% mutate(picked = "") %>% 
    select(picked,first_name,last_name,team,pos,
           master_tier,overall_ecr, adp, league_round, #expected_slot,
           points, sd_pts, ceiling, uncertainty, age, sos
           #,JW_Notes
           ) %>% arrange(master_tier,overall_ecr,adp)
  

  

  generate_output(model_output, config)

  } # generate outputs 

main()

