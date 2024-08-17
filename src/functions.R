# functions.R

# 1.  ----------- Configuration Validations  ----------- 
# Ensure user has uploaded correct details to Config file 
validate_inputs <- function(config){
  
  if(!config$scoring_summary %in% c("Standard","Half-PPR","PPR")){
    stop("Invalid scoring summary provided.")
  }
  
  if(!(config$team_count > 0)){
    stop("Team count must be a positive integer.")}
}


# 2.  ----------- Helper Functions  -----------

# Function to clean names in the data frame
clean_names <-function(df, first_name_col, last_name_col){
  
  df[[first_name_col]]<- sub(".","", df[[first_name_col]], fixed =TRUE)
  df[[first_name_col]]<- sub(".","", df[[first_name_col]], fixed =TRUE)
  
  df[[last_name_col]]<- sub(".","", df[[last_name_col]], fixed =TRUE)
  df[[last_name_col]]<- sub(".","", df[[last_name_col]], fixed =TRUE)
  
  df[[last_name_col]]<- sub(" .*","", df[[last_name_col]]) 
  
  return(df)
}

# Function to merge projections
merge_projections <-function(main_df, additional_df, join_columns){
  merge(main_df, additional_df, by = join_columns, all.x =TRUE)}


# 3.  ----------- Main Scrape Functions -----------

# Fantasy Football Rankings Scraped with ffanalytics package
scrape_ffanalytics <-function(config){
  
  main_roster_scrape <- scrape_data(pos = config$positions, season = config$season_choice, week = 0)
  main_projections <- projections_table(main_roster_scrape, scoring_rules = config$scoring_rules) # change scoring based on league
    
  # main_projections <- apply_scoring_rules(
  #   main_roster_scrape, 
  #   config$scoring_rules[[paste0(tolower(config$league_name),"_standard_points")]])
    
  
  # Add extra calculations
  main_projections <- main_projections %>%
      add_ecr() %>%
      add_adp() %>%
      add_aav() %>%
      add_uncertainty() %>%
      add_player_info() %>%
      filter(avg_type =='weighted') # going with weighted average calculation
  
  main_projections <- clean_names(main_projections,"first_name","last_name")

  return(main_projections)
}

## NOTE - subject to change w season 

manual_ChangesToPlayers <- function(main_projections){
  # MANUAL EDITS TO PLAYER NAMES FOR MERGES (improve)
  main_projections$first_name <- sub(".", "", main_projections$first_name, fixed=TRUE)
  main_projections$first_name <- sub(".", "", main_projections$first_name, fixed=TRUE)
  
  main_projections$last_name <- sub(".", "", main_projections$last_name, fixed=TRUE)
  main_projections$last_name <- sub(".", "", main_projections$last_name, fixed=TRUE)
  
  main_projections$last_name <- sub(" .*", "", main_projections$last_name)
  
  

  return(main_projections)
}




# Scrape Boris Chen data
scrape_BorisChen <-function(config){
  if(config$scoring_summary == "Standard"){
    bchen_data <- read_sheet('https://docs.google.com/spreadsheets/d/1aeCDrRHeqY2oLdrcqfirsl4bjca3pcjUg3RP5fJrtyc/pubhtml#', range = "Standard")
  }else if(config$scoring_summary == "Half-PPR"){
    bchen_data <- read_sheet('https://docs.google.com/spreadsheets/d/1aeCDrRHeqY2oLdrcqfirsl4bjca3pcjUg3RP5fJrtyc/pubhtml#', range = "Half PPR")
  }else if(config$scoring_summary == "PPR"){
    bchen_data <- read_sheet('https://docs.google.com/spreadsheets/d/1aeCDrRHeqY2oLdrcqfirsl4bjca3pcjUg3RP5fJrtyc/pubhtml#', range = "PPR")
  }else{
    stop("Bad scoring summary entered.")
  }
  
  bchen_data <- clean_names(bchen_data,"first_name","last_name")
  
  return(bchen_data)
}

# manual fix Boris Chen data 
manual_ChangesToPlayers_BC <- function(bchen_data){
  
  # recreate names for merging
  bchen_data_nondef <- bchen_data %>% filter(Position != "DST") %>%
    mutate(
      name_breakdown = str_split(Player.Name, pattern = " ")[sapply(str_split(Player.Name, pattern = " "), length) >= 2],
      first_name = '',
      last_name = ''
    )
  # iterate through and restich 
  for (i in 1:nrow(bchen_data_nondef)){
    bchen_data_nondef$first_name[i] <- bchen_data_nondef$name_breakdown[[i]][1]
    bchen_data_nondef$last_name[i] <- bchen_data_nondef$name_breakdown[[i]][2]
  }
  
  # remove name matching process 
  bchen_data_nondef <- bchen_data_nondef %>% select(-name_breakdown,-Player.Name)
  
  # one off 
  # bchen_data_nondef <- bchen_data_nondef %>% mutate(last_name = ifelse(first_name == 'Amon-Ra', 'St. Brown', last_name))

  bchen_data_nondef$first_name <- sub(".", "", bchen_data_nondef$first_name, fixed=TRUE)
  bchen_data_nondef$first_name <- sub(".", "", bchen_data_nondef$first_name, fixed=TRUE)
  bchen_data_nondef$last_name <- sub(".", "", bchen_data_nondef$last_name, fixed=TRUE)
  bchen_data_nondef$last_name <- sub(".", "", bchen_data_nondef$last_name, fixed=TRUE)
  bchen_data_nondef$last_name <- sub(" .*", "", bchen_data_nondef$last_name)
  
  bchen_data_def <- bchen_data %>% filter(Position == "DST") %>% separate(Player.Name, into = c("first_name", "last_name"), sep = " (?=[^ ]+$)")
  
  # re-merge data sets and clean up
  bchen_data <- rbind(bchen_data_nondef,bchen_data_def) %>% arrange(Rank) # arrange by rank
  bchen_data <- bchen_data %>% rename(bc_rank = Rank, bc_tier = Tier, 
                                      bc_best_rank = Best.Rank, bc_worst_rank = Worst.Rank, 
                                      bc_avg_rank = Avg.Rank, bc_std_dev = Std.Dev) %>% select(-Position)
  
  # 2023 one offs 
  # bchen_data <- bchen_data %>% 
  #   mutate(first_name = ifelse(bc_rank == '137', 'Devon', first_name))
}




# Scrape FantasyPros data 
scrape_FantasyPros <- function(config){
  fantasypros_data <- read_csv(config$fantasypros_source)
  # fantasypros_data <- clean_names(fantasypros_data,"first_name","last_name")
  
  non_def_notes <- fantasypros_data %>% filter(!grepl("DST", POS)) %>% separate(`PLAYER NAME`, into = c("first_name", "last_name"), sep = "^\\S*\\K\\s+")
  non_def_notes$first_name <- sub(".", "", non_def_notes$first_name, fixed=TRUE)
  non_def_notes$first_name <- sub(".", "", non_def_notes$first_name, fixed=TRUE)
  non_def_notes$last_name <- sub(".", "", non_def_notes$last_name, fixed=TRUE)
  non_def_notes$last_name <- sub(".", "", non_def_notes$last_name, fixed=TRUE)
  non_def_notes$last_name <- sub(" .*", "", non_def_notes$last_name)
  
  
  
  def_notes <- fantasypros_data %>% filter(grepl("DST", POS)) %>% separate(`PLAYER NAME`, into = c("first_name", "last_name"), sep = " (?=[^ ]+$)")
  fantasypros_data <- rbind(non_def_notes,def_notes) %>% arrange(RK) # arrange by rank
  
  #data engineering 
  fantasypros_data <- fantasypros_data %>% 
    rename(fp_rank = RK, fp_tier = TIERS, ecr_vs_adp = `ECR VS. ADP`, sos = `SOS SEASON`) %>% 
    select(-TEAM, -POS, -`BYE WEEK`) %>% 
    mutate(sos = as.numeric(substr(sos, 1, 1)))
  
  return(fantasypros_data)
}




# Load Manual Player Notes 

manual_PlayerNotes <- function(config){
  manual_player_notes <- read.xlsx(config$manual_PlayerNotes) %>% 
    select(id, JW_Notes)
} 

# Merge Projections 
merge_all_projections <- function(main_projections, bchen_data, fantasypros_data, manual_player_notes){
  # Merge main_projections and boris chen 
  matched_projections <- merge_projections(main_projections, bchen_data, c("first_name","last_name"))# Load and process Fantasy Pros data
  # Merge Fantasy Pros data
  matched_projections <- merge_projections(matched_projections, fantasypros_data,c("first_name","last_name"))# Load and merge personal notes
  # Merge Player Notes 
  matched_projections <- merge_projections(matched_projections, manual_player_notes, c("id")) 
  
  return(matched_projections)

}



# Function to generate the output file

generate_output <-function(df, config){
  
  output_path <- paste0("../output/", 
                        Sys.Date(),"-", 
                        config$scoring_summary,"-", 
                        config$league_name,"-Model-Results.xlsx")
  
  wb <- createWorkbook()
  modifyBaseFont(wb, fontSize =12, fontName ="Futura")
  addWorksheet(wb,"model_output", gridLines =FALSE)
  writeData(wb, sheet =1, x = df, colNames =TRUE, rowNames =FALSE)# Add conditional formatting as required# ...
  
  saveWorkbook(wb, output_path, overwrite =TRUE)
  message("Output saved to: ", output_path)
  
}



