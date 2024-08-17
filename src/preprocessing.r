
## CLEAR WORKSPACE 
rm(list = ls())

## SOURCE PATH
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## LIBRARIES
# require("remotes")
# remotes::install_github("FantasyFootballAnalytics/ffanalytics")
require("ffanalytics")
require("googlesheets4")
require("dplyr")
require("readr")
require("tidyr")
require("openxlsx")
require("ggplot2")
require("stringr")

## ADDITIONAL SCRIPT LOADING
source("set_points_2024.r")

# RESEARCH
# https://fulltimefantasy.com/



# Fantasy Football Rankings Scraped with ffanalytics package
main_roster_scrape <- scrape_data(pos = positions, season = season_choice, week = 0)
main_projections <- projections_table(main_roster_scrape, scoring_rules = sleeper_league_half_ppr_points) #change scoring based on league

main_projections <- main_projections %>% 
  add_ecr() %>% 
  add_adp() %>% 
  add_aav() %>%
  add_uncertainty() 

main_projections <- main_projections %>% 
  add_player_info()

main_projections <- main_projections %>% filter(avg_type == 'weighted') # going with weighted average calculation

# Quick output for adding your own data
# ids_for_details <- main_projections %>% select(id, pos, rank, adp, first_name, last_name, team) %>% arrange(adp)
# wb <- createWorkbook()
# addWorksheet(wb, "Data")
# writeData(wb, "Data", ids_for_details)
# saveWorkbook(wb, "../output/ids_for_details.xlsx", overwrite = TRUE)



# MANUAL EDITS TO PLAYER NAMES FOR MERGES (improve)
main_projections$first_name <- sub(".", "", main_projections$first_name, fixed=TRUE)
main_projections$first_name <- sub(".", "", main_projections$first_name, fixed=TRUE)

main_projections$last_name <- sub(".", "", main_projections$last_name, fixed=TRUE)
main_projections$last_name <- sub(".", "", main_projections$last_name, fixed=TRUE)

main_projections$last_name <- sub(" .*", "", main_projections$last_name)

# 2023 One-offs 
main_projections <- main_projections %>% 
   mutate(first_name = ifelse(id == '14845', 'Gabe', first_name))

main_projections <- main_projections %>% 
  mutate(first_name = ifelse(id == '14017', 'Jeff', first_name))

# main_projections <- main_projections %>% 
#   mutate(first_name = ifelse(id == '14138', 'TJ', first_name))
# main_projections <- main_projections %>% 
#   mutate(first_name = ifelse(id == '13635', 'DJ', first_name))
# main_projections <- main_projections %>% 
#   mutate(first_name = ifelse(id == '15711', 'Ken', first_name))
# main_projections <- main_projections %>% 
#   mutate(first_name = ifelse(id == '13668', 'DJ', first_name))


# RUN STEP 1 


# Boris Chen Tier Google Sheets -------------------------------------------
# DevNote: Need to add auto login code for google sheets
if(scoring_summary == "Standard"){
  bchen_data <- read_sheet('https://docs.google.com/spreadsheets/d/1aeCDrRHeqY2oLdrcqfirsl4bjca3pcjUg3RP5fJrtyc/pubhtml#', range = "Standard")
}else if(scoring_summary == "Half-PPR"){
  bchen_data <- read_sheet('https://docs.google.com/spreadsheets/d/1aeCDrRHeqY2oLdrcqfirsl4bjca3pcjUg3RP5fJrtyc/pubhtml#', range = "Half PPR")
}else if(scoring_summary == "PPR"){
  bchen_data <- read_sheet('https://docs.google.com/spreadsheets/d/1aeCDrRHeqY2oLdrcqfirsl4bjca3pcjUg3RP5fJrtyc/pubhtml#', range = "PPR")
}else{
  print("Bad scoring summary entered. ")
}


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
bchen_data_nondef <- bchen_data_nondef %>% mutate(last_name = ifelse(first_name == 'Amon-Ra', 'St. Brown', last_name))

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
bchen_data <- bchen_data %>% 
  mutate(first_name = ifelse(bc_rank == '137', 'Devon', first_name))


# merge onto main set (CONFIRM WORKS)
matched_projections <- merge(main_projections, bchen_data, by = c("first_name","last_name"), all.x=TRUE)
bchen_data %>% filter(!(bc_rank %in% matched_projections$bc_rank)) # should be empty 


# Fantasy Pros Tiers, Notes, and Personal Notes ---------------------------
# https://www.fantasypros.com/nfl/rankings/half-point-ppr-cheatsheets.php
#load sources
fantasypros_data <- read_csv("../data/2023/FantasyPros_2024_Draft_ALL_Rankings_HalfPPR.csv") #half-ppr
# fantasypros_data <- read_csv("../data/2023/FantasyPros_2023_Draft_OP_Rankings_SuperFlex.csv") #superflex
# fantasypros_data <- read_csv("../data/2023/FantasyPros_2023_Draft_OP_Rankings_Standard.csv") #standard



# clean up names 
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



# merge fantasypros_data
matched_projections <- merge(matched_projections, fantasypros_data, by = c("first_name","last_name"), all.x=TRUE)


# MY JW NOTES ----------------------------------------------------------------
james_player_notes <- read.xlsx("../data/2023/2023_JW_PlayerNotes.xlsx") %>% select(id, JW_Notes)
matched_projections <- merge(matched_projections,james_player_notes, by = c("id"), all.x=TRUE)


# ADDITIONAL STATS --------------------------------------------------------

# take fp tier - fill in with 25 + average where not available. Good enough resource based on data 
# matched_projections <- matched_projections %>% mutate(player_tier = ifelse(is.na(fp_tier)==TRUE, tier + 25, fp_tier))

## Aggregate Tiers (proj tier, brian chen tier, fantasy pro tier)

rev_tiers <- matched_projections %>% 
  filter(!(pos %in% c('K','DEF'))) %>% 
  # select(first_name, last_name, adp, tier,bc_tier,fp_tier,points) %>% 
  rowwise() %>%
  mutate(master_tier = round(median(c_across(c('tier', 'bc_tier', 'fp_tier')), na.rm=TRUE))) %>% 
  arrange(master_tier,adp) %>% select(id, master_tier)


matched_projections <- merge(matched_projections,rev_tiers, by = c("id"), all.x=TRUE)


# expected round aka 1 for 14 rows then 2 for 14 rows etc 
matched_projections <- matched_projections %>% rowwise() %>% mutate(league_round = ceiling(overall_ecr/team_count)) 
                                                                    
# expected pick 
# indices_to_check = c(8,21,36,49,64,77,92,105,120,133,148,161,176,189,204)
# matched_projections <- matched_projections %>% arrange(adp, overall_ecr, master_tier) 
# matched_projections$expected_slot = ifelse(rownames(matched_projections) %in% indices_to_check,"1","")


# Select pertinent variables for model
model_output <- matched_projections %>% mutate(picked = "") %>% 
  select(picked,first_name,last_name,team,pos,
         master_tier,overall_ecr, adp, league_round, #expected_slot,
         points, sd_pts, ceiling, uncertainty, age, sos,
         JW_Notes) %>% arrange(master_tier,overall_ecr,adp)


# Clean to excel workbook output ------------------------------------------
# Add color coding and sort widget commands 

wb <- createWorkbook()
options("openxlsx.borderColour" = "#4F80BD")
options("openxlsx.borderStyle" = "thin")
options("openxlsx.numFmt" = "0.00")
modifyBaseFont(wb, fontSize = 12, fontName = "Futura")

addWorksheet(wb, sheetName = "model_output", gridLines = FALSE)

# Tab 1
freezePane(wb, sheet = 1, firstRow = TRUE, firstCol = TRUE) ## freeze first row and column
writeData(wb, sheet = 1, x = model_output,
               colNames = TRUE, rowNames = FALSE)
addFilter(wb, 1, row = 1, cols = 1:ncol(model_output))
setColWidths(wb, sheet = 1, cols = "O", widths = 51)

# Format positions for Table 1
RB_Style <- createStyle(bgFill = "#FF9900")
WR_Style <- createStyle(bgFill = "#00CCFF")
TE_Style <- createStyle(bgFill = "#99CC00")
QB_Style <- createStyle(bgFill = "#FFCC00") # #993300
K_Style <- createStyle(bgFill = "#CC99FF")
DST_Style <- createStyle(bgFill = "#808080")

conditionalFormatting(wb, "model_output",
                      cols = 5, rows = 1:nrow(model_output), type = "contains", rule = "RB", style = RB_Style)
conditionalFormatting(wb, "model_output",
                      cols = 5, rows = 1:nrow(model_output), type = "contains", rule = "WR", style = WR_Style)
conditionalFormatting(wb, "model_output",
                      cols = 5, rows = 1:nrow(model_output), type = "contains", rule = "TE", style = TE_Style)
conditionalFormatting(wb, "model_output",
                      cols = 5, rows = 1:nrow(model_output), type = "contains", rule = "QB", style = QB_Style)
conditionalFormatting(wb, "model_output",
                      cols = 5, rows = 1:nrow(model_output), type = "contains", rule = "K", style = K_Style)
conditionalFormatting(wb, "model_output",
                      cols = 5, rows = 1:nrow(model_output), type = "contains", rule = "DST", style = DST_Style)

# 
# Create a color palette for conditional formatting (you can customize this)
unique_numbers <- unique(na.omit(model_output$master_tier))
even_style <- createStyle(bgFill = "#0072BB")
odd_style <- createStyle(bgFill = "#4CAF50")

# Loop through unique numbers and apply conditional formatting
for (i in 1:length(unique_numbers)) {
  number <- unique_numbers[i]
  if (as.numeric(number) %% 2 == 0) {
    choose_style <- even_style
  } else {
    choose_style <- odd_style
  }

  # Set the background color for each cell in the matching rows
  conditionalFormatting(wb, "model_output",
                        cols = 6, rows = 1:nrow(model_output), type = "contains", rule = as.character(number),
                        style = choose_style)
}



# add color scale 
conditionalFormatting(wb, "model_output",
                      cols = 7, rows = 1:nrow(model_output),
                      style = c("green","yellow", "red"),
                      type = "colourScale"
)
conditionalFormatting(wb, "model_output",
                      cols = 8, rows = 1:nrow(model_output),
                      style = c("green","yellow", "red"),
                      type = "colourScale"
)
conditionalFormatting(wb, "model_output",
                      cols = 9, rows = 1:nrow(model_output),
                      style = c("green","yellow", "red"),
                      type = "colourScale"
)
conditionalFormatting(wb, "model_output",
                      cols = 10, rows = 1:nrow(model_output),
                      style = c("green","yellow", "red"),
                      type = "colourScale"
)
conditionalFormatting(wb, "model_output",
                      cols = 11, rows = 1:nrow(model_output),
                      style = c("green","yellow", "red"),
                      type = "colourScale"
)
conditionalFormatting(wb, "model_output",
                      cols = 12, rows = 1:nrow(model_output),
                      style = c("green","yellow", "red"),
                      type = "colourScale"
)
conditionalFormatting(wb, "model_output",
                      cols = 13, rows = 1:nrow(model_output),
                      style = c("green","yellow", "red"),
                      type = "colourScale"
)
conditionalFormatting(wb, "model_output",
                      cols = 14, rows = 1:nrow(model_output),
                      style = c("green","yellow", "red"),
                      type = "colourScale"
)


# Write notebook
output_path = paste0("../output/",Sys.Date(),"-",scoring_summary,"-",league_name,"-Model-Results.xlsx")
saveWorkbook(wb, output_path, overwrite = TRUE) ## save to working directory



# FUTURE ADDITIONS ---------------------------------------------------------

# Depth Chart Position / Prop of exp touches
# Injury Report / %

# FUTURE MODELS  -----------------------------------------------------------
# Cluster Tiers / Project Points

## Model variables 
# - Strength of Season
# - ECR VS. ADP (BULL OR BEAR)
# - AGE
# - YEARS IN LEAGUE
# - INJURY PRESENT / HISTORY









