
## CLEAR WORKSPACE 
rm(list = ls())

## SOURCE PATH
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## LIBRARIES
require("googlesheets4")
require("ffanalytics")
require("dplyr")
require("readr")
require("tidyr")
require("openxlsx")
require("ggplot2")

## ADDITIONAL SCRIPT LOADING
source("set_points.r")

# Fantasy Football Rankings Scraped with ffanalytics package
main_roster_scrape <- scrape_data(pos = positions, season = season_choice, week = 0)
main_projections <- projections_table(main_roster_scrape, scoring_rules = yahoo_jim_standard_points)

main_projections <- main_projections %>% 
  add_ecr() %>% 
  add_adp() %>% 
  add_aav() %>%
  add_uncertainty() 

main_projections <- main_projections %>% 
  add_player_info()

main_projections <- main_projections %>% filter(avg_type == 'weighted') # going with weighted average calculation


# 2022 MANUAL EDITS TO PLAYER NAMES FOR MERGES 
main_projections$first_name <- sub(".", "", main_projections$first_name, fixed=TRUE)
main_projections$last_name <- sub(".", "", main_projections$last_name, fixed=TRUE)
main_projections$last_name <- sub(" .*", "", main_projections$last_name)

main_projections <- main_projections %>% 
  mutate(first_name = ifelse(id == '15319', 'Joshua', first_name))
main_projections <- main_projections %>% 
  mutate(first_name = ifelse(id == '14138', 'TJ', first_name))
main_projections <- main_projections %>% 
  mutate(first_name = ifelse(id == '13635', 'DJ', first_name))
main_projections <- main_projections %>% 
  mutate(first_name = ifelse(id == '15711', 'Ken', first_name))
main_projections <- main_projections %>% 
  mutate(first_name = ifelse(id == '13668', 'DJ', first_name))



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
bchen_data_nondef <- bchen_data %>% filter(Position != "DST") %>% separate(Player.Name, into = c("first_name", "last_name"), sep = "^\\S*\\K\\s+")
bchen_data_nondef$first_name <- sub(".", "", bchen_data_nondef$first_name, fixed=TRUE)
bchen_data_nondef$last_name <- sub(".", "", bchen_data_nondef$last_name, fixed=TRUE)
bchen_data_nondef$last_name <- sub(" .*", "", bchen_data_nondef$last_name)

bchen_data_def <- bchen_data %>% filter(Position == "DST") %>% separate(Player.Name, into = c("first_name", "last_name"), sep = " (?=[^ ]+$)")

# re-merge data sets and clean up
bchen_data <- rbind(bchen_data_nondef,bchen_data_def) %>% arrange(Rank) # arrange by rank
bchen_data <- bchen_data %>% rename(bc_rank = Rank, bc_tier = Tier, 
                                    bc_best_rank = Best.Rank, bc_worst_rank = Worst.Rank, 
                                    bc_avg_rank = Avg.Rank, bc_std_dev = Std.Dev) %>% select(-Position)
## edits to names 
bchen_data <- bchen_data %>% 
  mutate(first_name = ifelse(last_name == 'Hockenson', 'TJ', first_name))

# merge onto main set (CONFIRM WORKS)
matched_projections <- merge(main_projections, bchen_data, by = c("first_name","last_name"), all.x=TRUE)
bchen_data %>% filter(!(bc_rank %in% matched_projections$bc_rank)) # should be empty 


# Fantasy Pros Tiers, Notes, and Personal Notes ---------------------------
# https://www.fantasypros.com/nfl/rankings/half-point-ppr-cheatsheets.php
#load sources
# fantasypros_halfppr_tiers <- read_csv("../data/FantasyPros_2022_Draft_ALL_Rankings_HalfPPR.csv")
fantasypros_data <- read_csv("../data/FantasyPros_2022_Draft_OP_Rankings_Standard_SuperFlex.csv")
james_player_notes <- read_csv("../data/2022_JamesPlayerNotes.csv") %>% filter(`JAMES NOTES` != "")

#merge on name 
tier_notes_data <- merge(fantasypros_data,james_player_notes, by = c("PLAYER NAME"), all.x=TRUE)

# clean up names 
non_def_notes <- tier_notes_data %>% filter(!grepl("DST", POS)) %>% separate(`PLAYER NAME`, into = c("first_name", "last_name"), sep = "^\\S*\\K\\s+")
non_def_notes$first_name <- sub(".", "", non_def_notes$first_name, fixed=TRUE)
non_def_notes$last_name <- sub(".", "", non_def_notes$last_name, fixed=TRUE)
non_def_notes$last_name <- sub(" .*", "", non_def_notes$last_name)

def_notes <- tier_notes_data %>% filter(grepl("DST", POS)) %>% separate(`PLAYER NAME`, into = c("first_name", "last_name"), sep = " (?=[^ ]+$)")
tier_notes_data <- rbind(non_def_notes,def_notes) %>% arrange(RK) # arrange by rank

tier_notes_data <- tier_notes_data %>% rename(fp_rank = RK, fp_tier = TIERS, fp_notes = NOTES, jw_notes = `JAMES NOTES`) %>% select(-TEAM, -POS, -`BYE WEEK`)

tier_notes_data <- tier_notes_data %>% 
  mutate(first_name = ifelse(last_name == 'Hockenson', 'TJ', first_name))


# merge 
matched_projections <- merge(matched_projections, tier_notes_data, by = c("first_name","last_name"), all.x=TRUE)



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


# ADDITIONAL STATS --------------------------------------------------------

## Aggregate Tiers 

# take fp tier - fill in with 25 + average where not available. Good enough resource based on data 
matched_projections <- matched_projections %>% mutate(player_tier = ifelse(is.na(fp_tier)==TRUE, tier + 25, fp_tier))
# expected round aka 1 for 14 rows then 2 for 14 rows etc 
matched_projections <- matched_projections %>% mutate(adp_round = ceiling(adp/team_count))

# Select pertinent variables for model
model_output <- matched_projections %>% mutate("picked" = "") %>% select("picked","first_name","last_name","pos","team","points", "adp",
                                         "adp_round","player_tier","fp_rank", "bc_avg_rank","rank",
                                         "pos_rank", "uncertainty", "jw_notes") %>% arrange(player_tier,fp_rank)

notes <- matched_projections %>% select("first_name","last_name","pos","team","age","exp","player_tier","fp_rank","fp_notes") %>% arrange(player_tier,fp_rank)



# Clean to excel workbook output ------------------------------------------
# Add color coding and sort widget commands 

wb <- createWorkbook()
options("openxlsx.borderColour" = "#4F80BD")
options("openxlsx.borderStyle" = "thin")
options("openxlsx.numFmt" = "0.00")
modifyBaseFont(wb, fontSize = 10, fontName = "Futura")

addWorksheet(wb, sheetName = "FF Model Output", gridLines = FALSE)
addWorksheet(wb, sheetName = "Notes", gridLines = FALSE)

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

conditionalFormatting(wb, "FF Model Output",
                      cols = 4, rows = 1:nrow(model_output), type = "contains", rule = "RB", style = RB_Style)
conditionalFormatting(wb, "FF Model Output",
                      cols = 4, rows = 1:nrow(model_output), type = "contains", rule = "WR", style = WR_Style)
conditionalFormatting(wb, "FF Model Output",
                      cols = 4, rows = 1:nrow(model_output), type = "contains", rule = "TE", style = TE_Style)
conditionalFormatting(wb, "FF Model Output",
                      cols = 4, rows = 1:nrow(model_output), type = "contains", rule = "QB", style = QB_Style)
conditionalFormatting(wb, "FF Model Output",
                      cols = 4, rows = 1:nrow(model_output), type = "contains", rule = "K", style = K_Style)
conditionalFormatting(wb, "FF Model Output",
                      cols = 4, rows = 1:nrow(model_output), type = "contains", rule = "DST", style = DST_Style)

# add color scale 

conditionalFormatting(wb, "FF Model Output",
                      cols = 6, rows = 1:nrow(model_output),
                      style = c("green","yellow", "red"),
                      type = "colourScale"
)
conditionalFormatting(wb, "FF Model Output",
                      cols = 7, rows = 1:nrow(model_output),
                      style = c("green","yellow", "red"),
                      type = "colourScale"
)
conditionalFormatting(wb, "FF Model Output",
                      cols = 8, rows = 1:nrow(model_output),
                      style = c("green","yellow", "red"),
                      type = "colourScale"
)
conditionalFormatting(wb, "FF Model Output",
                      cols = 9, rows = 1:nrow(model_output),
                      style = c("green","yellow", "red"),
                      type = "colourScale"
)
conditionalFormatting(wb, "FF Model Output",
                      cols = 10, rows = 1:nrow(model_output),
                      style = c("green","yellow", "red"),
                      type = "colourScale"
)
conditionalFormatting(wb, "FF Model Output",
                      cols = 11, rows = 1:nrow(model_output),
                      style = c("green","yellow", "red"),
                      type = "colourScale"
)
conditionalFormatting(wb, "FF Model Output",
                      cols = 12, rows = 1:nrow(model_output),
                      style = c("green","yellow", "red"),
                      type = "colourScale"
)
conditionalFormatting(wb, "FF Model Output",
                      cols = 13, rows = 1:nrow(model_output),
                      style = c("green","yellow", "red"),
                      type = "colourScale"
)
conditionalFormatting(wb, "FF Model Output",
                      cols = 14, rows = 1:nrow(model_output),
                      style = c("green","yellow", "red"),
                      type = "colourScale"
)

# Tab 2
freezePane(wb, sheet = 1, firstRow = TRUE, firstCol = TRUE) ## freeze first row and column
writeDataTable(wb, sheet = 2, x = notes,
               colNames = TRUE, rowNames = FALSE,
               tableStyle = "TableStyleLight9")

setColWidths(wb, sheet = 2, cols = "I", widths = 101) #make it go down a row 

# Write notebook
output_path = paste0("../output/",Sys.Date(),"-",scoring_summary,"-Model-Results.xlsx")
saveWorkbook(wb, output_path, overwrite = TRUE) ## save to working directory













