# Load necessary libraries
library(httr)
library(jsonlite)

# Define the Sleeper API base URL
base_url <- "https://api.sleeper.app/v1"

# Replace 'your_league_id' with your actual league ID
league_id <- "990703011024445440"

# Define a function to get all available players in the draft
get_all_players_in_draft <- function(league_id, draft_year) {
  # Construct the URL for the available players endpoint
  endpoint <- paste0(base_url, "/league/", league_id, "/available_players/", draft_year)
  
  # Make a GET request to the API
  response <- GET("https://api.sleeper.app/v1/players/nfl")
  
  # Check if the request was successful (status code 200)
  if (http_status(response)$message == "Success: (200) OK") {
    # Parse the JSON response
    drafted_players <- content(response, "parsed")
    return(drafted_players)
  } else {
    # Handle errors here (e.g., print an error message)
    cat("HTTP Error:", http_status(response)$status, "\n")
    return(NULL)
  }
}

# Year for the 2023 draft
draft_year <- 2023

# Get all available players in the 2023 draft
drafted_players <- get_all_players_in_draft(league_id, draft_year)

# Check if drafted_players is not NULL before processing
if (!is.null(drafted_players)) {
  # Convert the list of players to a data frame
  test <-  as.data.frame(do.call(rbind, drafted_players))
  test$player_id <- rownames(test)
  df <- as.data.frame(drafted_players)
  
  # Display the data frame
  print(df)
} else {
  cat("Failed to retrieve draft data.")
}
