library(httr)

# Define your Sleeper API base URL
base_url <- "https://api.sleeper.app/v1"



# content(GET("https://api.sleeper.app/v1/user/dumptruck420"))
# 
# user_id = "868316917696516096"
# league_id = "990703011024445440"
# formal_draft_id = "990703011024445441"
# 
# test_endpoint <- "https://api.sleeper.app/v1/user/868316917696516096/leagues/nfl/2023"
test_endpoint <- "https://api.sleeper.app/v1/draft/1000934868458573824"
test_endpoint <- "https://api.sleeper.app/v1/draft/1000934868458573824/picks"


# test = GET(test_endpoint)
# http_status(test)$message == "Success: (200) OK"
# content())


# Function to get drafted players in a specific draft
get_drafted_players <- function(league_id, draft_id) {
  # Construct the URL for the draft picks endpoint
  endpoint <- paste0(base_url, "/draft/", draft_id, "/picks")
  
  # Make a GET request to the API
  response <- tryCatch({
    GET(endpoint)
  }, error = function(e) {
    # Handle the error here (e.g., print an error message)
    cat("Error in GET request:", conditionMessage(e), "\n")
    return(NULL)
  })
  
  # Check if the response is NULL (indicating an error)
  if (is.null(response)) {
    # Handle the error here (e.g., return an error message)
    return(NULL)
  }
  
  # Check if the request was successful (status code 200)
  if (http_status(response)$message == "Success: (200) OK") {
    # Parse the JSON response
    drafted_players <- content(response, "parsed")
    return(drafted_players)
  } else {
    # Handle errors here (e.g., return an error message)
    cat("HTTP Error:", http_status(response)$status, "\n")
    return(NULL)
  }
}

# Example usage:
league_id <- "990703011024445440"
draft_id <- "1000934868458573824"

drafted_players <- get_drafted_players(league_id, draft_id)

# Check if drafted_players is not NULL before processing
if (!is.null(drafted_players)) {
  # Print the drafted players (or perform any desired operations)
  print(drafted_players)
}



