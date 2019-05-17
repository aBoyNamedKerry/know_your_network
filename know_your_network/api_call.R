library(httr)
library(jsonlite)
library(dotenv)

get_events <- function(location = 'Birmingham', date_from = Sys.Date(), date_to = Sys.Date() + 7){
  username <- "Paste_API_Username_Here"
  password <- "Paste_API_Password_Here"

  dotenv::load_dot_env()
  
  base <- "https://api.ents24.com/auth/token"
  body <- list(client_id = CLIENT_ID, client_secret = CLIENT_SECRET)
  response <- POST(base, body = body)
  response_text <- content(response, "text", encoding = 'UTF-8')
  response_list <- fromJSON(response_text, flatten = TRUE)
  access_token <- response_list$access_token
  
  base <- "https://api.ents24.com/event/list"
  query <- paste(base, "?", 'location=name:', location, '&', 'date_from=', date_from, '&', 'date_to=', date_to, sep="")
  response <- GET(query, add_headers(Authorization = access_token))
  
  response_text <- content(response, "text", encoding = 'UTF-8')
  response_df <- fromJSON(response_text, flatten = TRUE)
  response_df
}

df <- get_events(location = 'Birmingham', date_from = '2019-05-18', date_to = '2019-05-22')
