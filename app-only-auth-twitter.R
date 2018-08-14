# In order to use application-only authentification, one will need a consumer key and consumer secret.
# For more information regarding this mode of authentification : https://dev.twitter.com/oauth/application-only 
library(httr)
library(base64enc)

get_bearer_token <- function(consumer_key, consumer_secret) {
  # 1. Encode consumer key and secret
  enc_consumer_key <- URLencode(URL = consumer_key)
  enc_consumer_secret <- URLencode(URL = consumer_secret)
  
  key_secret <- sprintf(fmt = "%s:%s", enc_consumer_key, enc_consumer_secret)
  
  # Encoding consumer key and secret using Base64 encoding. key_secret must be convert to raw vector
  enc_key_secret <- base64enc::base64encode(what = charToRaw(key_secret))
  
  # 2. Obtaining bearer token using HTTP POST request
  
  headers_connect <- c("Host"            = "api.twitter.com",
                       "Authorization"   = sprintf("Basic %s", enc_key_secret),
                       "Content-Type"    = "application/x-www-form-urlencoded;charset=UTF-8",
                       "Content-Length"  = "29",
                       "Accept-Encoding" = "gzip")
  
  connect_app <- httr::POST(url    = "https://api.twitter.com/oauth2/token",
                            config = httr::add_headers(headers_connect),
                            body   = "grant_type=client_credentials")
  
  bearer <- httr::content(connect_app)
  r <- httr::add_headers(Authorization = paste0("Bearer ", bearer$access_token))
  structure(r, bearer = bearer, class = c("bearer", "list"))
}

