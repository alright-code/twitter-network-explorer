library(rtweet)
source("app-only-auth-twitter.R")

RateLimitedLookup <- function(text, token) {
  rtlimit <- rate_limit(token, "search/lookup")
  remaining <- rtlimit[["remaining"]] * 100
  reset <- rtlimit[["reset"]]
  reset <- as.numeric(reset, "secs")
  
  if (identical(remaining, 0)) {
    ntimes <- ceiling((length(text) - remaining) / 30000)
  } else {
    ntimes <- ceiling((length(text) - remaining) / 30000) + 1
  }
  rt <- vector("list", ntimes)
  maxid <- max_id
  for (i in seq_len(ntimes)) {
    ## if rate limited (exhausted token)
    if (any(identical(remaining, 0), isTRUE(remaining < 10))) {
      message(paste0(
        "retry on rate limit...\n",
        "waiting about ",
        round(reset / 60, 0),
        " minutes..."))
      Sys.sleep(reset + 2)
      remaining <- 300 * 100    # Remaining is number of searches left in the token
    }###
    rt[[i]] <- tryCatch(
      .lookup_tweets(
        statuses = text[(i*(remaining - 1)):remaining]
        token = token
        ...),
      error = function(e) return(NULL))
    ## break if error
    if (is.null(rt[[i]])) break
    ## break if final i
    if (i == ntimes) break
    if (parse) {
      ## get next maxid
      maxid.new <- rt[[i]][["status_id"]][[NROW(rt[[i]])]]
    } else {
      maxid.new <- return_last(unlist(go_get_var(rt[[1]], "statuses", "id")))
    }
    ## break if new maxid is null, empty, or unchanged
    if (any(is.null(maxid.new),
            identical(length(maxid.new), 0L),
            identical(maxid, maxid.new))) break
    ## update maxid value
    maxid <- maxid.new
    ## refresh rate limit data
    rtlimit <- rate_limit(token, "search/tweets")
    remaining <- rtlimit[["remaining"]] * 100
    reset <- rtlimit[["reset"]]
    reset <- as.numeric(reset, "secs")
  }
}  


consumer_key <- "YysJwhUvIYyEZsOqLtE5mlsHv"
consumer_secret <- "EC997cOOlCzp4qedKVaYCB2zs8HnPyKEhIqLV2PNKCirXartcE"

token <- get_bearer_token(consumer_key, consumer_secret)

files <- rev(list.files("/data/BLM/bth_ids", pattern = "*.txt"))

for(file in files) {
  print(paste("Reading ids for", file))
  # Read in ids
  text <- read.delim(paste0("/data/BLM/bth_ids/", file), colClasses = c("character"))[, 1]
  # Lookup tweet ids
  print(paste("Looking up", length(text), "tweets for", file))
  out <- lookup_tweets(text, token = token)
  print(paste("Saving csv for", file))
  save_as_csv(out, paste0("/data/BLM/bth_data/", gsub("*.txt", "", file)))
  # Copy text file to finished folder
  print(paste("Copying", file))
  file.copy(from = paste0("/data/BLM/bth_ids/", file), to = "/data/BLM/bth_ids/alex_bth_ids_done/")
  # Delete old text file
  print(paste("Removing old", file))
  file.remove(paste0("/data/BLM/bth_ids/", file))
  print(paste(file, "has finished."))
  #readline(prompt="Press [enter] to continue")
}

