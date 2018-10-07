# Data Functions ----------------------------------------------------------

# Search twitter for n.tweets tweets matching the query and whether to include rts,
#   return a data frame of those tweets
getData <- function(query, num.tweets, include.rts, type) {
  query <- query[!is.na(query)]
  data <- search_tweets2(query, n = num.tweets, include_rts = include.rts,
                         token = token, type = type, lang = "en", verbose = TRUE)
  return(data)
}

# Input: data dataframe, query
# Output: dataframe only with tweets including the specified hashtags in query
getDataSubset <- function(data, subset_query) {
  if(length(subset_query) == 1) {
    filter(data, query %in% subset_query) %>%
      distinct(status_id, .keep_all = TRUE)
  } else {
    filter(data, query %in% subset_query) %>%
      group_by(status_id) %>%
      filter(n() > 1) %>%
      ungroup() %>%
      distinct(status_id, .keep_all = TRUE)
  }
}