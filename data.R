getData <- function(queries, num_tweets, include_rts, token, type) {
  # Gets tweet data using rtweet based off search query.
  # 
  # Args:
  #   queries: Vector of strings to be used to search for tweets. Queries are allowed to be NA.
  #   num_tweets: Number of tweets to be returned for each query.
  #   include_rts: Boolean to determine whether to include retweets in result
  #   token: Twitter API token
  #   type: String can be "recent", "popular", or "mixed" determining type of tweets searched for.
  #
  # Returns:
  #   Tibble of tweet search results.
  queries <- lapply(queries[!is.na(queries)], function(query) {
    split <- strsplit(query, "\\* ")[[1]]
    if(length(split) == 2) {
      split[2]
    } else {
      query
    }
  })
  data <- search_tweets2(queries, n = num_tweets, include_rts = include_rts,
                         token = token, type = type, lang = "en", verbose = TRUE)
  return(data)
}

getDataSubset <- function(data, subset_queries) {
  # Gets tweet data from the main data tibble corresponding to a specific search query.
  # 
  # Args:
  #   data: Tibble of tweet data returned by rtweet.
  #   subset_queries: Vector of strings corresponding to queries to be extracted from data.
  #                   Can be a single query or multiple.
  #
  # Returns:
  #   Tibble only including unique tweets with the queries in subset_query
  if(length(subset_queries) == 1) {
    filter(data, query %in% subset_queries) %>%
      distinct(status_id, .keep_all = TRUE)
  } else {
    filter(data, query %in% subset_queries) %>%
      group_by(status_id) %>%
      filter(n() > 1) %>%
      ungroup() %>%
      distinct(status_id, .keep_all = TRUE)
  }
}