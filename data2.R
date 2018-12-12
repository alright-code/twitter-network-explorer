#' Fetches tweet from database.
#'
#' @param ??
#' @return Dataframe of tweet data.
fetchData <- function() {
  # TODO: Need more information about fetching data and format
}

#' Gets subset of data.
#'
#' @param data Dataframe with tweet data.
#' @param subset_query String to base subset from.
#' @param subset_type String type of query. Can be hashtag, screen_name, mentions_screen_name, word. 
#' @return Subset of main data based on query and type.
getSubset <- function(data, subset_query, subset_type) {
  subset_query <- toupper(subset_query)
  if(subset_type == "hashtags") {
    data_subset <- filter(data, toupper(data$hashtags) %in% subset_query)
  } else if(subset_type == "screen_name") {
    data_subset <- filter(data, toupper(data$screen_name) %in% subset_query)
  } else if(subset_type == "mentions_screen_name") {
    data_subset <- filter(data, toupper(data$mentions_screen_name) %in% subset_query)
  } else if(subset_type == "word") {
    # TODO: Text processing
  }
  return(data_subset)
}