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
#' @param subset_type String type of query. Can be hashtags, screen_name, mentions_screen_name, word. 
#' @return Subset of main data based on query and type.
getSubset <- function(data, subset_query, subset_type) {
  subset_query <- toupper(subset_query)
  if(subset_type == "hashtags") {
    data_subset <- filter(data, toupper(data$hashtags) %in% subset_query)
  } else if(subset_type == "screen_name") {
    data_subset <- filter(data, toupper(data$screen_name) %in% subset_query)
  } else if(subset_type == "mentions_screen_name") {
    data_subset <- filter(data, toupper(data$mentions_screen_name) %in% subset_query)
  } else if(subset_type == "text") {
    # TODO: Text processing
  }
  return(data_subset)
}

#' Gets subset of data from multiple queries.
#'
#' @param data Dataframe with tweet data.
#' @param subset_query Vector of strings to base subset on.
#' @param subset_type Vector of string type of query. Can be hashtags, screen_name, mentions_screen_name, word. 
#' @return Subset of main data based on queries and types.
getSubset2 <- function(data, subset_queries, subset_types) {
  subsets <- vector("list", length = length(subset_queries))
  for(i in 1:length(subsets)) {
    subsets[[i]] <- getSubset(data, subset_queries[i], subset_types[i])
  }
  subset <- do.call("rbind", subsets) %>%
    distinct()
  return(subset)
}

#' Gets graph node from data.
#' 
#' @param data Dataframe with tweet data.
#' @param node_query String to base node off of.
#' @param node_type Type of the node. Can be hashtags, screen_name, mentions_screen_name, text.
#' @param node_name Name of the node.
#' @return Dataframe of node data for graph. Node is located at (0,0).
getNode <- function(data, node_query, node_type, node_name) {
  node_value <- nrow(getSubset(data, node_query, node_type))
  node <- data.frame(id = node_query,
                      label = node_name,
                      color = color.blue,
                      font = "10px arial #fd7e14",
                      value = node_value,
                      x = 0,
                      y = 0,
                      type = node_type)
  return(node)
}

#' Joins together list of nodes into one dataframe.
#' 
#' @param nodes_list list of node dataframes to join in their list order.
#' @return Node dataframes joined into one dataframe.
joinNodes <- function(nodes_list) {
  nodes <- do.call("rbind", nodes_list)
  nodes <- updatePositions(nodes)
  return(nodes)
}

#' Updates the x and y values of each node.
#' 
#' @param nodes Single dataframe of node data.
#' @param nodes_list list of node dataframes.
#' @returns Dataframe of node data with correct positions.
updatePositons <- function(nodes, nodes_list) {
  radius <- 5
  scale <- 75
  angles <- rev(seq(0, (3/2)*pi, (2 * pi)/12))
  angles <- c(angles, seq((3/2)*pi, 2*pi, (2 * pi)/12)[3:2])
  for(i in 1:min(length(a), 12)) {
    if(!is.null(nodes_list[[i]])) {
      nodes$x[i] <- scale * radius * cos(angles[[i]])
      nodes$y[i] <- -scale * radius * sin(angles[[i]])
    }
  }
  return(nodes)
}

#' Gets graph node edge data from data.
#' 
#' @param data Dataframe with tweet data.
#' @param nodes Dataframe of node data.
#' @param node_query Node to get edges for.
#' @param edge_type Type of the edge. Can be hashtags, screen_name, mentions_screen_name, text.
#' @returns Dataframe of edge data.
getEdges <- function(data, nodes, node_query, edge_type) {
  data_subset <- getSubset2(data, )
}