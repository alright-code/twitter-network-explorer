# Node Functions ----------------------------------------------------------

getNodes <- function(data, queries) {
  # Get node data for visNetwork input.
  # 
  # Args:
  #   data: Tibble of tweet data returned by rtweet.
  #   queries: Vector of strings to be used to search for tweets. Queries are allowed to be NA.
  #
  # Returns:
  #   Data frame in the format:
  #     id  label  color  font  value  x  y 
  if(length(queries[!is.na(queries)]) != 0) {
    nodes <- data.frame(id = queries[!is.na(queries)],
                        label = queries[!is.na(queries)],
                        color = color.blue,
                        font = "10px arial #fd7e14")
    nodes$value <- getNodesValue(data, nodes)
    nodes <- getCoords(queries, nodes)
  } else {
    nodes <- NULL
  }
  return(nodes)
}

getNodesValue <- function(data, nodes) {
  # Get value (size) for each node.
  # 
  # Args:
  #   data: Tibble of tweet data returned by rtweet.
  #   nodes: Data frame containing an id column.
  #
  # Returns:
  #   Data frame column with the size of each node (amount of tweets returned by the node's query).
  nodes_rows <- nrow(nodes)
  value <- c(length = nodes_rows)
  # Determine how many tweets include each hashtag
  for(i in 1:nodes_rows) {
    value[i] <- sum(data$query == nodes$id[i])
  }
  return(value)
}

getCoords <- function(queries, nodes) {
  # Get location (x,y) for each node.
  # 
  # Args:
  #   queries: Vector of strings to be used to search for tweets. Queries are allowed to be NA.
  #   nodes: Data frame 
  #
  # Returns:
  #   Data frame column with the position of each query's node.
  radius <- 5
  scale <- 75
  angles <- rev(seq(0, (3/2)*pi, (2 * pi)/12))
  angles <- c(angles, seq((3/2)*pi, 2*pi, (2 * pi)/12)[3:2])
  angles <- unlist(lapply(1:12, function(x) {
    if(is.na(queries[x])) {
      NA
    } else {
      angles[x]
    }
  }))
  angles <- angles[!is.na(angles)]
  nodes$x <- rep(0, length(queries[!is.na(queries)]))
  nodes$y <- rep(0, length(queries[!is.na(queries)]))
  nodes$x[1:min(12, length(queries[!is.na(queries)]))] <- scale * radius * cos(angles)
  nodes$y[1:min(12, length(queries[!is.na(queries)]))] <- -scale * radius * sin(angles)
  return(nodes) 
}

# Edge Functions ----------------------------------------------------------

getEdges <- function(data, queries) {
  # Get edge data for visNetwork input.
  # 
  # Args:
  #   data: Tibble of tweet data returned by rtweet.
  #   queries: Vector of strings to be used to search for tweets. Queries are allowed to be NA.
  #
  # Returns:
  #   Data frame in the format:
  #     to  from  index  color 
  queries <- queries[!is.na(queries)]
  if(nrow(data) != 0) {
    edges <- getToFrom(data)
    if(!is.null(edges)) {
      edges <- getEdgesIndices(edges)
      edges <- getEdgesColors(edges)
    }
  } else {
    edges <- NULL
  }
  return(edges)
}

getToFrom <- function(data) {
  # Get edge data for visNetwork input.
  # 
  # Args:
  #   data: Tibble of tweet data returned by rtweet.
  #
  # Returns:
  #   Data frame in the format:
  #     to  from  index  color 
  cleaned <- data %>%
    group_by(status_id) %>%
    filter(n() > 1)
  if(nrow(cleaned) == 0) {
    edges <- NULL
  } else {
    cleaned <- cleaned %>%
      transmute(query = list(t(combn(query, 2)))) %>%
      distinct() %>%
      select(query)
    edges <- as_tibble(do.call(rbind, cleaned$query)) %>%
      group_by(V1, V2) %>%
      mutate(value = n()) %>%
      distinct() %>%
      setNames(c("to", "from", "value"))
  }
  return(edges)
}

# Input: edges dataframe
# Ouput: edges dataframe with indices column
# Note: Indices are used to track what edge is selected in shiny
getEdgesIndices <- function(edges) {
  # Add an index column to edges to keep track of selected edge.
  # 
  # Args:
  #   edges: Data frame. 
  #
  # Returns:
  #   Data frame with column numbered 1-nrows 
  edges.rows <- nrow(edges)
  if(edges.rows != 0) {
    edges$index <- 1:edges.rows
  }
  return(edges)
}

# Input: Edges dataframe
# Output: Edges dataframe with color column
getEdgesColors <- function(edges) {
  # Add a color column to edges.
  # 
  # Args:
  #   edges: Data frame.
  #
  # Returns:
  #   Data frame with color column (white).
  edges.rows <- nrow(edges)
  color <- c(length = edges.rows)
  color[1:edges.rows] <- color.white
  edges$color <- color
  return(edges)
}