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
                        color = color.blue,
                        font = "10px arial #fd7e14")
    nodes$id <- getNodesId(queries)
    nodes$label <- getNodesLabel(queries)
    nodes$value <- getNodesValue(data, nodes)
    nodes$position_ <- getPosition(queries)
  } else {
    nodes <- NULL
  }
  return(nodes)
}

getNodesId <- function(queries) {
  # Get id (search query) for each node.
  #
  # Args: 
  #   queries: Vector of strings to be used to search for tweets. Queries are allowed to be NA.
  #   
  # Returns:
  #   Data frame column with the id of each node, deteremined from the literal query.
  ids <- lapply(queries[!is.na(queries)], function(query) {
    split <- strsplit(query, "\\* ")[[1]]
    if(length(split) == 2) {
      split[2]
    } else {
      query
    }
  })
  return(ids)
}

getNodesLabel <- function(queries) {
  # Get label for each node.
  #
  # Args: 
  #   queries: Vector of strings to be used to search for tweets. Queries are allowed to be NA.
  #   
  # Returns:
  #   Data frame column with the label of each node, deteremined from the literal query.
  labels <- lapply(queries[!is.na(queries)], function(query) {
    split <- strsplit(query, "\\* ")[[1]]
    if(length(split) == 2) {
      split[1]
    } else {
      query
    }
  })
  return(labels)
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

getPosition <- function(queries) {
  # Get location around the circle of each node.
  # 
  # Args:
  #   queries: Vector of strings to be used to search for tweets. Queries are allowed to be NA.
  #
  # Returns:
  #   Data frame column with the position of each query's node, # from 1-12.
  position <- vapply(1:length(queries), function(i){
    if(!is.na(queries[[i]])){
      i
    } else {
      0
    }
  }, numeric(1))
  position_ <- position[position != 0]
  return(position_) 
}

getCoords <- function(nodes) {
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
  nodes$x <- vapply(nodes$position_, function(i) {
    if(i <= 12) {
      scale * radius * cos(angles[[i]])
    } else {
      0
    }
  }, numeric(1))
  nodes$y <- vapply(nodes$position_, function(i) {
    if(i <= 12) {
      -scale * radius * sin(angles[[i]])
    } else {
      0
    }
  }, numeric(1))
  return(nodes)
}

# Edge Functions ----------------------------------------------------------

getEdges <- function(data, queries) {
  # Get edge data for visNetwork input.
  # 
  # Args:
  #   data: Tibble of tweet data returned by rtweet.
  #   queries: Vector of strings to be used to search for tweets. Queries are NOT allowed to be NA.
  #
  # Returns:
  #   Data frame in the format:
  #     to  from  index  color 
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