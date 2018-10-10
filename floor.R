# Node Functions ----------------------------------------------------------

# Input: data dataframe, query vector
# Output: Data frame with id and value columns
getNodes <- function(data, query) {
  if(length(query[!is.na(query)]) != 0) {
    nodes <- data.frame(id = query[!is.na(query)],
                        label = query[!is.na(query)],
                        color = color.blue,
                        font = "10px arial #fd7e14")
    nodes$value <- getNodesValue(data, nodes)
    nodes <- getCoords(nodes, query)
  } else {
    nodes <- NULL
  }
  return(nodes)
}

# Input: data dataframe, nodes dataframe
# Output: nodes value column
getNodesValue <- function(data, nodes) {
  nodes.rows <- nrow(nodes)
  value <- c(length = nodes.rows)
  # Determine how many tweets include each hashtag
  for(i in 1:nodes.rows) {
    value[i] <- sum(data$query == nodes$id[i])
  }
  return(value)
}

getCoords <- function(nodes, query) {
  radius <- 5
  scale <- 75
  angles <- rev(seq(0, (3/2)*pi, (2 * pi)/12))
  angles <- c(angles, seq((3/2)*pi, 2*pi, (2 * pi)/12)[3:2])
  angles <- unlist(lapply(1:12, function(x) {
    if(is.na(query[x])) {
      NA
    } else {
      angles[x]
    }
  }))
  angles <- angles[!is.na(angles)]
  nodes$x <- rep(0, length(query[!is.na(query)]))
  nodes$y <- rep(0, length(query[!is.na(query)]))
  nodes$x[1:min(12, length(query[!is.na(query)]))] <- scale * radius * cos(angles)
  nodes$y[1:min(12, length(query[!is.na(query)]))] <- -scale * radius * sin(angles)
  return(nodes) 
}

# Edge Functions ----------------------------------------------------------

# Input: data dataframe, query vector
# Output: Data frame with to and from columns and attribute columns
getEdges <- function(data, query) {
  query <- query[!is.na(query)]
  if(nrow(data) != 0) {
    edges <- getToFrom(data, query)
    if(!is.null(edges)) {
      edges <- getEdgesIndices(edges)
      edges <- getEdgesColors(edges)
    }
  } else {
    edges <- NULL
  }
  return(edges)
}

# Input: data dataframe, query vector
# Output: Two column dataframe with to and from columns
getToFrom <- function(data, query) {
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
  edges.rows <- nrow(edges)
  if(edges.rows != 0) {
    edges$index <- 1:edges.rows
  }
  return(edges)
}

# Input: Edges dataframe
# Output: Edges dataframe with color column
getEdgesColors <- function(edges) {
  edges.rows <- nrow(edges)
  color <- c(length = edges.rows)
  color[1:edges.rows] <- color.white
  edges$color <- color
  return(edges)
}