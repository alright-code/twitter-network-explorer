
# Data Functions ----------------------------------------------------------

# Search twitter for n.tweets tweets matching the query and whether to include rts,
#   return a data frame of those tweets
GetData <- function(query.c, num.tweets, include.rts, type) {
  data <- search_tweets2(query.c, n = num.tweets, include_rts = include.rts,
                        token = token, type = type, lang = "en", verbose = TRUE)
  return(data)
}

# Input: data dataframe, query
# Output: dataframe only with tweets including the specified hashtags in query
GetDataSubset <- function(data, query.c) {
  if(length(query.c) == 1) {
    filter(data, query %in% query.c) %>%
      distinct(status_id, .keep_all = TRUE)
  } else {
    filter(data, query %in% query.c) %>%
      group_by(status_id) %>%
      filter(n() > 1) %>%
      ungroup() %>%
      distinct(status_id, .keep_all = TRUE)
  }
}

# Node Functions ----------------------------------------------------------

# Input: data dataframe, query vector
# Output: Data frame with id and value columns
GetNodes <- function(data, query.c) {
  nodes <- data.frame(id = query.c[!is.na(query.c)],
                      label = query.c[!is.na(query.c)],
                      color = color.blue,
                      font = "10px arial #fd7e14")
  nodes$value <- GetNodesValue(data, nodes)
  nodes <- GetCoords(nodes, query.c)
  return(nodes)
}

# Input: data dataframe, nodes dataframe
# Output: nodes value column
GetNodesValue <- function(data, nodes) {
  nodes.rows <- nrow(nodes)
  value <- c(length = nodes.rows)
  # Determine how many tweets include each hashtag
  for(i in 1:nodes.rows) {
    value[i] <- sum(data$query == nodes$id[i])
  }
  return(value)
}

GetCoords <- function(nodes, query.c) {
  radius <- 5
  scale <- 75
  angles <- rev(seq(0, (3/2)*pi, (2 * pi)/12))
  angles <- c(angles, seq((3/2)*pi, 2*pi, (2 * pi)/12)[2:3])
  angles <- unlist(lapply(1:12, function(x) {
    if(is.na(query.c[x])) {
      NA
    } else {
      angles[x]
    }
  }))
  angles <- angles[!is.na(angles)]
  nodes$x <- scale * radius * cos(angles)
  nodes$y <- -scale * radius * sin(angles)
  return(nodes) 
}

# Edge Functions ----------------------------------------------------------

# Input: data dataframe, query vector
# Output: Data frame with to and from columns and attribute columns
GetEdges <- function(data, query.c) {
  edges <- GetToFrom(data, query.c)
  if(!is.null(edges)) {
    edges <- GetEdgesIndices(edges)
    edges <- GetEdgesColors(edges)
  }
  return(edges)
}

# Input: data dataframe, query vector
# Output: Two column dataframe with to and from columns
GetToFrom <- function(data, query.c) {
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
GetEdgesIndices <- function(edges) {
  edges.rows <- nrow(edges)
  edges$index <- 1:edges.rows
  return(edges)
}

# Input: Edges dataframe
# Output: Edges dataframe with color column
GetEdgesColors <- function(edges) {
  edges.rows <- nrow(edges)
  color <- c(length = edges.rows)
  color[1:edges.rows] <- color.white
  edges$color <- color
  return(edges)
}

# Misc Functions ----------------------------------------------------------

StringQueryToVector <- function(query.string) {
  query.c <- scan(text = query.string, what = "character", quiet = TRUE)
  if(length(query.c) < 12) {
    query.c[(length(query.c) + 1):12] <- NA
  }
  return(query.c)
}