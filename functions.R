###############################################################################
# DATA FUNCTIONS
###############################################################################

# Search twitter for n.tweets tweets matching the query and whether to include rts,
#   return a data frame of those tweets
GetData <- function(query.c, num.tweets, include.rts) {
  query.string <- paste(query.c, collapse = " OR ")
  data <- search_tweets(query.string, n = num.tweets, include_rts = include.rts,
                        token = token, type = 'recent')
  return(data)
}

# Input: data dataframe, query
# Output: dataframe only with tweets including the specified hashtags in query
GetDataSubset <- function(data, query.c) {
  # Remove hashtag symbols from query 
  clean.query.c <- toupper(sapply(query.c, USE.NAMES = FALSE, function(x) {
    gsub('#', '', x)
  }))
  tmp <- sapply(data$hashtags, function(x) {
    if(all(clean.query.c %in% toupper(x))) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  data.subset <- filter(data, tmp)
  return(data.subset)
}

GetAllDataSubsets <- function(data, query.c) {
  all.subsets <- vector("list", 12)
  names(all.subsets) <- toupper(query.c)
  x <- 1
  for(hashtag in toupper(query.c)) {
    all.subsets[[hashtag]] <- list(index = x, data = GetDataSubset(data, hashtag)) 
    x <- x + 1
  }
  return(all.subsets)
}

###############################################################################
# NODE FUNCTIONS
###############################################################################

# Input: data dataframe, query vector
# Output: Data frame with id and value columns
GetNodes <- function(data, query.c) {
  nodes <- GetId(query.c)
  nodes$value <- GetNodesValue(data, nodes)
  nodes$label <- GetNodesLabel(nodes)
  nodes$color <- GetNodesColor(nodes)
  nodes$font <- GetNodesFont(nodes)
  nodes <- GetCoords(nodes)
  return(nodes)
}

# Input: query vector
# Output: nodes dataframe with id column
GetId <- function(query.c) {
  # Create id vector to store included hashtags, for consistancy use uppercase
  id <- toupper(query.c)
  nodes <- data.frame(id = id)
  return(nodes)
}

# Input: data dataframe, nodes dataframe
# Output: nodes value column
GetNodesValue <- function(data, nodes) {
  nodes.rows <- nrow(nodes)
  value <- c(length = nodes.rows)
  # Determine how many tweets include each hashtag
  all.hashtags <- paste('#',toupper(unlist(data$hashtags)), sep = '')
  for(i in 1:nodes.rows) {
    value[i] <- sum(all.hashtags == nodes$id[i])
  }
  return(value)
}

# Input: nodes dataframe
# Ouput: nodes label column
GetNodesLabel <- function(nodes) {
  return(nodes$id)
}

# Input: nodes dataframe
# Ouput: nodes color column
GetNodesColor <- function(nodes) {
  nodes.rows <- nrow(nodes)
  color <- c(length = nodes.rows)
  color[1:nodes.rows] <- color.blue
  return(color)
}

# Input: nodes dataframe
# Ouput: nodes font column
GetNodesFont <- function(nodes) {
  nodes.rows <- nrow(nodes)
  font <- character(length = nodes.rows)
  font[1:nodes.rows] <- '0px arial #fd7e14'
  return(font)
}

GetCoords <- function(nodes) {
  radius <- 5
  scale <- 75
  remove <- nrow(nodes) - 12
  angles <- rev(seq(-(pi/2), (3/2) * pi, (2 * pi)/12))[1:nrow(nodes)]
  nodes$x <- scale * radius * cos(angles)
  nodes$y <- -scale * radius * sin(angles)
  return(nodes) 
}

###############################################################################
# EDGE FUNCTIONS
###############################################################################

# Input: data dataframe, query vector
# Output: Data frame with to and from columns and attribute columns
GetEdges <- function(data, query.c) {
  edges <- GetToFrom(data, query.c)
  if(nrow(edges) > 0) {
    edges <- GetEdgesValues(edges)
    edges <- GetEdgesIndices(edges)
    edges <- GetEdgesColors(edges)
  }
  return(edges)
}

# Input: data dataframe, query vector
# Output: Two column dataframe with to and from columns
GetToFrom <- function(data, query.c) {
  # Create a list identical to data$hashtags, but remove the hashtags not 
  #   searched for. Everything is uppercase
  matched <- lapply(data$hashtags, function(x) {
    intersect(toupper(paste0("#", x)), toupper(query.c))
  })
  # Sometimes there is no hashtag?? Different font, will need to look into
 
  # Generate a two column matrix that takes each entry in the matched list and 
  #    makes an edge for each combination of node in that entry
  edges <- do.call(rbind, lapply(matched, function (x) {
    x.sorted <- sort(x)
    if(length(x.sorted) > 1) {
      t(combn(x.sorted, 2))
    } else {
      tmp <- matrix(nrow = 1, ncol = 2)
      tmp[1,1] <- x.sorted
      tmp
    }
  }))
  edges <- as.data.frame(edges)
  # Remove edges that lead nowhere
  edges <- edges[!is.na(edges[ ,2]), ]
  colnames(edges) <- c('to', 'from')
  return(edges)
}

# Input: Edges dataframe
# Output: Edges dataframe with value column
GetEdgesValues <- function(edges) {
  # Use table to find the frequencies of edges
  edges <- data.frame(table(edges))
  colnames(edges) <- c('to', 'from', 'value')
  # Remove same edge to same edge entries
  edges <- edges[edges$value != 0, ]
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

###############################################################################
# OTHER FUNCTIONS
###############################################################################

StringQueryToVector <- function(query.string) {
  query.c <- unlist(strsplit(query.string, " OR "))
  query.c <- unlist(strsplit(query.c, ", "))
  query.c <- unlist(strsplit(query.c, " "))
  return(query.c)
}