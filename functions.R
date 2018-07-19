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
  color[1:nodes.rows] <- "#1D8DEE"
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
  angles <- head(seq(0, 2 * pi, (2 * pi)/12), -1 + remove)
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
    intersect(toupper(paste('#', x, sep = '')), toupper(query.c))
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
  color[1:edges.rows] <- "#f0f0f0"
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

UpdateWall <- function(data, query.c) {
  fluidRow(
    lapply(c(10:1,12,11), function(x) {
      if(x > length(query.c)) {
        column(width = 1,
               offset = 0)
      } else {
        data.subset <- GetDataSubset(data, query.c[[x]])
        column(width = 1,
               offset = 0,
               tags$div(
                 style = 'height: 780px;
                 overflow-y: auto;
                 overflow-x: hidden;',
                 includeCSS("~/ShinyApps/TwitterBrowserMW/wall.css"),
                 tags$h2(query.c[[x]]),
                 if(nrow(data.subset) > 0) {
                   lapply(1:nrow(data.subset), function(y) {
                     colored.text <- ColorHashtags(data.subset$text[[y]], query.c)
                     tags$div(style = 'border: 2px solid #000000',
                              tags$h3(paste("@", data.subset$screen_name[[y]], sep = "")),
                              tags$p(HTML(colored.text)),
                              tags$header(
                                tags$h3("Favorites:"),
                                tags$span(data.subset$favorite_count[[y]])
                              ),
                              tags$header(
                                tags$h3("Retweets:"),
                                tags$span(data.subset$retweet_count[[y]])
                              )
                     )
                   })
                 }
               )
        )
      }
    })
  )
}

# Color the hashtags in a string using HTML
ColorHashtags <- function(string, query.c) {
  string.c <- unlist(strsplit(string, "[ (\n)]"))
  hashtag.indices <- grep("#", string.c)
  colored.string.c <- lapply(1:length(string.c), function(x) {
    if(x %in% hashtag.indices) {
      if(toupper(string.c[[x]]) %in% toupper(query.c)) {
        paste('<font color="#1D8DEE">', string.c[[x]], '</font>')
      } else {
        paste('<font color="#cc6666">', string.c[[x]], '</font>')  
      }
    } else {
      string.c[[x]]
    }
  })
  colored.string <- paste(colored.string.c, collapse = " ")
  return(colored.string)
}