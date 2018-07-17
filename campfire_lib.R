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
# EXTERNAL FUNCTIONS
###############################################################################

StringQueryToVector <- function(query.string) {
  query.c <- unlist(strsplit(query.string, " OR "))
  query.c <- unlist(strsplit(query.c, ", "))
  query.c <- unlist(strsplit(query.c, " "))
  return(query.c)
}

###############################################################################
# SHINY DEFAULTS 
###############################################################################

# Default hashtags
default.query.c <- c("#DataScience", "#DataAnalytics",
                     "#DataAnalysis", "#MachineLearning",
                     "#DeepLearning", "#BigData", "#data",
                     "#Programming", "#Math", "#rstats")

default.query.string <- paste(default.query.c, collapse = " ")

token <- create_token(
  app = "Campfire_Twitter",
  consumer_key = "YysJwhUvIYyEZsOqLtE5mlsHv",
  consumer_secret = "EC997cOOlCzp4qedKVaYCB2zs8HnPyKEhIqLV2PNKCirXartcE",
  access_token = "3038115538-sDa3AOf3UwkvWM9g9REitniqw9L8EZxJFza8qj4",
  access_secret = "qRZja6LSKA2rm7mVBpbNGQKKmBlCKbsXmbL3TmmvEoPp2"
)

###############################################################################
# MULTIWINDOW SHINY
###############################################################################

campfireApp = function(controller = NA, wall = NA, floor = NA, monitor=NA, serverFunct = NA) {
  ui <- campfireUI(controller, wall, floor, monitor)
  
  serverValues <- reactiveValues()
  
  serverValues$type <- "prescreen"
  serverValues$current_edge_index <- 0
  serverValues$current_node_id <- 0
  
  campfire_server <- shinyServer(function(input, output) {
    
    observeEvent(input$update, {
      # Create a vector using the string query entered, read 
      query.c <- StringQueryToVector(input$query)
      serverValues$data <- GetData(query.c,
                      serverValues$numberOfTweets,
                      FALSE)
      serverValues$edges <- GetEdges(serverValues$data, query.c)
      serverValues$nodes <- GetNodes(serverValues$data, query.c)
      serverValues$type <- "none"
    })
    
    # Actions to be taken when edge or node selection is changed
    observeEvent({
      input$current_node_id
      input$current_edge_index
      }, {
        # When neither an edge or node is selected 
        if(serverValues$current_node_id == 0 && serverValues$current_edge_index == 0){
          serverValues$type <- "none"
        # When edge is selected
        } else if(serverValues$current_node_id == 0) {
          serverValues$type <- "edge"
          edge <- serverValues$edges[serverValues$edges$index == input$current_edge_index, ]
          query <- c(as.character(edge$to), as.character(edge$from))
          serverValues$data.subset <- GetDataSubset(serverValues$data, query)
          # When node is selected
        } else {
          serverValues$type <- "node"
          query <- input$current_node_id
          serverValues$data.subset <- GetDataSubset(serverValues$data, query)
        } 
      })
    
    # Related Hashtags -- Ability to add them
    # Popularity
    # Engagement
    observe({
      # Stuff to print when node is selected
      if(serverValues$type == "node") {
        node.name <- serverValues$current_node_id
        node.size <- serverValues$nodes$value[serverValues$nodes$id == serverValues$current_node_id]
        # node.total.favs <- 
        # node.favs.per.tweet <- 
        str1 <- paste("Current Node:", node.name)
        str2 <- paste("Size of Node:", node.size)
        serverValues$tweets.info <- HTML(paste(str1, str2, sep = '<br/>'))
      }
      # Stuff to print when edge is selected
      # Percent Commonality
      else if(serverValues$type == "edge") {
        edge <- serverValues$edges[serverValues$edges$index == serverValues$current_edge_index, ]
        query <- c(as.character(edge$to), as.character(edge$from))
        edge.name <- paste(query, collapse = " AND ")
        edge.size <- serverValues$edges$value[serverValues$edges$index == serverValues$current_edge_index]
        str1 <- paste("Current Edge:", edge.name)
        str2 <- paste("Size of Edge:", edge.size)
        serverValues$tweets.info <- HTML(paste(str1, str2, sep = '<br/>'))
      }
      # Stuff to print when nothing is selected
      else if(serverValues$type == "none") {
        num.tweets.found <- nrow(serverValues$data)
        str1 <- paste("Total number of tweets found:", num.tweets.found)
        str2 <- "placeholder"
        serverValues$tweets.info <- HTML(paste(str1, str2, sep = '<br/>'))
      }
      # Stuff to print before a network is created
      else if(serverValues$type == "prescreen") {
        str1 <- "Welcome to the Campfire Twitter Network Explorer!"
        serverValues$tweets.info <- HTML(str1)
      }
    })
    
    observeEvent(input$update, {
      query.c <- StringQueryToVector(input$query)
      serverValues$wall <- fluidRow(
        lapply(c(10:1,12,11), function(x) {
          if(x > length(query.c)) {
            column(width = 1, offset = 0)
          } else {
            data.subset <- GetDataSubset(serverValues$data, query.c[[x]])
            column(width = 1, offset = 0,
                   div(
                     style = 'height: 780px;
                              overflow-y: auto;
                              overflow-x: hidden;',
                     tags$h2(query.c[[x]]),
                     tags$head(tags$style(HTML('
                                              h2 {
                                                color: #1D8DEE;
                                              }
                                              h3 {
                                                color: #1D8DEE;
                                              }
                                              h5 {
                                                color: #1D8DEE;
                                              }
                                              body {
                                                color: #f0f0f0;
                                              }
                                             '))),
                     lapply(1:nrow(data.subset), function(y) {
                       div(
                         style = 'border: 1px solid #1B2737',
                         tags$h5("Author:"),
                         tags$body(HTML(data.subset$screen_name[[y]])),
                         tags$h5("Tweet:"),
                         tags$body(HTML(data.subset$text[[y]])),
                         tags$h5("Favorites:"),
                         tags$body(HTML(data.subset$favorite_count[[y]])),
                         tags$h5("Retweets:"),
                         tags$body(HTML(data.subset$retweet_count[[y]]))
                       )
                     })
                   )
            )
          }
        })
      ) 
    })
  
    observe({
      for (inputId in names(input)) {
        serverValues[[inputId]] <- input[[inputId]]
      }
    })
    
    serverFunct(serverValues, output)
    
  })
  
  options(shiny.port = 5480)
  shinyApp(ui, server = campfire_server)
}

campfireUI = function(controller, wall, floor, monitor) {
  ui <- shinyUI(bootstrapPage(
    HTML('<script type="text/javascript">
         $(function() {
         $("div.Window").hide(); 
         var tokens = window.location.href.split("?");
         if (tokens.length > 1) {
         var shown_window = tokens[1];
         $("div."+shown_window).show();
         } else {
         $("div.WindowSelector").show();
         }
         });
         </script>'),
    div(class="WindowSelector Window",
        HTML('<h2><a href="?Controller">Controller</a></h2>'),
        HTML('<h2><a href="?Wall">Wall</a></h2>'),
        HTML('<h2><a href="?Floor">Floor</a></h2>'),
        HTML('<h2><a href="?Monitor">External Monitor</a></h2>'),
        style='position: absolute; 
        top: 50%; left: 50%; 
        margin-right: -50%; 
        transform: translate(-50%, -50%)'
    ),
    div(class="Controller Window",
        controller
    ),
    div(class="Wall Window",
        wall 
    ),
    div(class="Floor Window",
        floor
    ),
    div(class="Monitor Window",
        monitor
    )
    
    ))
  
  return(ui)
}