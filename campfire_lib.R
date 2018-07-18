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
    
    UpdateWall <- reactive({
      serverValues$wall <- fluidRow(
        lapply(c(10:1,12,11), function(x) {
          if(x > length(serverValues$query.c)) {
            column(width = 1,
                   offset = 0)
          } else {
            data.subset <- GetDataSubset(serverValues$data, serverValues$query.c[[x]])
            column(width = 1,
                   offset = 0,
                   tags$div(
                     style = 'height: 780px;
                     overflow-y: auto;
                     overflow-x: hidden;',
                     includeCSS("wall.css"),
                     tags$h2(serverValues$query.c[[x]]),
                     if(nrow(data.subset) > 0) {
                       lapply(1:nrow(data.subset), function(y) {
                         tags$div(style = 'border: 2px solid #000000',
                                  tags$h3(paste("@", data.subset$screen_name[[y]], sep = "")),
                                  tags$p(data.subset$text[[y]]),
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
    })
    
    UpdateFloor <- reactive({
      serverValues$edges <- GetEdges(serverValues$data, serverValues$query.c)
      serverValues$nodes <- GetNodes(serverValues$data, serverValues$query.c)
      serverValues$type <- "none"
    })
    
    observeEvent(input$update, {
      serverValues$query.c <- default.query.c
      serverValues$data <- GetData(serverValues$query.c,
                                   serverValues$numberOfTweets,
                                   FALSE)
      UpdateWall()
      UpdateFloor()
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