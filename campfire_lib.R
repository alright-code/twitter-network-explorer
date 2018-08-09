
# Defaults ----------------------------------------------------------------

# Default hashtags
default.query.c <- c("#DataScience", "#DataAnalytics",
                     "#DataAnalysis", "#MachineLearning",
                     "#DeepLearning", "#BigData", "#data",
                     "#Programming", "#Math", "#rstats")

health.query <- c("#HealthTech", "#Healthcare", "#DataScience", "#Bigdata",
                  "#AI", "#Health", "#Fitness", "#MachineLearning", "#IOT",
                  "#Nutrition", "#Blockchain")

default.query.string <- paste(default.query.c, collapse = " ")

token <- create_token(
  app = "Campfire_Twitter",
  consumer_key = "YysJwhUvIYyEZsOqLtE5mlsHv",
  consumer_secret = "EC997cOOlCzp4qedKVaYCB2zs8HnPyKEhIqLV2PNKCirXartcE",
  access_token = "3038115538-sDa3AOf3UwkvWM9g9REitniqw9L8EZxJFza8qj4",
  access_secret = "qRZja6LSKA2rm7mVBpbNGQKKmBlCKbsXmbL3TmmvEoPp2",
  FALSE
)


# MW Shiny ----------------------------------------------------------------

campfireApp = function(controller = NA, wall = NA, floor = NA, datamonitor = NA, urlmonitor = NA, serverFunct = NA) {
  ui <- campfireUI(controller, wall, floor, datamonitor, urlmonitor)
  
  serverValues <- reactiveValues()
  
  # Default serverValues variables
  serverValues$initialized <- FALSE
  
  campfire_server <- shinyServer(function(input, output, session) {
    
    UpdateValues <- reactive({
      for(inputId in names(input)) {
        serverValues[[inputId]] <- input[[inputId]]
      }
    })
    
    # Use the controller query to pull information to completely update the app
    UpdateButton <- reactive({
      withProgress(message = "Collecting Tweets", value = 0, {
        query.c.nna <- serverValues$query.c[!is.na(serverValues$query.c)]
        serverValues$data <- GetData(query.c.nna,
                                     serverValues$number.tweets,
                                     FALSE, serverValues$search.type)
        serverValues$col.list <- UpdateWall(serverValues$data, serverValues$query.c)
        serverValues$edges <- GetEdges(serverValues$data, query.c.nna)
        serverValues$nodes <- GetNodes(serverValues$data, serverValues$query.c)
        serverValues$type <- "none"
      })
    })
    
    # Get default data on startup
    isolate({
      if(serverValues$initialized == FALSE) {
        UpdateValues()
        serverValues$query.c <- StringQueryToVector(serverValues$query)
        UpdateButton()
        serverValues$initialized <- TRUE
      }
    })
    
    # Observe when update button is pressed, the read in data and update
    # corresponding areas
    observeEvent(input$update, {
      UpdateValues()
      serverValues$query.c <- StringQueryToVector(serverValues$query)
      UpdateButton()
    })
    
    # Actions to be taken when edge or node selection is changed
    observeEvent({
      input$current_node_id
      input$current_edge_index
      input$type
      }, {
        UpdateValues()
        # When neither an edge or node is selected 
        if(serverValues$type == "none"){
          serverValues$data.subset <- NULL
        # When edge is selected
        } else if(serverValues$type == "edge") {
          edge <- serverValues$edges[serverValues$edges$index == serverValues$current_edge_index, ]
          query <- c(as.character(edge$to), as.character(edge$from))
          serverValues$data.subset <- GetDataSubset(serverValues$data, query)
        # When node is selected
        } else if(serverValues$type == "node") {
          query <- serverValues$current_node_id
          serverValues$data.subset <- GetDataSubset(serverValues$data, query)
        } 
      })
    
    # Observe when a node is chosen to be deleted after a doubleclick, the
    # remove the data associated
    observeEvent(input$delete_node, {
      UpdateValues()
      index <- which(serverValues$query.c %in% serverValues$delete_node)
      serverValues$query.c[node.index] <- NA
      serverValues$data.subset <- NULL
      serverValues$col.list <- UpdateWall(serverValues$data, serverValues$query.c)
    })
    
    # Observe when text on the wall is clicked, and update query and wall/floor
    observeEvent(input$clicked_text, {
      UpdateValues()
      if(substr(serverValues$clicked_text, 1, 1) == "#" ||  substr(serverValues$clicked_text, 1, 1) == "@") {
        if(toupper(serverValues$clicked_text) %in% toupper(serverValues$query.c)) {
          index <- which(toupper(serverValues$query.c) %in% toupper(serverValues$clicked_text))
          text <- serverValues$query.c[index]
          visNetworkProxy("network", session = floor.domain) %>%
            visRemoveNodes(text)
          serverValues$query.c[index] <- NA
          serverValues$data.subset <- NULL
          serverValues$col.list <- UpdateWall(serverValues$data, serverValues$query.c)
        } else {
          index <- which(is.na(serverValues$query.c))[1]
          if(!is.na(index)) {
            serverValues$query.c[[index]] <- serverValues$clicked_text
            UpdateButton()
          }
        }
      } else {
        serverValues$url <- input$clicked_text
      }
    })
    
    # Observe when a drag event ends, and adjust columns on wall based on movement
    observeEvent(input$end_position, {
      UpdateValues()
      angle <- cart2pol(serverValues$end_position[[1]]$x, -serverValues$end_position[[1]]$y)$theta
      angles <- rev(seq(0, (3/2)*pi, (2 * pi)/12))
      angles <- c(angles, seq((3/2)*pi, 2*pi, (2 * pi)/12)[2:4])
      # Find the closest angle value to the newly calculated
      new.index <- which(abs(angles - angle) == min(abs(angles - angle)))
      # If angle is close to 2pi, set the index to 10
      if(new.index == 13) {
        new.index <- 10
      }
      # Store old values to move the old node
      tmp.node <- serverValues$query.c[new.index]
      tmp.index <- which(serverValues$query.c %in% serverValues$current_node_id)
      tmp.col <- serverValues$col.list[[new.index]]
      # Change the position of the node moved onto
      visNetworkProxy("network", session = floor.domain) %>%
        visMoveNode(tmp.node, serverValues$start_position[[1]]$x, serverValues$start_position[[1]]$y)
      serverValues$query.c[new.index] <- serverValues$current_node_id
      serverValues$query.c[tmp.index] <- tmp.node
      serverValues$col.list[[new.index]] <- serverValues$col.list[[tmp.index]]
      serverValues$col.list[[tmp.index]] <- tmp.col
    })
    
    # observeEvent(input$query.c, {
    #   print("ok")
    #   updateTextInput(session, "query", value = paste(serverValues$query.c[!is.na(serverValues$query.c)], collapse = " "))
    # })
    
    # Observe all wall buttons, then update query and wall/floor
    observeEvent({
      input$button.column.1
    }, {
      UpdateValues()
      serverValues$query.c[1] <- serverValues$text.column.1
      UpdateButton()
    })
    
    observeEvent({
      input$button.column.2
    }, {
      UpdateValues()
      serverValues$query.c[2] <- serverValues$text.column.2
      UpdateButton()
    })
    
    observeEvent({
      input$button.column.3
    }, {
      UpdateValues()
      serverValues$query.c[3] <- serverValues$text.column.3
      UpdateButton()
    })
    
    observeEvent({
      input$button.column.4
    }, {
      UpdateValues()
      serverValues$query.c[4] <- serverValues$text.column.4
      UpdateButton()
    })
    
    observeEvent({
      input$button.column.5
    }, {
      UpdateValues()
      serverValues$query.c[5] <- serverValues$text.column.5
      UpdateButton()
    })
    
    observeEvent({
      input$button.column.6
    }, {
      UpdateValues()
      serverValues$query.c[6] <- serverValues$text.column.6
      UpdateButton()
    })
    
    observeEvent({
      input$button.column.7
    }, {
      UpdateValues()
      serverValues$query.c[7] <- serverValues$text.column.7
      UpdateButton()
    })
    
    observeEvent({
      input$button.column.8
    }, {
      UpdateValues()
      serverValues$query.c[8] <- serverValues$text.column.8
      UpdateButton()
    })
    
    observeEvent({
      input$button.column.9
    }, {
      UpdateValues()
      serverValues$query.c[9] <- serverValues$text.column.9
      UpdateButton()
    })
    
    observeEvent({
      input$button.column.10
    }, {
      UpdateValues()
      serverValues$query.c[10] <- serverValues$text.column.10
      UpdateButton()
    })
    
    observeEvent({
      input$button.column.11
    }, {
      UpdateValues()
      serverValues$query.c[11] <- serverValues$text.column.11
      UpdateButton()
    })
    
    observeEvent({
      input$button.column.12
    }, {
      UpdateValues()
      serverValues$query.c[12] <- serverValues$text.column.12
      UpdateButton()
    })
    
    serverFunct(serverValues, output, session)
    
  })
  
  shinyApp(ui, server = campfire_server)
}

campfireUI = function(controller, wall, floor, datamonitor, urlmonitor) {
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
        HTML('<h2><a href="?DataMonitor">Data Monitor</a></h2>'),
        HTML('<h2><a href="?Monitor">URL Monitor</a></h2>'),
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
    div(class="DataMonitor Window",
        datamonitor
    ),
    div(class="Monitor Window",
        urlmonitor
    )
    
    ))
  
  return(ui)
}