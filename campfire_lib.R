# MW Shiny ----------------------------------------------------------------

campfireApp = function(controller = NA, wall = NA, floor = NA, datamonitor = NA, urlmonitor = NA, serverFunct = NA) {
  ui <- campfireUI(controller, wall, floor, datamonitor, urlmonitor)
  
  serverValues <- reactiveValues(initialized = FALSE)
  
  campfire_server <- shinyServer(function(input, output, session) {
    
    UpdateValues <- reactive({
      for(inputId in names(input)) {
        serverValues[[inputId]] <- input[[inputId]]
      }
    })
    
    # Use the controller query to pull information to completely update the app
    UpdateButton <- reactive({
      serverValues$data_subset <- NULL
      if(is.null(serverValues$monitor.domain)) {
        d <- getDefaultReactiveDomain()
      } else {
        d <- serverValues$monitor.domain
      }
      withProgress(message = "Reloading...", value = 0, session = d, {
        incProgress(0, detail = "Getting Tweets", session = d)
        serverValues$data <- getData(serverValues$query,
                                     serverValues$number_tweets,
                                     FALSE, serverValues$search_type)
        incProgress(1/3, detail = "Generating Wall", session = d)
        serverValues$col.list <- UpdateWall(serverValues$data, serverValues$query)
        serverValues$edges <- getEdges(serverValues$data, serverValues$query)
        serverValues$nodes <- getNodes(serverValues$data, serverValues$query)
        incProgress(1/3, detail = "Generating Graph", session = d)
        serverValues$type <- "none"
      })
    })
    
    # Get default data on startup
    isolate({
      if(serverValues$initialized == FALSE) {
        UpdateValues()
        serverValues$query <- StringQueryToVector(serverValues$query)
        UpdateButton()
        serverValues$initialized <- TRUE
      }
    })
    
    # Observe when update button is pressed, the read in data and update
    # corresponding areas
    observeEvent(input$update, {
      UpdateValues()
      serverValues$query <- StringQueryToVector(serverValues$query)
      UpdateButton()
    })
    
    observeEvent(input$file, {
      UpdateValues()
      text <- read.table(serverValues$file$datapath, header = FALSE,
                         comment.char = "", stringsAsFactors = FALSE)$V1
      serverValues$query <- text
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
          serverValues$data_subset <- NULL
        # When edge is selected
        } else if(serverValues$type == "edge") {
          edge <- serverValues$edges[serverValues$edges$index == serverValues$current_edge_index, ]
          query <- c(as.character(edge$to), as.character(edge$from))
          serverValues$data_subset <- getDataSubset(serverValues$data, query)
        # When node is selected
        } else if(serverValues$type == "node") {
          query <- serverValues$current_node_id
          serverValues$data_subset <- getDataSubset(serverValues$data, query)
        } 
      })
    
    # Observe when a node is chosen to be deleted after a doubleclick, the
    # remove the data associated
    observeEvent(input$delete_node, {
      UpdateValues()
      index <- which(serverValues$query %in% serverValues$delete_node)
      serverValues$query[index] <- NA
      serverValues$data_subset <- NULL
      serverValues$data <- serverValues$data %>%
                             filter(query != serverValues$delete_node)
      serverValues$col.list <- UpdateWall(serverValues$data, serverValues$query)
    })
    
    # Observe when text on the wall is clicked, and update query and wall/floor
    observeEvent(input$clicked_text, {
      UpdateValues()
      if(substr(serverValues$clicked_text, 1, 1) == "#" ||  substr(serverValues$clicked_text, 1, 1) == "@") {
        if(toupper(serverValues$clicked_text) %in% toupper(serverValues$query)) {
          index <- which(toupper(serverValues$query) %in% toupper(serverValues$clicked_text))
          text <- serverValues$query[index]
          serverValues$current_node_id <- text
          serverValues$data_subset <- getDataSubset(serverValues$data, text)
          serverValues$type <- "node"
        } else {
          index <- which(is.na(serverValues$query))[1]
          if(!is.na(index)) {
            serverValues$query[[index]] <- serverValues$clicked_text
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
      angles <- c(angles, seq((3/2)*pi, 2*pi, (2 * pi)/12)[3:2], 2*pi)
      # Find the closest angle value to the newly calculated
      new.index <- which(abs(angles - angle) == min(abs(angles - angle)))
      # If angle is close to 2pi, set the index to 10
      if(new.index == 13) {
        new.index <- 10
      }
      # Store old values to move the old node
      tmp.node <- serverValues$query[new.index]
      tmp.index <- which(serverValues$query %in% serverValues$current_node_id)
      tmp.col <- serverValues$col.list[[new.index]]
      start.distance <- ((serverValues$start_position[[1]]$x)^2 + (serverValues$start_position[[1]]$y)^2)^.5
      end.distance <- ((serverValues$end_position[[1]]$x)^2 + (serverValues$end_position[[1]]$y)^2)^.5
      # When we move a node from the center to an edge
      if(start.distance < 187 && end.distance >= 187) {
        #Normal 
        
      # When we move a node from the edge to the center    
      } else if(start.distance >= 187 && end.distance < 187) {
        # Abnormal: 
        
      # Normal movement, when both nodes are on the edge  
      } else if(start.distance >= 187 && end.distance >= 187) {
        
      # Move two nodes in the center  
      } else {
        # Do nothing
      }
      # Change the position of the node moved onto
      if(tmp.index != new.index) {
        visNetworkProxy("network") %>%
          visMoveNode(tmp.node, serverValues$start_position[[1]]$x, serverValues$start_position[[1]]$y)
        serverValues$query[new.index] <- serverValues$current_node_id
        serverValues$query[tmp.index] <- tmp.node
        serverValues$col.list[[new.index]] <- serverValues$col.list[[tmp.index]]
        serverValues$col.list[[tmp.index]] <- tmp.col
      }
    })
    
    # Observe all wall buttons, then update query and wall/floor
    observeEvent({
      input$button.column.1
    }, {
      UpdateValues()
      serverValues$query[1] <- serverValues$text.column.1
      UpdateButton()
    })
    
    observeEvent({
      input$button.column.2
    }, {
      UpdateValues()
      serverValues$query[2] <- serverValues$text.column.2
      UpdateButton()
    })
    
    observeEvent({
      input$button.column.3
    }, {
      UpdateValues()
      serverValues$query[3] <- serverValues$text.column.3
      UpdateButton()
    })
    
    observeEvent({
      input$button.column.4
    }, {
      UpdateValues()
      serverValues$query[4] <- serverValues$text.column.4
      UpdateButton()
    })
    
    observeEvent({
      input$button.column.5
    }, {
      UpdateValues()
      serverValues$query[5] <- serverValues$text.column.5
      UpdateButton()
    })
    
    observeEvent({
      input$button.column.6
    }, {
      UpdateValues()
      serverValues$query[6] <- serverValues$text.column.6
      UpdateButton()
    })
    
    observeEvent({
      input$button.column.7
    }, {
      UpdateValues()
      serverValues$query[7] <- serverValues$text.column.7
      UpdateButton()
    })
    
    observeEvent({
      input$button.column.8
    }, {
      UpdateValues()
      serverValues$query[8] <- serverValues$text.column.8
      UpdateButton()
    })
    
    observeEvent({
      input$button.column.9
    }, {
      UpdateValues()
      serverValues$query[9] <- serverValues$text.column.9
      UpdateButton()
    })
    
    observeEvent({
      input$button.column.10
    }, {
      UpdateValues()
      serverValues$query[10] <- serverValues$text.column.10
      UpdateButton()
    })
    
    observeEvent({
      input$button.column.11
    }, {
      UpdateValues()
      serverValues$query[11] <- serverValues$text.column.11
      UpdateButton()
    })
    
    observeEvent({
      input$button.column.12
    }, {
      UpdateValues()
      serverValues$query[12] <- serverValues$text.column.12
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
        HTML('<h2><a href="?Monitor">External Monitor</a></h2>'),
        HTML('<h2><a href="?URLMonitor">URL Monitor</a></h2>'),
        style = 'position: absolute; 
        top: 50%; left: 50%; 
        margin-right: -50%; 
        transform: translate(-50%, -50%)'
    ),
    div(class = "Controller Window",
        controller
    ),
    div(class = "Wall Window",
        wall 
    ),
    div(class = "Floor Window",
        floor
    ),
    div(class = "Monitor Window",
        datamonitor
    ),
    div(class = "URLMonitor Window",
        urlmonitor
    )
    
    ))
  
  return(ui)
}