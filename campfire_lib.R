# Set twitter token, consumer_key and consumer_secret stored in token_info.R file
source("token_info.R")
token <- get_bearer_token(consumer_key, consumer_secret)

# Default search queries for startup. Used for the controller text box, default value for queries_string.
default_queries <- paste(c("#DataScience",
                           "#DataAnalytics",
                           "#DataAnalysis",
                           "#MachineLearning",
                           "#DeepLearning",
                           "#BigData",
                           "#data",
                           "#Programming",
                           "#Math",
                           "#rstats"),
                         collapse = " ")

# MW Shiny ----------------------------------------------------------------

campfireApp = function(controller = NA, wall = NA, floor = NA, datamonitor = NA,
                       urlmonitor = NA, serverFunct = NA) {
  
  ui <- campfireUI(controller, wall, floor, datamonitor, urlmonitor)
  
  # MW Shiny central reactive values. initialized makes sure default search is done on startup.
  serverValues <- reactiveValues(initialized = FALSE,
                                 data_subset = NULL,
                                 type = "none",
                                 current_node_id = -1,
                                 current_node_index = -1)
  
  campfire_server <- shinyServer(function(input, output, session) {
    
    updateValues <- reactive({
      # Updates the central serverValues with the input values from a SPECIFIC window.
      # Whatever domain calls this will update its input values with serverValues.
      for(inputId in names(input)) {
        serverValues[[inputId]] <- input[[inputId]]
      }
    })
    
    updateComplete <- reactive({
      # Gets tweet data and updates entire wall and entire floor with updated data.
      # Load bar will default to the specific window domain this is called in. 
      # We keep track of the monitor domain so the load bar will have priority there if it is open.
      if(is.null(serverValues$monitor.domain)) {
        d <- getDefaultReactiveDomain()
      } else {
        d <- serverValues$monitor.domain
      }
      withProgress(message = "Reloading...", value = 0, session = d, {
        incProgress(0, detail = "Getting Tweets", session = d)
        serverValues$data <- getData(serverValues$queries,
                                     serverValues$number_tweets,
                                     FALSE,
                                     token,
                                     serverValues$search_type)
        incProgress(1/3, detail = "Generating Wall", session = d)
        serverValues$nodes <- getNodes(serverValues$data, serverValues$queries)
        serverValues$edges <- getEdges(serverValues$data, serverValues$nodes$id)
        serverValues$col_list <- updateWall(serverValues$data, serverValues$nodes)
        #print(serverValues$nodes)
        incProgress(1/3, detail = "Generating Graph", session = d)
        serverValues$current_node_id = -1
        serverValues$current_edge_index = -1
      })
    })
    
    updateFromController <- reactive({
      # Use the text box input from the controller to replace the current query and update.
      updateValues()
      serverValues$queries <- StringQueryToVector(serverValues$queries_string)
      updateComplete()
    })
    
    # Get default data on startup.
    isolate({
      if(!serverValues$initialized) {
        updateFromController()
        serverValues$initialized <- TRUE
      }
    })
    
    observeEvent(input$update, {
      # Update the application from the controller text box queries when the update button is pressed.
      #
      # Event:
      #   Controller update button is pressed
      updateFromController()
    })
    
    observeEvent(input$file, {
      # Change queries to the input of a text file.
      #
      # Event:
      #   Text file is chosen on controller. 
      updateValues()
      serverValues$queries <- read.table(serverValues$file$datapath, header = FALSE,
                         comment.char = "", stringsAsFactors = FALSE)$V1
    })
    
    # Actions to be taken when edge or node selection is changed
    observeEvent(c(
      input$current_node_id,
      input$current_edge_index
      ), {
        # Update the information on the external monitor.
        #
        # Event:
        #   Selected node id is changed
        #   Selected edge index is changed
        updateValues()
        # When neither an edge or node is selected 
        if(serverValues$current_node_id == -1 && serverValues$current_edge_index == -1){
          serverValues$data_subset <- NULL
        # When node is selected
        } else if(serverValues$current_node_id != -1) {
          query <- serverValues$current_node_id
          serverValues$data_subset <- getDataSubset(serverValues$data, query)
        # When edge is selected
        } else if(serverValues$current_edge_index != -1) {
          edge <- serverValues$edges[serverValues$edges$index == serverValues$current_edge_index, ]
          query <- c(as.character(edge$to), as.character(edge$from))
          serverValues$data_subset <- getDataSubset(serverValues$data, query)
        } 
      })
    
    # Observe when a node is chosen to be deleted after a doubleclick, the
    # remove the data associated
    observeEvent(input$delete_node, {
      # Update the data when a node is deleted.
      #
      # Event:
      #   Node is double clicked on the floor
      updateValues()
      index <- which(serverValues$queries %in% serverValues$delete_node)
      serverValues$queries[index] <- NA
      serverValues$data_subset <- NULL
      serverValues$data <- serverValues$data %>%
                             filter(query != serverValues$delete_node)
      serverValues$col_list <- UpdateWall(serverValues$data, serverValues$queries)
    })
    
    # Observe when text on the wall is clicked, and update query and wall/floor
    observeEvent(input$clicked_text, {
      # Determine what was clicked on the wall and update the appropriate values.
      #
      # Event:
      #   Text is clicked on the wall.
      updateValues()
      if(substr(serverValues$clicked_text, 1, 1) == "#" ||  substr(serverValues$clicked_text, 1, 1) == "@") {
        if(toupper(serverValues$clicked_text) %in% toupper(serverValues$queries)) {
          index <- which(toupper(serverValues$queries) %in% toupper(serverValues$clicked_text))
          text <- serverValues$queries[index]
          serverValues$current_node_id <- text
          serverValues$data_subset <- getDataSubset(serverValues$data, text)
          serverValues$current_node_id = -1
          serverValues$current_edge_index = -1
        } else {
          index <- which(is.na(serverValues$queries))[1]
          if(!is.na(index)) {
            serverValues$queries[[index]] <- serverValues$clicked_text
            updateComplete()
          }
        }
      } else {
        serverValues$url <- input$clicked_text
      }
    })
    
    observeEvent(input$end_position, {
      # Change column positions based on moved nodes.
      #
      # Event:
      #   Node is released from drag on the foor.
      updateValues()
      angle <- cart2pol(serverValues$end_position[[1]]$x, -serverValues$end_position[[1]]$y)$theta
      angles <- rev(seq(0, (3/2)*pi, (2 * pi)/12))
      angles <- c(angles, seq((3/2)*pi, 2*pi, (2 * pi)/12)[3:2], 2*pi)
      # Find the closest angle value to the newly calculated
      to_index <- which(abs(angles - angle) == min(abs(angles - angle)))
      # If angle is close to 2pi, set the index to 10
      if(to_index == 13) {
        to_index <- 10
      }
      # Store old values to move the old node
      to_node <- serverValues$queries[[to_index]]
      from_index <- which(serverValues$queries %in% serverValues$current_node_id)
      to_col <- serverValues$col_list[[to_index]]
      start_distance <- ((serverValues$start_position[[1]]$x)^2 + (serverValues$start_position[[1]]$y)^2)^.5
      end_distance <- ((serverValues$end_position[[1]]$x)^2 + (serverValues$end_position[[1]]$y)^2)^.5
      if(start_distance < 187 && end_distance >= 187) {
        # When we move a node from the center to an edge
        visNetworkProxy("network") %>%
          visMoveNode(to_node, 0, 0)
        serverValues$queries[[to_index]] <- serverValues$current_node_id
        serverValues$queries[[from_index]] <- to_node
        serverValues$col_list[[to_index]] <- UpdateColumn(
          getDataSubset(serverValues$data, serverValues$current_node_id),
          serverValues$queries,
          to_index) 
        
      } else if(start_distance >= 187 && end_distance < 187) {
        # When we move a node from the edge to the center 
        serverValues$queries <- c(serverValues$queries, serverValues$current_node_id)
        serverValues$queries[[from_index]] <- NA
        serverValues$col_list[[from_index]] <- column(width = 1,
                                                     textInput(paste0("text.column.", from_index), label = ""),
                                                     actionButton(paste0("button.column.", from_index), "Submit"))
    
        
        
      # Normal movement, when both nodes are on the edge  
      } else if(start_distance >= 187 && end_distance >= 187) {
        # Change the position of the node moved onto
        if(from_index != new_index) {
          visNetworkProxy("network") %>%
            visMoveNode(to_node, serverValues$start_position[[1]]$x, serverValues$start_position[[1]]$y)
          serverValues$queries[to_index] <- serverValues$current_node_id
          serverValues$queries[from_index] <- to_node
          serverValues$col_list[[to_index]] <- serverValues$col_list[[from_index]]
          serverValues$col_list[[from_index]] <- to_col
        }
        
      # Move two nodes in the center  
      } else {
        # Do nothing
      }
      
      
    })
    
    # Observe all wall buttons, then update query and wall/floor
    observeEvent({
      input$button.column.1
    }, {
      updateValues()
      serverValues$queries[1] <- serverValues$text.column.1
      updateComplete()
    })
    observeEvent({
      input$button.column.2
    }, {
      updateValues()
      serverValues$queries[2] <- serverValues$text.column.2
      updateComplete()
    })
    observeEvent({
      input$button.column.3
    }, {
      updateValues()
      serverValues$queries[3] <- serverValues$text.column.3
      updateComplete()
    })
    observeEvent({
      input$button.column.4
    }, {
      updateValues()
      serverValues$queries[4] <- serverValues$text.column.4
      updateComplete()
    })
    observeEvent({
      input$button.column.5
    }, {
      updateValues()
      serverValues$queries[5] <- serverValues$text.column.5
      updateComplete()
    })
    observeEvent({
      input$button.column.6
    }, {
      updateValues()
      serverValues$queries[6] <- serverValues$text.column.6
      updateComplete()
    })
    observeEvent({
      input$button.column.7
    }, {
      updateValues()
      serverValues$queries[7] <- serverValues$text.column.7
      updateComplete()
    })
    observeEvent({
      input$button.column.8
    }, {
      updateValues()
      serverValues$queries[8] <- serverValues$text.column.8
      updateComplete()
    })
    observeEvent({
      input$button.column.9
    }, {
      updateValues()
      serverValues$queries[9] <- serverValues$text.column.9
      updateComplete()
    })
    observeEvent({
      input$button.column.10
    }, {
      updateValues()
      serverValues$queries[10] <- serverValues$text.column.10
      updateComplete()
    })
    observeEvent({
      input$button.column.11
    }, {
      updateValues()
      serverValues$queries[11] <- serverValues$text.column.11
      updateComplete()
    })
    observeEvent({
      input$button.column.12
    }, {
      updateValues()
      serverValues$queries[[12]] <- serverValues$text.column.12
      updateComplete()
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