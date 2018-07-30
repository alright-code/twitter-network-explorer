###############################################################################
# SHINY DEFAULTS 
###############################################################################

# Default hashtags
default.query.c <- c("#DataScience", "#DataAnalytics",
                     "#DataAnalysis", "#MachineLearning",
                     "#DeepLearning", "#BigData", "#data",
                     "#Programming", "#Math", "#rstats")

health.query <- c("#HealthTech", "#Healthcare", "#DataScience", "#Bigdata",
                  "#AI", "#Health", "#Fitness", "#MachineLearning", "#IOT",
                  "#Nutrition", "#Blockchain", "#Fit")

default.query.string <- paste(health.query, collapse = " ")

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
  
  # Default serverValues variables
  serverValues$initialized <- FALSE
  
  campfire_server <- shinyServer(function(input, output) {
    
    UpdateValues <- reactive({
      for (inputId in names(input)) {
        serverValues[[inputId]] <- input[[inputId]]
      }
    })
    
    # Use the controller query to pull information to completely update the app
    UpdateButton <- reactive({
      UpdateValues()
      serverValues$query.c <- StringQueryToVector(serverValues$query)
      data <- GetData(serverValues$query.c,
                                   serverValues$number.tweets,
                                   FALSE)
      serverValues$tweets.collected <- nrow(data)
      serverValues$all.subsets <- GetAllDataSubsets(data, serverValues$query.c)
      serverValues$col.list <- UpdateWall(serverValues$all.subsets, serverValues$query.c)
      serverValues$edges <- GetEdges(data, serverValues$query.c)
      serverValues$nodes <- GetNodes(data, serverValues$query.c)
    })
    
    # Get default data on startup
    isolate({
      if(serverValues$initialized == FALSE) {
        UpdateButton()
        serverValues$initialized <- TRUE
      }
    })
    
    # Observe when update button is pressed, the read in data and update
    # corresponding areas
    observeEvent(input$update, {
      UpdateButton()
    })
    
    # Actions to be taken when edge or node selection is changed
    observeEvent({
      input$current_node_id
      input$current_edge_index
      }, {
        UpdateValues()
        # When neither an edge or node is selected 
        if(serverValues$current_node_id == 0 && serverValues$current_edge_index == 0){
          serverValues$data.subset <- NULL
        # When edge is selected
        } else if(serverValues$current_node_id == 0) {
          edge <- serverValues$edges[serverValues$edges$index == serverValues$current_edge_index, ]
          node1 <- as.character(edge$to)
          node2 <- as.character(edge$from)
          subset1 <- serverValues$all.subsets[[node1]]$data
          subset2 <- serverValues$all.subsets[[node2]]$data
          serverValues$data.subset <- unique(merge(subset1, subset2))
        # When node is selected
        } else {
          query <- input$current_node_id
          serverValues$data.subset <- serverValues$all.subsets[[query]]$data
        } 
      })
    
    observeEvent(input$delete_node, {
      UpdateValues()
      node.index <- serverValues$all.subsets[[serverValues$delete_node]]$index
      serverValues$col.list[[node.index]] <- column(width = 1,
                                                    textInput(paste0("text.column.", node.index), node.index),
                                                    actionButton(paste0("button.column.", node.index), NULL))
      serverValues$all.subsets[[serverValues$delete_node]] <- NULL
      serverValues$data.subset <- NULL
      print(names(serverValues))
    })
    
    serverFunct(serverValues, output)
    
  })
  
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