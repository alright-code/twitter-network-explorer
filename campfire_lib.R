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
  
  # Default serverValues variables
  serverValues$current_edge_index <- 0
  serverValues$current_node_id <- 0
  serverValues$query.c <- default.query.c
  
  serverValues$data <- isolate(GetData(serverValues$query.c,
                                        500,
                                        FALSE))
  serverValues$wall <- isolate(WallUI(serverValues$data, serverValues$query.c))
  serverValues$edges <- isolate(GetEdges(serverValues$data, serverValues$query.c))
  serverValues$nodes <- isolate(GetNodes(serverValues$data, serverValues$query.c))
  serverValues$type <- "none"
  
  campfire_server <- shinyServer(function(input, output) {
    
    # Observe when update button is pressed, the read in data and update
    # corresponding areas
    observeEvent(input$update, {
      serverValues$query.c <- StringQueryToVector(serverValues$query)
      serverValues$data <- GetData(serverValues$query.c,
                                   serverValues$numberOfTweets,
                                   FALSE)
      serverValues$wall <- WallUI(serverValues$data, serverValues$query.c)
      serverValues$edges <- GetEdges(serverValues$data, serverValues$query.c)
      serverValues$nodes <- GetNodes(serverValues$data, serverValues$query.c)
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
  
    observe({
      for (inputId in names(input)) {
        serverValues[[inputId]] <- input[[inputId]]
      }
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