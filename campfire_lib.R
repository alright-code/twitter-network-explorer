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
      query.c.nna <- serverValues$query.c[!is.na(serverValues$query.c)]
      serverValues$data <- GetData(query.c.nna,
                                   serverValues$number.tweets,
                                   FALSE, serverValues$search.type)
      serverValues$col.list <- UpdateWall(serverValues$data, serverValues$query.c)
      serverValues$edges <- GetEdges(serverValues$data, query.c.nna)
      serverValues$nodes <- GetNodes(serverValues$data, serverValues$query.c)
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
          query <- input$current_node_id
          serverValues$data.subset <- GetDataSubset(serverValues$data, query)
        } 
      })
    
    observeEvent(input$delete_node, {
      UpdateValues()
      node.index <- which(serverValues$query.c %in% input$delete_node)
      serverValues$col.list[[node.index]] <- column(width = 1,
                                                    textInput(paste0("text.column.", node.index), node.index),
                                                    actionButton(paste0("button.column.", node.index), NULL))
      serverValues$query.c[[node.index]] <- NA
      serverValues$data.subset <- NULL
    })
    
    # Yikes
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