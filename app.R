library(shiny)
library(visNetwork)
library(rtweet)
library(shinythemes)
library(tidyverse)
library(shinyjs)

source("~/ShinyApps/TwitterBrowserMW/functions.R")
source("~/ShinyApps/TwitterBrowserMW/campfire_lib.R")

###############################################################################
# MULTIWINDOW SHINY
###############################################################################

campfireApp(
  
  controller = div(
    h1("Controller"),
    textAreaInput("query", "Hashtags", default.query.string, height = '200px'),
    sliderInput(inputId = "numberOfTweets",
                label = "Choose number of tweets for the search:",
                min = 50, max = 10000, value = 500),
    actionButton(inputId = "update",
                 label = "Update"),
    style="position: absolute; 
    top: 50%; left: 50%; 
    margin-right: -50%; 
    transform: translate(-50%, -50%)"
  ),
  
  wall = div(
    uiOutput("wall.ui"),
    style="background: #151E29;
           scroll: hidden;"
  ),
  
  floor = div(
    visNetworkOutput("network", width = "1920px", height = "1080px"),
    style="position: absolute; 
           top: 50%; left: 50%;
           margin-right: -50%; 
           transform: translate(-50%, -50%);
           background: #151E29;"
  ),
  
  monitor = div(
    h1("External Monitor"),
    htmlOutput("tweets.info"),
    style="position: absolute; 
           top: 50%; left: 50%;
           margin-right: -50%; 
           transform: translate(-50%, -50%);
           background: rgb(255, 255, 255);"
  ),
  
  serverFunct = function(serverValues, output) {
    
    output$network <- renderVisNetwork({
    if(!is.null(serverValues$nodes)) {
        visNetwork(serverValues$nodes, serverValues$edges) %>%
          visEdges(scaling = list("min" = 0), smooth = list("enabled" = TRUE)) %>%
          visNodes(scaling = list("min" = 0, "max" = 30)) %>%
          # After drawing the network, center on 0,0 to keep position
          # independant of node number
          visEvents(type = "once", afterDrawing = "function() {
            this.moveTo({
                          position: {
                            x: 0,
                            y: 0
                          },
                    scale: 1
            })}") %>%
          visPhysics(stabilization = FALSE, enabled = FALSE) %>%
          # visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE)) %>%
          # Define behavior when clicking on nodes or edges
          visEvents(selectEdge = "function(properties) {
                                  Shiny.onInputChange('current_edge_index', this.body.data.edges.get(properties.edges[0]).index);
                                  }",
                  selectNode = "function(properties) {
                                Shiny.onInputChange('current_node_id', this.body.data.nodes.get(properties.nodes[0]).id);
                                }",
                  deselectEdge = "function() {
                                  Shiny.onInputChange('current_edge_index', 0);
                                  }",
                  deselectNode = "function() {
                                  Shiny.onInputChange('current_node_id', 0);
                                  }")
      }
    })
    
    output$tweets.info <- renderUI({
      serverValues$tweets.info
    })
    
    output$wall.ui <- renderUI({
      serverValues$wall
    })
    
  }
)
