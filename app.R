library(shiny)
library(visNetwork)
library(rtweet)
library(tidyverse)
library(shinyjs)
library(ggplot2)

###Colors###
color.green <- "#1dee7e"
color.pink <- "#ee1d8d"
color.orange <- "#ee7e1d"
color.white <- "#f0f0f0"
color.blue <- "#1D8DEE"
color.back <- "#151E29"
color.offback <- "#1B2737"

source("functions.R")
source("wall.R")
source("campfire_lib.R")

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
    style = "position: absolute; 
    top: 50%; left: 50%; 
    margin-right: -50%; 
    transform: translate(-50%, -50%)"
  ),
  
  wall = div(
    uiOutput("wall.ui"),
    style = paste("background: ", color.back, "; scroll: hidden;",
                 sep = "")
  ),
  
  floor = div(
    visNetworkOutput("network", width = "1920px", height = "1080px"),
    style = paste("position: absolute; 
           top: 50%; left: 50%;
           margin-right: -50%; 
           transform: translate(-50%, -50%);
           background: ", color.back, ";", sep = "")
  ),
  
  monitor = div(fluidPage(
    fluidRow(
      column(6,
             plotOutput("top.users.bar.extern", height = "1010px")
             ),
      column(6,
             plotOutput("top.hashtags.bar.extern", height = "1010px")
             )
    ),
    fluidRow(
      column(12,
             uiOutput("tweets.info")
             )
    )),
    style = paste("background: ", color.back, ";", sep = "")
  ),
  
  serverFunct = function(serverValues, output) {
    
    output$network <- renderVisNetwork({
    if(!is.null(serverValues$nodes)) {
        visNetwork(serverValues$nodes, serverValues$edges) %>%
          visEdges(scaling = list("min" = 0), smooth = list("enabled" = TRUE)) %>%
          visNodes(scaling = list("min" = 10, "max" = 50)) %>%
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
      # Stuff to print when node is selected
      if(serverValues$type == "node") {
        node.name <- serverValues$current_node_id
        node.size <- serverValues$nodes$value[serverValues$nodes$id == serverValues$current_node_id]
        str1 <- paste("<font color=", color.white, "> Current Node: ", node.name, "</font>", sep = "")
        str2 <- paste("<font color=", color.white, "> Node Size: ", node.size, "</font>", sep = "")
        HTML(paste(str1, str2, sep = '<br/>'))
      }
      # Stuff to print when edge is selected
      # Percent Commonality
      else if(serverValues$type == "edge") {
        edge <- serverValues$edges[serverValues$edges$index == serverValues$current_edge_index, ]
        query <- c(as.character(edge$to), as.character(edge$from))
        edge.name <- paste(query, collapse = " AND ")
        edge.size <- serverValues$edges$value[serverValues$edges$index == serverValues$current_edge_index]
        str1 <- paste("<font color=", color.white, "> Current Edge: ", edge.name, "</font>", sep = "")
        str2 <- paste("<font color=", color.white, "> Edge Size: ", edge.size, "</font>", sep = "")
        HTML(paste(str1, str2, sep = '<br/>'))
      }
      # Stuff to print when nothing is selected
      else if(serverValues$type == "none") {
        num.tweets.found <- nrow(serverValues$data)
        str1 <- paste("<font color=", color.white, "> Total number of tweets found: ", num.tweets.found, "</font>", sep = "")
        str2 <- "placeholder"
        HTML(paste(str1, str2, sep = '<br/>'))
      }
    })
    
    output$wall.ui <- renderUI({
      serverValues$wall
    })
    
    output$top.users.bar.extern <- renderPlot({
      if(!is.null(serverValues$data.subset)) {
        serverValues$data.subset %>% 
          count(screen_name) %>% 
          arrange(desc(n)) %>%
          slice(1:5) %>%
          ggplot(aes(reorder(screen_name, n), n)) + 
          geom_col(fill = color.blue, color = color.blue) + 
          coord_flip() + 
          labs(x = "Screen Name", y = "Tweets", title = "Top 5 Users") + 
          theme_dark() +
          theme(plot.background = element_rect(fill = color.back), axis.text = element_text(colour = "#f0f0f0"), text = element_text(colour = "#1D8DEE"))
      }
    
    })
    
    output$top.hashtags.bar.extern <- renderPlot({
      if(!is.null(serverValues$data.subset)) {
        serverValues$data.subset %>%
          unnest(hashtags) %>%
          mutate(hashtags = toupper(hashtags)) %>%
          filter(!(paste("#", hashtags, sep = "") %in% toupper(serverValues$query.c))) %>%
          count(hashtags) %>%
          arrange(desc(n)) %>%
          slice(1:5) %>%
          ggplot(aes(reorder(hashtags, n), n)) +
          geom_col(fill = color.blue, color = color.blue) +
          coord_flip() +
          labs(x = "Hashtag", y = "Frequency", title = "Top 5 Hashtags") +
          theme_dark() +
          theme(panel.border = element_blank(), plot.background = element_rect(fill = color.back), axis.text = element_text(colour = "#f0f0f0"), text = element_text(colour = "#1D8DEE"))
      }
      
    })
    
  }
)
