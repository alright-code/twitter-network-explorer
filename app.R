library(shiny)
library(visNetwork)
library(rtweet)
library(shinythemes)
library(tidyverse)
library(shinyjs)
library(ggplot2)

source("functions.R")
source("campfire_lib.R")

###Colors###
color.white <- "#f0f0f0"
color.blue <- "#1D8DEE"
color.back <- "#151E29"
color.offback <- "#1B2737"

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
  
  monitor = div(fluidPage(
    fluidRow(
      column(6,
             plotOutput("top.users.bar.extern")
             ),
      column(6,
             plotOutput("top.hashtags.bar.extern")
             )
    )),
    style="background: #151E29;"
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
      serverValues$tweets.info
    })
    
    output$wall.ui <- renderUI({
      serverValues$wall
    })
    
    output$top.users.bar.extern <- renderPlot({
      serverValues$data.subset %>% 
        count(screen_name) %>% 
        arrange(desc(n)) %>%
        slice(1:5) %>%
        ggplot(aes(reorder(screen_name, n), n)) + 
          geom_col(fill = "#1D8DEE", color = "#1D8DEE") + 
          coord_flip() + 
          labs(x = "Screen Name", y = "Tweets", title = "Top 5 Users") + 
          theme_dark() +
          theme(plot.background = element_rect(fill = "#151E29"), axis.text = element_text(colour = "#f0f0f0"), text = element_text(colour = "#1D8DEE"))
    })
    
    output$top.hashtags.bar.extern <- renderPlot({
      serverValues$data.subset %>%
        unnest(hashtags) %>%
        mutate(hashtags = toupper(hashtags)) %>%
        filter(!(paste("#", hashtags, sep = "") %in% toupper(serverValues$query.c))) %>%
        count(hashtags) %>%
        arrange(desc(n)) %>%
        slice(1:5) %>%
        ggplot(aes(reorder(hashtags, n), n)) +
          geom_col(fill = "#1D8DEE", color = "#1D8DEE") +
          coord_flip() +
          labs(x = "Hashtag", y = "Frequency", title = "Top 5 Hashtags") +
          theme_dark() +
          theme(plot.background = element_rect(fill = "#151E29"), axis.text = element_text(colour = "#f0f0f0"), text = element_text(colour = "#1D8DEE"))
    })
    
  }
)
