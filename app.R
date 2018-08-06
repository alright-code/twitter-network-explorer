library(shiny)
library(visNetwork)
library(rtweet)
library(tidyverse)
library(shinyjs)
library(ggplot2)
library(shinycssloaders)

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
    sliderInput(inputId = "number.tweets",
                label = "Choose number of tweets for the search:",
                min = 50, max = 1000, value = 50),
    selectInput(inputId = "search.type",
                label = "Search Type:",
                choices = list("recent", "mixed", "popular")),
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
          visEvents(type = "once", beforeDrawing = "function() {
            this.moveTo({
                          position: {
                            x: 0,
                            y: 0
                          },
                    scale: 1
            })
            Shiny.onInputChange('current_node_id', 0);
            Shiny.onInputChange('current_edge_index', 0);
            Shiny.onInputChange('type', 'none');
          }") %>%
          visPhysics(stabilization = FALSE, enabled = FALSE) %>%
          # visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE)) %>%
          # Define behavior when clicking on nodes or edges
          visEvents(
                    click = "function(properties) {
                              if(this.getSelectedNodes().length == 1) {
                                Shiny.onInputChange('current_node_id', this.getSelectedNodes()[0]);
                                Shiny.onInputChange('type', 'node');
                              } else if(this.getSelectedEdges().length == 1) {
                                Shiny.onInputChange('current_edge_index', this.body.data.edges.get(properties.edges[0]).index);
                                Shiny.onInputChange('type', 'edge');
                              } else {
                                Shiny.onInputChange('type', 'none');
                              }
                            }",
                    doubleClick = "function(properties) {
                                     if(this.getSelectedNodes().length == 1) {
                                       Shiny.onInputChange('delete_node', this.getSelectedNodes()[0]);
                                       this.deleteSelected();
                                       Shiny.onInputChange('type', 'none');
                                     }
                                   }"
                  )
                  
      }
    })
    
    output$tweets.info <- renderUI({
      # Stuff to print when node is selected
      if(serverValues$type == "node") {
        node.name <- serverValues$current_node_id
        node.size <- nrow(serverValues$data.subset)
        str1 <- paste0("<font color=", color.white, "> Current Node: ", node.name, "</font>")
        str2 <- paste0("<font color=", color.white, "> Node Size: ", node.size, "</font>")
        HTML(paste(str1, str2, sep = '<br/>'))
      }
      # Stuff to print when edge is selected
      # Percent Commonality
      else if(serverValues$type == "edge") {
        edge <- serverValues$edges[serverValues$edges$index == serverValues$current_edge_index, ]
        query <- c(as.character(edge$to), as.character(edge$from))
        edge.name <- paste(query, collapse = " AND ")
        edge.size <- nrow(serverValues$data.subset)
        str1 <- paste0("<font color=", color.white, "> Current Edge: ", edge.name, "</font>")
        str2 <- paste0("<font color=", color.white, "> Edge Size: ", edge.size, "</font>")
        HTML(paste(str1, str2, sep = '<br/>'))
      }
      # Stuff to print when nothing is selected
      else if(serverValues$type == "none") {
        str1 <- paste0("<font color=", color.white, "> Total number of tweets found: ", nrow(serverValues$data), "</font>")
        str2 <- a(serverValues$url, href=serverValues$url, target="_blank")
        browseURL(serverValues$url)
        HTML(paste(str1, str2, sep = '<br/>'))
      } else if(serverValues$type == "load") {
        str1 <- paste0("<font color=", color.white, "> Loading New Tweets... ", "</font>")
        HTML(str1)
      }
    })
    
    output$wall.ui <- renderUI({
      fluidPage(
        fluidRow(
          tags$script(HTML(
            '$(document).on("click", ".clickable", function () {
              var text =  $(this).text();
              Shiny.onInputChange("clicked_text", text);
            });'
          )),
          lapply(1:12, function(col.num) {
            serverValues$col.list[[col.num]] 
          })
        )
      )
    })
    
    output$top.users.bar.extern <- renderPlot({
      if(!is.null(serverValues$data.subset)) {
        serverValues$data.subset %>% 
          count(screen_name) %>% 
          arrange(desc(n)) %>%
          slice(1:10) %>%
          ggplot(aes(reorder(screen_name, n), n)) + 
          geom_col(fill = color.blue, color = color.blue) + 
          coord_flip() + 
          labs(x = "Screen Name", y = "Tweets", title = "Top 10 Users") + 
          theme_dark() +
          theme(plot.background = element_rect(fill = color.back),
                axis.text = element_text(size = 20, colour = color.white),
                text = element_text(size = 20, colour = color.blue))
      }
    
    })
    
    output$top.hashtags.bar.extern <- renderPlot({
      if(!is.null(serverValues$data.subset)) {
        serverValues$data.subset %>%
          unnest(hashtags) %>%
          mutate(hashtags = toupper(hashtags)) %>%
          filter(!(paste("#", hashtags, sep = "") %in% toupper(unique(serverValues$data$query)))) %>%
          count(hashtags) %>%
          arrange(desc(n)) %>%
          slice(1:10) %>%
          ggplot(aes(reorder(hashtags, n), n)) +
            geom_col(fill = color.blue, color = color.blue) +
            coord_flip() +
            labs(x = "Hashtag", y = "Frequency", title = "Top 10 Hashtags") +
            theme_dark() +
            theme(panel.border = element_blank(),
                  plot.background = element_rect(fill = color.back),
                  axis.text = element_text(size = 20, colour = color.white),
                  text = element_text(size = 20, colour = color.blue))
      }
      
    })
    
  }
)
