library(shiny)
library(visNetwork)
library(rtweet)
library(tidyverse)
library(ggplot2)
library(useful)

#Explore Tweets
#Expand Network
#Build Network

###Colors###
color.green <- "#1dee7e"
color.pink <- "#ee1d8d"
color.orange <- "#ee7e1d"
color.white <- "#f0f0f0"
color.blue <- "#1D8DEE"
color.back <- "#151E29"
color.offback <- "#1B2737"
colors <- c("#1D8DEE", "#1dee7e", "#ee7e1d", "#ee1d8d", "#64B0F3", "#64F3A6", "#F3A664", "#B10D65", "#0D65B1", "#0DB159", "#B1590D", "#F364B0")

source("app-only-auth-twitter.R")
source("functions.R")
source("wall.R")
source("campfire_lib.R")

campfireApp(
  
  controller = div(
    h1("Controller"),
    textAreaInput("query", "Search Query", default.query.string, height = '200px'),
    fileInput("file", "Upload File", accept = c("text/plain")),
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
    style = paste0("background: ", color.back, "; overflow: hidden;",
                   "height: 665px")
  ),
  
  floor = div(
    visNetworkOutput("network", width = "1000px", height = "900px"),
    style = paste0("position: absolute; 
           top: 50%; left: 50%;
           margin-right: -50%; 
           transform: translate(-50%, -50%);
           background: ", color.back,
           "; height: 900px; overflow: hidden")
  ),
  
  datamonitor = div(fluidPage(
    fluidRow(
      column(12,
            uiOutput("tweets.info")
      )
    )),
    fluidRow(
      column(6,
             plotOutput("top.users.bar.extern", height = "920px")
             ),
      column(6,
             plotOutput("top.hashtags.bar.extern", height = "920px")
             )
    ),
    style = paste0("background: ", color.back, ";
                   overflow: hidden;
                   height: 1050px")
  ),
  
  urlmonitor = div(fluidPage(
    htmlOutput("frame")
  )),
  
  serverFunct = function(serverValues, output, session) {

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
            Shiny.onInputChange('type', 'none');
          }") %>%
          visPhysics(stabilization = FALSE, enabled = FALSE) %>%
          visInteraction(dragView = FALSE, zoomView = FALSE) %>%
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
                    doubleClick = "function() {
                                     if(this.getSelectedNodes().length == 1) {
                                       Shiny.onInputChange('delete_node', this.getSelectedNodes()[0]);
                                       this.deleteSelected();
                                       Shiny.onInputChange('type', 'none');
                                     }
                                   }",
                    dragStart = "function() {
                                 var sel = this.getSelectedNodes();
                                 if(sel.length == 1) {
                                   Shiny.onInputChange('current_node_id', this.getSelectedNodes()[0]);
                                   Shiny.onInputChange('type', 'node');
                                   Shiny.onInputChange('start_position', this.getPositions(sel[0]))
                                 }
                               }",
                    dragEnd = "function() {
                                 var sel = this.getSelectedNodes();
                                 if(sel.length == 1) {
                                   Shiny.onInputChange('end_position', this.getPositions(sel[0]))
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
        tags$div(
          tags$h1(style = paste0("color:", color.blue), node.name),
          tags$h2(style = paste0("color:", color.blue), paste("Size:", node.size))
        )
      # Stuff to print when edge is selected
      # Percent Commonality
      } else if(serverValues$type == "edge") {
        edge <- serverValues$edges[serverValues$edges$index == serverValues$current_edge_index, ]
        query <- c(as.character(edge$to), as.character(edge$from))
        edge.name <- paste(query, collapse = " AND ")
        edge.size <- nrow(serverValues$data.subset)
        tags$div(
          tags$h1(style = paste0("color:", color.blue), edge.name),
          tags$h2(style = paste0("color:", color.blue), paste("Size:", edge.size))
        )
      # Stuff to print when nothing is selected
      } else if(serverValues$type == "none") {
        tags$div(
          tags$h1(style = paste0("color:", color.blue), "Twitter Network Explorer"),
          tags$h2(style = paste0("color:", color.blue), paste("Total number of tweets found:", nrow(serverValues$data)))  
        )
      }
    })
    
    output$wall.ui <- renderUI({
      fluidPage(
        tags$script(HTML(
          "$(document).on('click', '.clickable', function () {
              var text =  $(this).text();
              Shiny.onInputChange('clicked_text', text);
            });"
        )),
        fluidRow(
          lapply(1:12, function(col.num) {
            serverValues$col.list[[col.num]] 
          })
        )
      )
    })
    
    output$top.users.bar.extern <- renderPlot({
      serverValues$monitor.domain <- getDefaultReactiveDomain()
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
            theme(plot.background = element_rect(fill = color.back, color = NA),
                  axis.text = element_text(size = 20, colour = color.white),
                  text = element_text(size = 20, colour = color.blue))
      } else {
        serverValues$data %>%
          count(query) %>%
          ggplot(aes(reorder(query, n), n)) +
            geom_col(fill = color.blue, color = color.blue) +
            coord_flip() +
            labs(x = "Query", y = "Number of Tweets", title = "Tweet Composition") +
            theme_dark() +
            theme(panel.border = element_blank(),
                plot.background = element_rect(fill = "#151E29", color = NA),
                axis.text = element_text(size = 20, colour = "#f0f0f0"),
                text = element_text(size = 20, colour = "#1D8DEE"))
      }
    })
    
    output$top.hashtags.bar.extern <- renderPlot({
      if(!is.null(serverValues$data.subset)) {
        serverValues$data.subset %>%
          filter(!is.na(hashtags)) %>%
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
                  plot.background = element_rect(fill = color.back, color = NA),
                  axis.text = element_text(size = 20, colour = color.white),
                  text = element_text(size = 20, colour = color.blue))
      } else {
        serverValues$data %>% 
          distinct(screen_name, source) %>%
          count(source) %>% 
          filter(n >= 5) %>%
          ggplot(aes(reorder(source, n), n)) + 
            geom_col(fill = color.blue, color = color.blue) +
            coord_flip() + 
            labs(x = "Source", y = "Tweets", title = "Tweets by source", subtitle = "sources with >=5 tweets") +
            theme_dark() +
            theme(panel.border = element_blank(),
                plot.background = element_rect(fill = color.back, color = NA),
                axis.text = element_text(size = 20, colour = color.white),
                text = element_text(size = 20, colour = color.blue))
      }
      
    })
    
    output$frame <- renderUI({
      if(!is.null(serverValues$url)) {
        redirectScript <- paste0("window = window.open('", serverValues$url, "');")
        tags$script(HTML(redirectScript))
      } else {
        redirectScript <- paste0("window = window.open('", "http://orion.tw.rpi.edu/~olyerickson/RenIDEA_black_invert.jpg", "');")
        tags$script(HTML(redirectScript))
      }
    })
    
    observeEvent(serverValues$query.c, {
      text <- serverValues$query.c[!is.na(serverValues$query.c)]
      for(i in which(grepl("\\s", text))) {
        text[i] <- paste0('"', text[i], '"')
      }
      updateTextInput(session, "query", value = paste0(text, collapse = " "))
    })
    
    observeEvent(serverValues$current_node_id, {
      visNetworkProxy("network") %>%
        visSelectNodes(serverValues$current_node_id)
    })
    
  }
)
