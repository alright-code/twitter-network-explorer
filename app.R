library(shiny)
library(visNetwork)
library(rtweet)
library(tidyverse)
library(ggplot2)
library(useful)

source("app-only-auth-twitter.R")
source("data.R")
source("floor.R")
source("wall.R")
source("external-monitor.R")
source("utilities.R")
source("campfire_lib.R")

campfireApp(
  
  controller = div(
    h1("Controller"),
    textAreaInput("queries_string", "Search Queries", default_queries, height = '200px'),
    fileInput("file", "Upload File", accept = c("text/plain")),
    sliderInput(inputId = "number_tweets",
                label = "Choose number of tweets for the search:",
                min = 50, max = 1000, value = 50),
    selectInput(inputId = "search_type",
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
    uiOutput("wall_ui"),
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
            uiOutput("tweets_info")
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
                   height: 1080px")
  ),
  
  urlmonitor = div(fluidPage(
    htmlOutput("frame")
  )),
  
  serverFunct = function(serverValues, output, session) {

    output$network <- renderVisNetwork({
      if(!is.null(serverValues$nodes)) {
        nodes_with_coords <- getCoords(serverValues$nodes)
        visNetwork(nodes_with_coords, serverValues$edges) %>%
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
            Shiny.onInputChange('current_node_id', -1);
            Shiny.onInputChange('current_edge_index', -1);
          }") %>%
          visPhysics(stabilization = FALSE, enabled = FALSE) %>%
          visInteraction(dragView = FALSE, zoomView = FALSE) %>%
          # Define behavior when clicking on nodes or edges
          visEvents(
                    click = "function(properties) {
                              if(this.getSelectedNodes().length == 1) {
                                Shiny.onInputChange('current_node_id', this.getSelectedNodes()[0]);
                                Shiny.onInputChange('current_edge_index', -1);
                              } else if(this.getSelectedEdges().length == 1) {
                                Shiny.onInputChange('current_edge_index', this.body.data.edges.get(properties.edges[0]).index);
                                Shiny.onInputChange('current_node_id', -1);
                              } else {
                                Shiny.onInputChange('current_node_id', -1);
                                Shiny.onInputChange('current_edge_index', -1);
                              }
                            }",
                    doubleClick = "function() {
                                     if(this.getSelectedNodes().length == 1) {
                                       Shiny.onInputChange('delete_node', this.getSelectedNodes()[0]);
                                       this.deleteSelected();
                                       Shiny.onInputChange('current_node_id', -1);
                                       Shiny.onInputChange('current_edge_index', -1);
                                     }
                                   }",
                    dragStart = "function() {
                                 var sel = this.getSelectedNodes();
                                 if(sel.length == 1) {
                                   Shiny.onInputChange('current_node_id', this.getSelectedNodes()[0]);
                                   Shiny.onInputChange('current_edge_index', -1)
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
    
    output$tweets_info <- renderUI({
      if(serverValues$current_node_id == -1 && serverValues$current_edge_index == -1) {
        tags$div(
          tags$h1(style = paste0("color:", color.blue), "Twitter Network Explorer"),
          tags$h2(style = paste0("color:", color.blue), paste("Total number of tweets found:", nrow(serverValues$data)))  
        )
      } else if(serverValues$current_node_id != -1) {
        node.name <- serverValues$current_node_id
        node.size <- nrow(serverValues$data_subset)
        tags$div(
          tags$h1(style = paste0("color:", color.blue), node.name),
          tags$h2(style = paste0("color:", color.blue), paste("Size:", node.size))
        )
      } else if(serverValues$current_edge_index != -1) {
        edge <- serverValues$edges[serverValues$edges$index == serverValues$current_edge_index, ]
        query <- c(as.character(edge$to), as.character(edge$from))
        edge.name <- paste(query, collapse = " AND ")
        edge.size <- nrow(serverValues$data_subset)
        tags$div(
          tags$h1(style = paste0("color:", color.blue), edge.name),
          tags$h2(style = paste0("color:", color.blue), paste("Size:", edge.size))
        )
      }
    })
    
    output$wall_ui <- renderUI({
      fluidPage(
        tags$script(HTML(
          "$(document).on('click', '.clickable', function () {
              var text =  $(this).text();
              Shiny.onInputChange('clicked_text', text);
            });"
        )),
        fluidRow(
          lapply(1:12, function(col.num) {
            serverValues$col_list[[col.num]] 
          })
        )
      )
    })
    
    output$top.users.bar.extern <- renderPlot({
      serverValues$monitor.domain <- getDefaultReactiveDomain()
      if(!is.null(serverValues$data_subset)) {
        serverValues$data_subset %>% 
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
      if(!is.null(serverValues$data_subset)) {
        serverValues$data_subset %>%
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
        redirectScript <- paste0("window = window.open('", "https://docs.google.com/presentation/d/1g_q5qQTJAt4jVekozFlEsnEo4XdveubVzLC2t9aeWlo/present", "');")
        tags$script(HTML(redirectScript))
      }
    })
    
    observeEvent(serverValues$queries, {
      text <- serverValues$queries[!is.na(serverValues$queries)]
      for(i in which(grepl("\\s", text))) {
        text[i] <- paste0('"', text[i], '"')
      }
      updateTextInput(session, "queries_string", value = paste0(text, collapse = " "))
    })
    
    observeEvent(serverValues$current_node_id, {
      visNetworkProxy("network") %>%
        visSelectNodes(serverValues$current_node_id)
    })
    
  }
)
