# Only call when update on controller is pressed, update every column on the
# wall sequentially. There will only be gaps at the end
updateWall <- function(data, nodes) {
  # Creates the output displayed on the campfire wall of tweet columns.
  # 
  # Args:
  #   data: Tibble of tweet data returned by rtweet.
  #   nodes: Data frame containing node data formatted for visnetwork.
  #
  # Returns:
  #   List of Shiny HTML columns containing tweet data.
  col_list <- vector("list", 12)
  col_list <- lapply(1:12, function(col_num) {
    if(!(col_num %in% nodes$position_)) {
      column(width = 1,
             textInput(paste0("text.column.", col_num), label = ""),
             actionButton(paste0("button.column.", col_num), "Submit"))
    } else {
      current_node_data <- nodes[nodes$position_ == col_num, ]
      data_subset <- getDataSubset(data, current_node_data$id)
      UpdateColumn(data_subset, current_node_data, nodes$id)
    }
  })
}

UpdateColumn <- function(data_subset, current_node_data, queries) {
  # Creates a single Shiny HTMl column containing tweet data for specific single query.
  # 
  # Args:
  #   data_subset: Tibble of tweet data subset into a single search query.
  #   current_node_data: Row of nodes data frame with the matching current search query.
  #   queries: Vector of strings to be used to search for tweets. Queries are NOT allowed to be NA.
  #
  # Returns:
  #   List of Shiny html columns containing tweet data.
  column(width = 1,
         tags$div(includeCSS("wall.css"),
                  fluidRow(
                    tags$h2(tags$span(class = "clickable", current_node_data$label))
                  ),
                  fluidRow(style = 'height: 600px;
                  overflow-y: auto;
                  overflow-x: hidden;',
                           if(nrow(data_subset) > 0) {
                             lapply(1:nrow(data_subset), function(tweet_num) {
                               colored.text <- colorHashtags(data_subset$text[[tweet_num]],
                                                             queries,
                                                             data_subset$hashtags[[tweet_num]],
                                                             c(data_subset$urls_t.co[[tweet_num]], data_subset$ext_media_t.co[[tweet_num]]),
                                                             data_subset$mentions_screen_name[[tweet_num]])
                               tags$div(style = 'padding: 0px;',
                                        tags$h3(tags$span(class = "clickable", paste0("@", data_subset$screen_name[[tweet_num]]))),
                                        tags$p(HTML(colored.text)),
                                        tags$p(HTML(paste("&#x1F499", data_subset$favorite_count[[tweet_num]], "&#x1F504", data_subset$retweet_count[[tweet_num]])))
                               )
                             })
                           }
                  )
         )
  )
}

colorHashtags <- function(string, queries, hashtags, urls, mentions) {
  # Colors a string based on specific hashtags, urls, and mentions.
  # 
  # Args:
  #   string: String of text to be colored.
  #   queries: Vector of strings to be used to search for tweets. Queries are allowed to be NA.
  #   hashtags: Vector of hashtags to be colored in string. Do not need to begin with "#".
  #   urls: Vector of urls to be colored in string.
  #   mentions: Vector of hashtags to be colored in string. Do not need to begin with "@".
  #
  # Returns:
  #   String with HTML formatted coloring.
  string.copy <- string
  hashtags <- hashtags[order(nchar(hashtags), hashtags, decreasing = TRUE)]
  mentions <- mentions[order(nchar(mentions), mentions, decreasing = TRUE)]
  for(hashtag in hashtags) {
    if(toupper(paste0("#", hashtag)) %in% toupper(queries)) {
      replacement <- paste0('<span class="clickable included">', paste0("#&", hashtag), '</span>')
    } else {
      replacement <- paste0('<span class="clickable notincluded">', paste0("#&", hashtag), '</span>')
    }
    string <- str_replace_all(string, paste0("#", hashtag), replacement)
  }
  string <- str_replace_all(string, "#&", "#")
  for(mention in mentions) {
    if(toupper(paste0("@", mention)) %in% toupper(queries)) {
      replacement <- paste0('<span class="clickable mentionincluded">', paste0("@&", mention), '</span>')
    } else {
      replacement <- paste0('<span class="clickable notincluded">', paste0("@&", mention), '</span>')
    }
    string <- str_replace_all(string, paste0("@", mention), replacement)
  }
  string <- str_replace_all(string, "@&", "@")
  for(url in urls) {
    if(!is.na(url)) {
      replacement <- paste0('<span class="clickable url">', url, '</span>')
      string <- str_replace_all(string, url, replacement)
    }
  }
  return(string)
}