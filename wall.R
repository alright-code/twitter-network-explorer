# Only call when update on controller is pressed, update every column on the
# wall sequentially. There will only be gaps at the end
UpdateWall <- function(data, query) {
  col.list <- vector("list", 12)
  col.list <- lapply(1:12, function(col_num) {
    if(is.na(query[col_num])) {
      column(width = 1,
             textInput(paste0("text.column.", col_num), label = ""),
             actionButton(paste0("button.column.", col_num), "Submit"))
    } else {
      data_subset <- getDataSubset(data, query[[col_num]])
      UpdateColumn(data_subset, query, col_num)
    }
  })
  return(col.list)
}

UpdateColumn <- function(data_subset, query, col_num) {
  column(width = 1,
         tags$div(includeCSS("wall.css"),
                  fluidRow(
                    tags$h2(tags$span(class = "clickable", query[col_num]))
                  ),
                  fluidRow(style = 'height: 600px;
                  overflow-y: auto;
                  overflow-x: hidden;',
                           if(nrow(data_subset) > 0) {
                             lapply(1:nrow(data_subset), function(tweet_num) {
                               colored.text <- ColorHashtags(data_subset$text[[tweet_num]],
                                                             query[!is.na(query)],
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

# Color the hashtags and urls in a string using HTML
ColorHashtags <- function(string, query, hashtags, urls, mentions) {
  string.copy <- string
  hashtags <- hashtags[order(nchar(hashtags), hashtags, decreasing = TRUE)]
  mentions <- mentions[order(nchar(mentions), mentions, decreasing = TRUE)]
  for(hashtag in hashtags) {
    if(toupper(paste0("#", hashtag)) %in% toupper(query)) {
      replacement <- paste0('<span class="clickable included">', paste0("#&", hashtag), '</span>')
    } else {
      replacement <- paste0('<span class="clickable notincluded">', paste0("#&", hashtag), '</span>')
    }
    string <- str_replace_all(string, paste0("#", hashtag), replacement)
  }
  string <- str_replace_all(string, "#&", "#")
  for(mention in mentions) {
    if(toupper(paste0("@", mention)) %in% toupper(query)) {
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