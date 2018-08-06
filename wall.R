# Only call when update on controller is pressed, update every column on the
# wall sequentially. There will only be gaps at the end
UpdateWall <- function(data, query.c) {
  col.list <- vector("list", 12)
  col.list <- lapply(1:12, function(col.num) {
    if(is.na(query.c[col.num])) {
      column(width = 1,
             textInput(paste0("text.column.", col.num), col.num),
             actionButton(paste0("button.column.", col.num), NULL))
    } else {
      data.subset <- GetDataSubset(data, query.c[col.num])
      UpdateColumn(data.subset, query.c, col.num)
    }
  })
  return(col.list)
}

UpdateColumn <- function(data.subset, query.c, col.num) {
  column(width = 1,
         tags$div(includeCSS("wall.css"),
                  fluidRow(
                    tags$h2(query.c[col.num])
                  ),
                  fluidRow(style = 'height: 780px;
                  overflow-y: auto;
                  overflow-x: hidden;',
                           if(nrow(data.subset) > 0) {
                             lapply(1:nrow(data.subset), function(y) {
                               colored.text <- ColorHashtags(data.subset$text[[y]],
                                                             query.c[!is.na(query.c)],
                                                             data.subset$hashtags[[y]],
                                                             data.subset$urls_t.co[[y]])
                               tags$div(style='padding: 0px;',
                                        tags$h3(paste("@", data.subset$screen_name[[y]], sep = "")), 
                                        tags$p(HTML(colored.text)),
                                        tags$header(
                                          tags$h5("Favorites:"),
                                          tags$span(data.subset$favorite_count[[y]])
                                        ),
                                        tags$header(
                                          tags$h5("Retweets:"),
                                          tags$span(data.subset$retweet_count[[y]])
                                        )
                               )
                             })
                           }
                  )
         )
  )
}

# Color the hashtags in a string using HTML
ColorHashtags <- function(string, query.c, hashtags, urls) {
  hashtags <- paste0("#", hashtags)
  hashtags <- hashtags[order(nchar(hashtags), hashtags, decreasing = TRUE)]
  for(hashtag in hashtags) {
    if(toupper(hashtag) %in% toupper(query.c)) {
      replacement <- paste0('<font color=', color.blue, '>', hashtag, '</font>')
    } else {
      replacement <- paste0('<font color=', color.orange, '>', hashtag, '</font>')
    }
    string <- str_replace_all(string, hashtag, replacement)
  }
  for(url in urls) {
    if(!is.na(url)) {
      replacement <- paste0('<a href="url">', url, '</a>')
      string <- str_replace_all(string, url, replacement)
    }
  }
  return(string)
}