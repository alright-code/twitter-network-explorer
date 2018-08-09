# Only call when update on controller is pressed, update every column on the
# wall sequentially. There will only be gaps at the end
UpdateWall <- function(data, query.c) {
  col.list <- vector("list", 12)
  col.list <- lapply(1:12, function(col.num) {
    if(is.na(query.c[col.num])) {
      column(width = 1,
             textInput(paste0("text.column.", col.num), col.num),
             actionButton(paste0("button.column.", col.num), "Submit"))
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
                                                             c(data.subset$urls_t.co[[y]], data.subset$ext_media_t.co[[y]]),
                                                             data.subset$mentions_screen_name[[y]])
                               tags$div(style='padding: 0px;',
                                        tags$h3(paste0("@", data.subset$screen_name[[y]])), 
                                        tags$p(HTML(colored.text)),
                                        tags$p(HTML(paste("&#x1F499", data.subset$favorite_count[[y]], "&#x1F504", data.subset$retweet_count[[y]])))
                               )
                             })
                           }
                  )
         )
  )
}

# Color the hashtags and urls in a string using HTML
ColorHashtags <- function(string, query.c, hashtags, urls, mentions) {
  string.copy <- string
  hashtags <- hashtags[order(nchar(hashtags), hashtags, decreasing = TRUE)]
  for(hashtag in hashtags) {
    if(toupper(paste0("#", hashtag)) %in% toupper(query.c)) {
      replacement <- paste0('<span class="clickable"><font color=', color.blue, '>#&', hashtag, '</font></span>')
    } else {
      replacement <- paste0('<span class="clickable"><font color=', "#ee7e1d", '>#&', hashtag, '</font></span>')
    }
    string <- str_replace_all(string, paste0("#", hashtag), replacement)
  }
  string <- str_replace_all(string, "#&", "#")
  for(mention in mentions) {
    if(toupper(paste0("@", mention)) %in% toupper(query.c)) {
      replacement <- paste0('<span class="clickable"><font color=', color.green, '>@', mention, '</font></span>')
    } else {
      replacement <- paste0('<span class="clickable"><font color=', "#ee7e1d", '>@', mention, '</font></span>')
    }
    string <- str_replace_all(string, paste0("@", mention), replacement)
  }
  for(url in urls) {
    if(!is.na(url)) {
      replacement <- paste0('<span class="clickable"><font color=', "#ee1d8d", '>', url, '</font></span>')
      string <- str_replace_all(string, url, replacement)
    }
  }
  return(string)
}