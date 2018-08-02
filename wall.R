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
                               colored.text <- ColorHashtags(data.subset$text[[y]], query.c[!is.na(query.c)])
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
ColorHashtags <- function(string, query.c) {
  hashtags <- str_extract_all(string, "#(\\d|\\w)+")
  for(hashtag in hashtags[[1]]) {
    if(toupper(hashtag) %in% toupper(query.c)) {
      replacement <- paste0('<font color=', color.blue, '>', hashtag, '</font>')
    } else {
      replacement <- paste0('<font color=', color.orange, '>', hashtag, '</font>')
    }
    string <- str_replace_all(string, hashtag, replacement)
  }
  return(string)
  # print(string)
  # string.c <- unlist(strsplit(string, "[ (\n\n)(\n)]"))
  # print(string.c)
  # stop()
  # hashtag.indices <- grep("#(\\d|\\w)+", string.c)
  # colored.string.c <- lapply(1:length(string.c), function(x) {
  #   if(x %in% hashtag.indices) {
  #     if(toupper(string.c[[x]]) %in% toupper(query.c)) {
  #       paste('<font color=', color.blue, '>', string.c[[x]], '</font>', sep = "")
  #     } else {
  #       paste('<font color=', color.orange, '>', string.c[[x]], '</font>', sep = "")  
  #     }
  #   } else {
  #     string.c[[x]]
  #   }
  # })
  # colored.string <- paste(colored.string.c, collapse = " ")
  # return(colored.string)
}