WallUI <- function(data, query.c) {
  fluidPage(
    fluidRow(
      lapply(c(10:1,12,11), function(x) {
        # Empty column
        if(x > length(query.c)) {
          column(width = 1,
                 offset = 0)
        } else {
          data.subset <- GetDataSubset(data, query.c[[x]])
          column(width = 1,
                 offset = 0,
                 WallColumn(data.subset, query.c[[x]], query.c))
        }  
      })
    )
  )
}

WallColumn <- function(data.subset, hashtag, query.c) {
  tags$div(includeCSS("wall.css"),
           fluidRow(
             tags$h2(hashtag)
           ),
           fluidRow(style = 'height: 780px;
                  overflow-y: auto;
                  overflow-x: hidden;',
                    if(nrow(data.subset) > 0) {
                      lapply(1:nrow(data.subset), function(y) {
                        colored.text <- ColorHashtags(data.subset$text[[y]], query.c)
                        tags$div(style='padding: 0px;',
                                 tags$h3(paste("@", data.subset$screen_name[[y]], sep = "")), 
                                 tags$p(HTML(colored.text)),
                                 tags$header(
                                   tags$h3("Favorites:"),
                                   tags$span(data.subset$favorite_count[[y]])
                                 ),
                                 tags$header(
                                   tags$h3("Retweets:"),
                                   tags$span(data.subset$retweet_count[[y]])
                                 )
                        )
                      })
                    }
           )
  )
  
}

# Color the hashtags in a string using HTML
ColorHashtags <- function(string, query.c) {
  string.c <- unlist(strsplit(string, "[ (\n)]"))
  hashtag.indices <- grep("#", string.c)
  colored.string.c <- lapply(1:length(string.c), function(x) {
    if(x %in% hashtag.indices) {
      if(toupper(string.c[[x]]) %in% toupper(query.c)) {
        paste('<font color=', color.blue, '>', string.c[[x]], '</font>', sep = "")
      } else {
        paste('<font color=', color.orange, '>', string.c[[x]], '</font>', sep = "")  
      }
    } else {
      string.c[[x]]
    }
  })
  colored.string <- paste(colored.string.c, collapse = " ")
  return(colored.string)
}