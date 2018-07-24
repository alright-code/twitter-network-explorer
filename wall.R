WallUI <- function(data, query.c) {
  fluidPage(
    fluidRow(
      lapply(1:12, function(x) {
        # Empty column
        if(x > length(query.c)) {
          column(width = 1)
        } else {
          data.subset <- GetDataSubset(data, query.c[[x]])
          column(width = 1,
                 WallColumn(data.subset, query.c, x))
        }  
      })
    )
  )
}

WallColumn <- function(data.subset, query.c, x) {
  tags$div(includeCSS("wall.css"),
           fluidRow(
             textInput(paste("col.input.", x, sep = ""), NULL, width = "50%"),
             actionButton(paste("col.but.", x, sep = ""), NULL, width = "50%")
           ),
           fluidRow(
             tags$h2(query.c[[x]])
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