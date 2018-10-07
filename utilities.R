###Colors###
color.green <- "#1dee7e"
color.pink <- "#ee1d8d"
color.orange <- "#ee7e1d"
color.white <- "#f0f0f0"
color.blue <- "#1D8DEE"
color.back <- "#151E29"
color.offback <- "#1B2737"
colors <- c("#1D8DEE", "#1dee7e", "#ee7e1d", "#ee1d8d", "#64B0F3", "#64F3A6", "#F3A664", "#B10D65", "#0D65B1", "#0DB159", "#B1590D", "#F364B0")


# Misc Functions ----------------------------------------------------------

StringQueryToVector <- function(query.string) {
  query <- scan(text = query.string, what = "character", quiet = TRUE)
  if(length(query) < 12) {
    query[(length(query) + 1):12] <- NA
  }
  return(query)
}