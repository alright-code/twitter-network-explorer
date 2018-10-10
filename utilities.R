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

StringQueryToVector <- function(queries_string) {
  # Converts a string of queries to a vector of queries
  # 
  # Args:
  #   queries_string: String of queries, max length 12.
  #                   Individual queries longer than a word are put in double quotes.
  # Returns:
  #   Vector of queries from the string.
  queries <- scan(text = queries_string, what = "character", quiet = TRUE)
  if(length(queries) < 12) {
    queries[(length(queries) + 1):12] <- NA
  }
  return(queries)
}