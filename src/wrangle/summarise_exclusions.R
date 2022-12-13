summarise_exclusions <- function(demographics_summary) {
  
  exclusions <- demographics_summary$excluded_participants %>% 
    str_split(", ", simplify = TRUE) %>% 
    as.vector()

  return(exclusions)
}
