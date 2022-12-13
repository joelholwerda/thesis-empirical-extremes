# Function that takes percent_recode and attaches the data for value and skewness (shared options) together
wrangle_percent_for_figures <- function(data, exclusions, value = TRUE, skew = TRUE) {
  
  # Some experiments use the column name stimulus where others use stimulus_value
  column_names <- data %>% names()
  value_name <- ifelse("stimulus_value" %in% column_names, expr(stimulus_value), expr(stimulus))
  
  percent_value <- NULL
  percent_skew <- NULL
  
  # If the experiment includes low and high value options include their data
  if (value) {
  
  percent_value <- data %>% 
    filter(!participant %in% exclusions, !!value_name != "Extreme") %>% 
    # Remove ordering from factors so it doesn't interfere with bind_rows()
    mutate(!!value_name := factor(!!value_name, ordered = FALSE)) %>% 
    select(participant, x_variable = better_minus_worse , y_variable = !!value_name, fill = !!value_name , facet = condition)
  
  }
  
  # If the experiment includes shared options and skewed distributions, include their data
  if (skew) {
  
  percent_skew <- data %>% 
    filter(
      !participant %in% exclusions,
      as.character(stimulus) %in% c("Shared", "Decision")
    ) %>% 
    mutate(
      stimulus = "Shared" %>% factor(ordered = FALSE),
      !!value_name := !!value_name %>% factor(ordered = FALSE)
    ) %>% 
    select(participant, x_variable = better_minus_worse, y_variable = condition, fill = !!value_name , facet = stimulus)
  
  }
  
  percent_combined <- bind_rows(percent_value, percent_skew)
  
  
  # Reorder facet variable dependent on the experiment
  if (percent_combined$facet %>% str_detect("skewed") %>% any()) {
    percent_combined <- percent_combined %>% mutate(
      facet = facet %>% factor(levels = c("Right-skewed", "Left-skewed", "Shared"))
    )
    
  } else if (percent_combined$facet %>% str_detect("variance") %>% any()) {
    percent_combined <- percent_combined %>% mutate(
      facet = facet %>% factor(levels = c("High variance", "Low variance"))
    )
    
  } else if (percent_combined$facet %>% str_detect("Random") %>% any()) {
    percent_combined <- percent_combined %>% mutate(
      facet = facet %>% factor(levels = c("Random", "Alternating"))
    )
  }
  
  return(percent_combined)
}
