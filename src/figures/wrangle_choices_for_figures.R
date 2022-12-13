# Function that takes choice_summary and attaches the data for value and skewness (shared options) together
wrangle_choices_for_figures <- function(data, exclusions, value = TRUE, skew = TRUE) {
  
  # Some experiments use the column name trial_type where others use option_value
  column_names <- data %>% names()
  value_name <- ifelse("option_value" %in% column_names, expr(option_value), expr(trial_type))
  
  choices_value <- NULL
  choices_skew <- NULL
  
  # If the experiment includes low and high value options, include their data
  if (value) {
  
  choices_value <- data %>% 
    filter(!participant %in% exclusions, !trial_type %in% c("Catch", "Extreme")) %>%
    # Remove ordering from factors so it doesn't interfere with bind_rows()
    mutate(!!value_name := factor(!!value_name, ordered = FALSE)) %>% 
    select(participant, x_variable = risky_prop, y_variable = !!value_name, fill = !!value_name, facet = condition)
  
  }
  
  # If the experiment includes shared options and skewed distributions, include their data
  if (skew) {
  
  choices_skew <- data %>% 
    filter(
      !participant %in% exclusions,
      as.character(trial_type) %in% c("Shared", "Extreme", "Decision")
    ) %>% 
    mutate(trial_type = "Shared" %>% factor(ordered = FALSE)) %>% 
    mutate(!!value_name := !!value_name %>% factor(ordered = FALSE)) %>% 
    select(participant, x_variable = risky_prop, y_variable = condition, fill = !!value_name, facet = trial_type)
  
  }
  
  choices_combined <- bind_rows(choices_value, choices_skew)
  
  # Reorder facet variable dependent on the experiment
  if (choices_combined$facet %>% str_detect("skewed") %>% any()) {
    choices_combined <- choices_combined %>% mutate(
      facet = facet %>% factor(levels = c("Right-skewed", "Left-skewed", "Shared"))
    )
    
  } else if (choices_combined$facet %>% str_detect("variance") %>% any()) {
    choices_combined <- choices_combined %>% mutate(
      facet = facet %>% factor(levels = c("High variance", "Low variance"))
    )
    
  } else if (choices_combined$facet %>% str_detect("Random") %>% any()) {
    choices_combined <- choices_combined %>% mutate(
      facet = facet %>% factor(levels = c("Random", "Alternating"))
    )
  }
  
  return(choices_combined)
  
}
