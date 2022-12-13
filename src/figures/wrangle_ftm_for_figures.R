# Function that takes ftm_recode and attaches the data for value and skewness (shared options) together
wrangle_ftm_for_figures <- function(data, exclusions, value = TRUE, skew = TRUE) {
  
  # Some experiments use the column name stimulus where others use stimulus_value
  column_names <- data %>% names()
  value_name <- ifelse("stimulus_value" %in% column_names, expr(stimulus_value), expr(stimulus))
  response_name <- ifelse("response_recode_value" %in% column_names, expr(response_recode_value), expr(response_recode))
  
  ftm_value <- NULL
  ftm_skew <- NULL
  
  # If the experiment includes low and high value options include their data
  if (value) {
  
    ftm_value <- data %>% 
      filter(!participant %in% exclusions, !!value_name != "Extreme") %>% 
      # Remove ordering from factors so it doesn't interfere with bind_rows()
      mutate(!!value_name := factor(!!value_name, ordered = FALSE)) %>% 
      select(participant, y_variable = !!value_name, fill = !!response_name, facet_rows = condition)
  
  }

  # If the experiment includes shared options and skewed distributions, include their data
  if (skew) {
  
    ftm_skew <- data %>% 
      filter(
        !participant %in% exclusions,
        as.character(stimulus) %in% c("Shared", "Decision")
      ) %>% 
      mutate(stimulus := "Shared" %>% factor(ordered = FALSE)) %>% 
      select(participant, y_variable = condition, fill = !!response_name, facet_rows = stimulus)
  
  }
  
  ftm_combined <- bind_rows(ftm_value, ftm_skew)
  
  # Indicate whether better or worse outcome was reported
  ftm_outcome <- ftm_combined %>% 
    filter(fill %in% c("Better", "Worse")) %>%
    mutate(fill = factor(fill, levels = c("Better", "Worse")))
  
  # Indicate whether experienced or not experience outcome was reported
  ftm_accuracy <- ftm_combined %>%
    mutate(
      fill = case_when(
        fill %in% c("Better", "Worse") ~ "Experienced",
        TRUE ~ "Not experienced"
      ) %>% 
      factor(levels = c("Experienced", "Not experienced"))
    )
  
  ftm_outcome_and_accuracy <- bind_rows(outcome = ftm_outcome, accuracy = ftm_accuracy, .id = "facet_cols") %>% 
    mutate(
      fill = fill %>% factor(levels = c("Better", "Worse", "Experienced", "Not experienced")),
      facet_cols = facet_cols %>% factor(levels = c("outcome", "accuracy"))
    )
  
  # Reorder facet variable dependent on the experiment
  if (ftm_outcome_and_accuracy$facet_rows %>% str_detect("skewed") %>% any()) {
    ftm_outcome_and_accuracy <- ftm_outcome_and_accuracy %>% mutate(
      facet_rows = facet_rows %>% factor(levels = c("Right-skewed", "Left-skewed", "Shared"))
    )
    
  } else if (ftm_outcome_and_accuracy$facet_rows %>% str_detect("variance") %>% any()) {
    ftm_outcome_and_accuracy <- ftm_outcome_and_accuracy %>% mutate(
      facet_rows = facet_rows %>% factor(levels = c("High variance", "Low variance"))
    )
    
  } else if (ftm_outcome_and_accuracy$facet_rows %>% str_detect("Random") %>% any()) {
    ftm_outcome_and_accuracy <- ftm_outcome_and_accuracy %>% mutate(
      facet_rows = facet_rows %>% factor(levels = c("Random", "Alternating"))
    )
  }
  
  return(ftm_outcome_and_accuracy)
}
