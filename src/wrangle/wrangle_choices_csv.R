wrangle_choices_csv <- function(raw_data, exp_info){
  
  choices <- raw_data$choices
  
  # For exp_rank_types, trial_type is derived from singleOption and choice.
  if (exp_info$experiment_name == "exp_skew_types") {
    choices <- choices %>% mutate(
      trial_type = case_when(
        singleOption == TRUE ~ "Single",
        singleOption == FALSE & choice %in% c("safe", "risky") ~ "Shared",
        singleOption == FALSE & choice %in% c("context", "worst") ~ "Context"
      )
    )
  }
  
  # For exp_rank_tokens, trial_type was labelled choiceType. Change here for consistency.
  if (exp_info$experiment_name == "exp_skew_tokens") {
    choices <- choices %>% mutate(trial_type = choiceType)
  } 
  
  choices <- choices %>% 
    mutate(
      risky = case_when(
        choice %in% c("safe", "worst") ~ FALSE,
        choice %in% c("risky", "context") ~ TRUE
      )
    ) %>% 
    select(
      participant,
      condition,
      trial = banditTrial,
      block,
      trial_in_block = trialInBlock,
      trial_type,
      left_option = leftOption,
      right_option = rightOption,
      feedback,
      reaction_time = rt,
      choice,
      risky 
    )
  
  # Reformat condition names and convert to factors
  choices <- choices %>% 
    mutate(
      # Relabel the conditions based on skewness rather than rank of the shared option
      condition = case_when(
        condition == "lowRank" ~ "Left-skewed",
        condition == "highRank" ~ "Right-skewed",
        TRUE ~ condition
      ) %>% 
        format_conditions(),
      trial_type = format_conditions(trial_type)
    )
  
  # Test that the data was processed correctly and that there is no incomplete or duplicate data
  test_correct_dimensions(choices, exp_info, "choices")
  
  return(choices)
}