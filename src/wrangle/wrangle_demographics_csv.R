wrangle_demographics_csv <- function(raw_data, exp_info){
    
  info_prize <- raw_data$participant_info %>%
    filter(trial_index > 10) %>% 
    select(participant, condition, prize)
  
  info_wide <- raw_data$participant_info %>%
    mutate(
      trial_type = case_when(
        trial_index == 3 ~ "colour_blindness",
        trial_index == 4 ~ "demographics",
        trial_index > 10 ~ "murk_id"
      )
    ) %>% 
    select(
      participant,
      trial_type,
      responses
    ) %>% 
    spread(
      key = trial_type, 
      value = responses
    ) 
  
  info_flat <- info_wide %>%
    drop_na() %>% 
    mutate(
      demographics = map(demographics, fromJSON),
      colour_blindness = map(colour_blindness, fromJSON)
    ) %>%
    unnest(colour_blindness) %>%
    mutate(
      age = map(demographics, ~ unlist(.x)["Q0"]) %>% unlist(),
      gender = map(demographics, ~ unlist(.x)["Q1"]) %>% unlist(),
      language = map(demographics, ~ unlist(.x)["Q2"]) %>% unlist(),
      country = map(demographics, ~ unlist(.x)["Q3"]) %>% unlist(),
      colour_blindness = unlist(colour_blindness)
    ) %>% 
    mutate(
      # Recode gender columns (this works for the current dataset because all responses were female or male or non-binary)
      gender = case_when(
        gender %>% str_to_lower() %>% str_detect("f") ~ "Female",
        gender %>% str_to_lower() %>% str_detect("non") ~ "Non-binary",
        TRUE ~ "Male"
      )
    ) %>% 
    select(-demographics)
  
  demographics <- left_join(info_prize, info_flat, by = "participant") %>% 
    mutate(
      # Relabel the conditions based on skewness rather than rank of the shared option
      condition = case_when(
        condition == "lowRank" ~ "Left-skewed",
        condition == "highRank" ~ "Right-skewed",
        TRUE ~ condition
      ) %>% 
        format_conditions(),
      participant = participant %>% as.character()
    )
  
  # Test that the data was processed correctly and that there is no incomplete or duplicate data
  test_correct_dimensions(demographics, exp_info, "demographics")
  
  return(demographics)

}