summarise_demographics <- function(demographics_data, incomplete_choices = NA, missing_memory_data = NA, failed_catch = NA, failed_colour_blindness = NA) {
  
  require(rlang)
  
  # Count number of participants. If there are multiple experiments, first unite the experiment and participant columns
  if ("experiment" %in% names(demographics_data)) {
    n_participants <- unite(demographics_data, "participant", participant, experiment) %>% 
      .$participant %>% 
      unique() %>% 
      length()
  } else {
    n_participants <- demographics_data %>% 
      .$participant %>% 
      unique() %>% 
      length()
  }
  
  # Place demographics data into tibble
  demographics_summary <- tibble(
    n_participants = n_participants,
    n_per_condition = demographics_data$condition %>% droplevels() %>% table() %>% table_to_string(),
    age_mean = demographics_data$age %>% mean(na.rm = TRUE),
    age_sd = demographics_data$age %>% sd(na.rm = TRUE),
    gender = demographics_data$gender %>% table() %>% table_to_string(),
    n_female = sum(demographics_data$gender == "Female"),
    n_male = sum(demographics_data$gender == "Male"),
    prize_mean = demographics_data$prize %>% mean(na.rm = TRUE),
    prize_sd = demographics_data$prize %>% sd(na.rm = TRUE)
  )
  
  excluded <- tibble(participant = "", condition = "", .rows = 0)
  
  if (!is_na(incomplete_choices) | !is_na(missing_memory_data)) {
    
    incomplete_data = full_join(
      incomplete_choices %>% mutate(participant = participant %>% as.character()), 
      missing_memory_data %>% mutate(participant = participant %>% as.character())
    ) %>% 
      filter(!is.na(participant)) %>%
      unique()
    
    demographics_summary <- demographics_summary %>% 
      mutate(
        # Count how many participants had missing data
        n_incomplete_data = incomplete_data %>% dim() %>% .[1],
        # Write the participant numbers and conditions to strings
        incomplete_data_participants = incomplete_data$participant %>% paste(collapse = ", "),
        incomplete_data_conditions = incomplete_data$condition %>% droplevels() %>% table() %>% table_to_string()
      )
    
    excluded <- full_join(excluded, incomplete_data)
  }
  
  if (!is_na(failed_catch)) {
    
    failed_catch <- failed_catch %>% 
      filter(!is.na(participant)) %>%
      mutate(participant = participant %>% as.character()) %>%
      unique()
    
    demographics_summary <- demographics_summary %>% 
      mutate(
        # Count how many participants failed the catch trials
        n_failed_catch = failed_catch %>% dim() %>% .[1],
        # Write the participant numbers and conditions to strings
        failed_catch_participants = failed_catch$participant %>% paste(collapse = ", "),
        failed_catch_conditions = failed_catch$condition %>% droplevels() %>% table() %>% table_to_string()
      )
    
    excluded <- full_join(excluded, failed_catch)
  }
  
  if (!is_na(failed_colour_blindness)) {
    
    failed_colour_blindness <- failed_colour_blindness %>% 
      filter(!is_na(participant)) %>% 
      mutate(participant = participant %>% as.character()) %>%
      unique()
    
    demographics_summary <- demographics_summary %>% 
      mutate(
        # Count how many participants failed the catch trials
        n_failed_colour_blindness = failed_colour_blindness %>% dim() %>% .[1],
        failed_colour_blindness_participants = failed_colour_blindness$participant %>% paste(collapse = ", "),
        failed_colour_blindness_conditions = failed_colour_blindness$condition %>% droplevels() %>% table() %>% table_to_string()
      )
    
    excluded <- full_join(excluded, failed_colour_blindness)
  }
  
  demographics_summary <- demographics_summary %>% mutate(
    # Count how many exclusions there were in total
      n_excluded = excluded %>% dim() %>% .[1],
      excluded_participants = excluded$participant %>% paste(collapse = ", "),
      excluded_conditions = excluded$condition %>% table() %>% table_to_string()
  )
  
  return(demographics_summary)
  
}
