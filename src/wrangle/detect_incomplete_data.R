# Function that checks whether each participant has the correct number of rows for choices, ftm, and percent data. 
# Parameters: experiment must be an experiment object (e.g., exp_skew_centre); 
# n_choices, n_ftm, n_percent are the expected number of rows for the choices, ftm, and percent tasks
detect_incomplete_data <- function(experiment, n_choices, n_ftm, n_percent){
  
  incomplete_choices <- experiment$choices %>% 
    group_by(participant, condition) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    filter(n != n_choices)
  
  incomplete_ftm <- experiment$ftm %>% 
    group_by(participant, condition) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    filter(n != n_ftm)
  
  incomplete_percent <- experiment$percent %>% 
    group_by(participant, condition) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    filter(n != n_percent)
  
  incomplete_data <- full_join(incomplete_choices, incomplete_ftm) %>% full_join(incomplete_percent)
  
  participants_with_incomplete_data <- tibble(
    participant = incomplete_data$participant,
    condition = incomplete_data$condition
  ) %>% 
    mutate(
      incomplete_choices = participant %in% incomplete_choices$participant,
      incomplete_ftm = participant %in% incomplete_ftm$participant,
      incomplete_percent = participant %in% incomplete_percent$participant
    ) 
  
  return(participants_with_incomplete_data)
  
}
