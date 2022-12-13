# Function that checks for participants with choices data but no ftm or percent 
# data
detect_missing_memory_data <- function(experiment) {
  
  # Compare the participants that have choice data with those that have memory data
  # Participants that have data for choices but not memory must have dropped out
  # before completing the memory tasks.
  ftm_missing <- setdiff(
    experiment$choices_summary$participant, 
    experiment$ftm$participant
  )
  
  percent_missing <- setdiff(
    experiment$choices_summary$participant, 
    experiment$percent$participant
  )
  
  # Combine the two types of missing memory data and add a condition column
  memory_missing <- experiment$choices_summary %>% 
    # Filter based on whether the participant had missing ftm or percent data
    filter(participant %in% (c(ftm_missing, percent_missing) %>% unique())) %>% 
    select(participant, condition) %>%
    # Remove duplicated rows
    unique()
  
  missing_memory <- memory_missing %>% 
    mutate(
      ftm_missing = participant %in% ftm_missing,
      percent_missing = participant %in% percent_missing
    )
  
  return(missing_memory)
  
}