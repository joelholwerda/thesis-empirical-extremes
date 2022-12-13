  wrangle_ftm_csv <- function(raw_data, exp_info){
    
  ftm <- raw_data$ftm %>% 
    mutate(
      response_raw = map(responses, fromJSON) %>% unlist(),
      response_clean = suppressWarnings(as.numeric(response_raw))
    ) %>% 
    select(
      participant,
      condition,
      stimulus = option,
      response_raw,
      response_clean
    )
  
  # Reformat condition names and convert to factors
  ftm <- ftm %>% 
    mutate(
      # Relabel the conditions based on skewness rather than rank of the shared option
      condition = case_when(
        condition == "lowRank" ~ "Left-skewed",
        condition == "highRank" ~ "Right-skewed",
        TRUE ~ condition
      ) %>% 
        format_conditions(),
    )
  
  # Test that the data was processed correctly and that there is no incomplete or duplicate data
  test_correct_dimensions(ftm, exp_info, "ftm")
  
  return(ftm)

}
