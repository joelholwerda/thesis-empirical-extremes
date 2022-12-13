wrangle_ftm_mat <- function(raw_data, exp_info){
  
  ftm <- raw_data %>% 
    map_dfr(
      ~ tibble(
        # Select first to mind data from within raw_data
        participant = .[[exp_info$matlab_index$participant]] %>% as.numeric(),
        condition = .[[exp_info$matlab_index$condition]] %>% as.character(),
        stimulus = exp_info$memory_order %>% as.factor,
        response = .[[exp_info$matlab_index$ftm_responses]] %>% as.vector()
        
      )
    )
  
  # Reformat condition names and convert to factors
  ftm <- ftm %>% 
    mutate(
      condition = format_conditions(condition)
    )
  
  # Test that the data was processed correctly and that there is no incomplete or duplicate data
  test_correct_dimensions(ftm, exp_info, "ftm")
  
  return(ftm)
  
}