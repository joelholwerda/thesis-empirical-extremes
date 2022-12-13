wrangle_choices_mat <- function(raw_data, exp_info){
  
  choices <- raw_data %>%
    map_dfr(function(data){
      # Select choices data from with raw_data
      temp_choices <- data[[exp_info$matlab_index$choices]]
      colnames(temp_choices) <- exp_info$choice_names
      temp_choices <- temp_choices %>% as_tibble()
      
      # Drop replicate condition column if required
      if ("condition" %in% exp_info$choice_names) {
        
        temp_choices <- temp_choices %>% select(-condition)
        
      }
      
      n_choices <- dim(temp_choices)[1]
      
      # Bind choices data with participant and condition info
      tibble(
        participant = data[[exp_info$matlab_index$participant]] %>% 
          as.numeric() %>% 
          rep(times = n_choices),
        condition = data[[exp_info$matlab_index$condition]] %>% 
          as.character() %>% 
          rep(times = n_choices)
      ) %>% 
        bind_cols(temp_choices)
    })
  
  # Initialise columns and recode variables
  choices <- choices %>% 
    add_column(choice = NA, risky = NA) %>%
    mutate(
      stimulus_A = exp_info$memory_order[stimulus_A],
      stimulus_B = exp_info$memory_order[stimulus_B],
      trial_type = exp_info$trial_type_names[trial_type]
    )
  
  # Choosen stimulus based on response
  choices$choice[choices$response == 1] <- choices$stimulus_A[choices$response == 1]
  choices$choice[choices$response == 2] <- choices$stimulus_B[choices$response == 2]
  
  # Recode choice as risky? boolean 
  choices$risky[choices$choice %in% exp_info$safe] <- FALSE
  choices$risky[choices$choice %in% exp_info$risky] <- TRUE
  
  # Reformat condition names and convert to factors
  choices <- choices %>% 
    mutate(
      condition = format_conditions(condition),
      trial_type = format_conditions(trial_type)
    )
  
  # Test that the data was processed correctly and that there is no incomplete or duplicate data
  test_correct_dimensions(choices, exp_info, "choices")
  
  return(choices)
  
}