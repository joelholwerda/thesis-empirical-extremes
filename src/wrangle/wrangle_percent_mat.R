wrangle_percent_mat <- function(raw_data, exp_info){
  
  percent <- raw_data %>% 
    map_dfr(function(data){
      perc <- tibble(
        # Select percentage estimation data from within raw_data
        participant = data[[exp_info$matlab_index$participant]] %>% as.numeric(),
        condition = data[[exp_info$matlab_index$condition]] %>% as.character(),
        stimulus = exp_info$memory_order %>% 
          rep(each = exp_info$n_outcomes[[condition]]) %>%
          as.factor(),
        label = data[[exp_info$matlab_index$percent_labels]] %>% unlist(),
        response = data[[exp_info$matlab_index$percent_responses]] %>% as.vector()
      ) %>% 
        # remove "points ---->  "
        mutate(label = label %>%
                 str_replace("points -->", "") %>%  
                 as.numeric())
      
      # For each stimulus, filter for only outcomes that were experienced
      map2_dfr(exp_info$conditions[[ data[[exp_info$matlab_index$condition]] ]] %>% names(),
               exp_info$conditions[[ data[[exp_info$matlab_index$condition]] ]], 
               ~ perc %>% filter(stimulus == .x, label %in% .y)
      )
    })
  
  # Reformat condition names and convert to factors
  percent <- percent %>% 
    mutate(
      condition = format_conditions(condition)
    )
  
  # Test that the data was processed correctly and that there is no incomplete or duplicate data
  test_correct_dimensions(percent, exp_info, "percent")
  
  return(percent)
  
}