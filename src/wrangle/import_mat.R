import_mat <- function(exp_info){
  
  # List all matlab data files (ensure that pilot data removed)
  files <- here::here(exp_info$load_location, list.files(path = here::here(exp_info$load_location), pattern = "*.mat"))
  
  raw_data <- files %>%
    map(~ readMat(.)[[1]])
  
  # Rename conditions
  for (i in seq_along(raw_data)) {
    
    raw_data[[i]][[exp_info$matlab_index$condition]] <- raw_data[[i]][[exp_info$matlab_index$condition]] %>% 
      recode(!!!exp_info$condition_names)
    
  }
  
  return(raw_data)
  
}