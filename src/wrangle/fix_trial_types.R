fix_trial_types <- function(data, exp_info){
  
  # List trial types other than catch and single (these are dealt with separately)
  trial <- exp_info$trial_type_names[exp_info$trial_type_names != "catch" & exp_info$trial_type_names != "single"]
  
  # Start labelling all as catch as the default (this will remain only if stimulus A and B have different types)
  data$trial_type <- "catch"
  
  # If stimulus_A and stimulus_B have the same type, label the trial as that type
  for (type in trial) {
    
    data$trial_type[str_detect(data$stimulus_A, type) & str_detect(data$stimulus_B, type)] <- type
    
  }
  
  # If one stimulus is NA, label as single
  data$trial_type[is.na(data$stimulus_A) | is.na(data$stimulus_B)] <- "single"
  
  # Reformat names and convert to a factor 
  data <- data %>% mutate(trial_type = format_conditions(trial_type))
  
  return(data)
  
}
