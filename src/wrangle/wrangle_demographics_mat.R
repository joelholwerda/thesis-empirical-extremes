wrangle_demographics_mat <- function(raw_data, exp_info){
  
  demographics <- raw_data %>% 
    map_dfr(
      ~ tibble(
        participant = .[[exp_info$matlab_index$participant]] %>% as.character(),
        condition = .[[exp_info$matlab_index$condition]] %>% as.character(),
        age = ifelse(.[[exp_info$matlab_index$age]] %>% length() == 0, NA, .[[exp_info$matlab_index$age]] %>%as.numeric()),
        gender = ifelse(.[[exp_info$matlab_index$gender]] %>% length() == 0, "Undisclosed", .[[exp_info$matlab_index$gender]] %>% as.character()), 
        prize = .[[exp_info$matlab_index$prize]] %>% as.character(),
        start_time = .[[exp_info$matlab_index$start_time]] %>% dmy_hms(),
        end_time = .[[exp_info$matlab_index$end_time]] %>% dmy_hms()
      ) 
    ) %>%
    # remove dollar sign from prize and convert to numeric
    # ifelse is required because the prize was formatted differently in Experiment 1
    mutate(
      prize = ifelse(
        str_detect(prize, "\\$"),
        prize %>% str_sub(start = 2) %>% as.numeric(),
        prize %>% as.numeric() / 10),
      time_taken = end_time - start_time,
      condition = condition %>% format_conditions(),
      # Recode gender columns (this works for the current dataset because all responses were female or male or non-binary)
      gender = case_when(
        gender %>% str_to_lower() %>% str_detect("f") ~ "Female",
        gender %>% str_to_lower() %>% str_detect("non") ~ "Non-binary",
        gender == "Undisclosed" ~ "Undisclosed",
        TRUE ~ "Male"
      )
    ) %>% 
    as_tibble()
  
  # Test that the data was processed correctly and that there is no incomplete or duplicate data
  test_correct_dimensions(demographics, exp_info, "demographics")
  
  return(demographics)
  
}
