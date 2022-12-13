test_correct_dimensions <- function(data, exp_info, data_type) {

  if (length(data) == 0) stop(paste0(data_type, " for ", exp_info$experiment_name, " has length 0. Check that the data imported properly."))
  
  # Extract correct rows for appropriate data_type from experiment info
  correct_rows <- exp_info$correct_rows[[data_type]]
  
  # Count number of rows for each participant
  n_rows <- data %>% 
    group_by(participant) %>% 
    count() %>% 
    ungroup()
  
  # Filter participants that have the wrong number of rows of data
  incomplete_data <- n_rows %>% 
    filter(n != correct_rows) %>% 
    mutate(phrase = paste("Participant", participant, "has", n, "rows. "))
  
  # Provide warning if incorrect number of rows
  if (dim(incomplete_data)[1] > 0) {
    warning(
      paste0(
        "Some participants in ",
        exp_info$experiment_name,
        " have the wrong number of rows of ", 
        data_type,
        " data (the correct number of rows is ", 
        correct_rows, 
        "). \n This message can be safely ignored if data from the following participants has been excluded from analysis (i.e., the participant number is included in ",
        exp_info$experiment_name,
        "$exclusions), otherwise check that there are no duplicates or incomplete data: \n -- ",
        str_c(incomplete_data$phrase, collapse = "\n -- "),
        "\n"
      )
    )
  }
}
