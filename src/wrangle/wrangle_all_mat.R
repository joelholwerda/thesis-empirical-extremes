
# Load custom helper functions
here::here("src", "wrangle", "wrangle_demographics_mat.R") %>% source()
here::here("src", "wrangle", "wrangle_choices_mat.R") %>% source()
here::here("src", "wrangle", "summarise_choices_mat.R") %>% source()
here::here("src", "wrangle", "wrangle_ftm_mat.R") %>% source()
here::here("src", "wrangle", "wrangle_percent_mat.R") %>% source()
here::here("src", "wrangle", "fix_trial_types.R") %>% source()
here::here("src", "wrangle", "test_correct_dimensions.R") %>% source()

wrangle_all_mat <- function(raw_data, exp_info){
  
  if (length(raw_data) == 0) stop("raw_data has length 0. Check that the data imported properly.")
  
  # wrangle choices data first so that choices_summary can refer to it while creating the list
  choices_temp <- wrangle_choices_mat(raw_data, exp_info)
  
  # The trial types were programmed incorrectly in the Matlab script for Experiment 3
  # fix_trial_types derives the trial types from the combination of stimuli for each trial
  if (exp_info$experiment_name == "exp_variance") choices_temp <- choices_temp %>% fix_trial_types(exp_info)
  
  data <- list(
    demographics = wrangle_demographics_mat(raw_data, exp_info),
    choices = choices_temp,
    choices_summary = summarise_choices_mat(choices_temp, exp_info),
    ftm = wrangle_ftm_mat(raw_data, exp_info),
    percent = wrangle_percent_mat(raw_data, exp_info)
    
  )
}