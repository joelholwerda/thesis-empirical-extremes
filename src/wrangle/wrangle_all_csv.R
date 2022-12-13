
# Load custom helper functions
here::here("src", "wrangle", "wrangle_demographics_csv.R") %>% source()
here::here("src", "wrangle", "wrangle_choices_csv.R") %>% source()
here::here("src", "wrangle", "summarise_choices_csv.R") %>% source()
here::here("src", "wrangle", "wrangle_ftm_csv.R") %>% source()
here::here("src", "wrangle", "wrangle_percent_csv.R") %>% source()
here::here("src", "wrangle", "test_correct_dimensions.R") %>% source()

wrangle_all_csv <- function(raw_data, exp_info){
  
  if (length(raw_data) == 0) stop("raw_data has length 0. Check that the data imported properly.")
  
  # wrangle choices data first so that choices_summary can refer to it while creating the list
  choices_temp <- wrangle_choices_csv(raw_data, exp_info)
  
  data <- list(
    demographics = wrangle_demographics_csv(raw_data, exp_info),
    choices = choices_temp,
    choices_summary = summarise_choices_csv(choices_temp, exp_info),
    ftm = wrangle_ftm_csv(raw_data, exp_info),
    percent = wrangle_percent_csv(raw_data, exp_info)
    
  )
}