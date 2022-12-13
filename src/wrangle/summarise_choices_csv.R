summarise_choices_csv <- function(choices, exp_info){
  
  choices_summary <- choices %>%
    filter(trial_type != "Single") %>% 
    group_by(participant, condition, trial_type) %>% 
    summarise(
      risky_prop = mean(risky),
      risky_count = sum(risky),
      n_choices = n()
    ) %>% 
    ungroup()
  
  return(choices_summary)
  
}