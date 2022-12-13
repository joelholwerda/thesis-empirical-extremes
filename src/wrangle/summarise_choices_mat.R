summarise_choices_mat <- function(choices, exp_info){
  
  # Filter out single and catch trials, recode as factors
  choices_filtered <- choices %>%
    filter(trial_type != "Single") %>%
    mutate(condition = condition %>% factor(),
           stimulus_A = stimulus_A %>% factor(),
           stimulus_B = stimulus_B %>% factor(),
           trial_type = trial_type %>% as.factor(),
           choice = choice %>% factor()
    )
  
  # Summarise risky choices as a proportion and count
  choices_summary <- choices_filtered %>%
    group_by(participant, condition, trial_type) %>% 
    summarise(risky_prop = mean(risky),
              risky_count = sum(risky),
              n_choices = length(risky)) %>% 
    ungroup()

  return(choices_summary)
  
}