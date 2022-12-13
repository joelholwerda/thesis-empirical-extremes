
# Experiment info ---------------------------------------------------------

# This information in these files needs to be adapted for each experiment.

# Remember to remove pilot data from the data folder. The script loads all .csv files within the folder.

conditions = list(
  
  "Left-skewed" = list(
    safe = 50,
    risky = c(30, 70),
    context = c(1, 35:45, 55:65, 80:100),
    worst = 0
  ),
  
  "Right-skewed" = list(
    safe = 50,
    risky = c(30, 70),
    context = c(1:20, 35:45, 55:65, 100),
    worst = 0
  )
)

exp_skew_types_info <- list(

  experiment_name = "exp_skew_types",
  
  # Location relative to the R project
  load_location = file.path("data", "raw_data", "exp_skew_types"),

  # List of conditions, options, and outcomes
  conditions = conditions,
  
  condition_names = list(lowRank = names(conditions)[[1]], 
                         highRank = names(conditions)[[2]]),
  
  # Specify which options are safe or risky
  safe = c(
    "safe"
  ),
  
  risky = c(
    "risky"
  ),
  
  # Specify the correct number of rows for each participant for each data type
  correct_rows = list(
    demographics = 1,
    choices = 220,
    ftm = 4,
    percent = 2
  ),
  
  # Calculate number of conditions, options, and outcomes -- automatic
  n_conditions = conditions %>% length(),
  n_options = conditions %>% map(length),
  n_outcomes = conditions %>% 
    map(~ map_dbl(., length) %>% 
          sum())
  
)