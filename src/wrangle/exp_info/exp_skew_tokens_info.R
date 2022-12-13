
# Experiment info ---------------------------------------------------------

# This information in these files needs to be adapted for each experiment.

# Remember to remove pilot data from the data folder. The script loads all .csv files within the folder.

conditions = list(
  
  "Left-skewed" = list(
    low_value_safe = 10,
    low_value_risky = c(0, 20),
    high_value_safe = 90,
    high_value_risky = c(80, 100),
    decision_safe = 50,
    decision_risky = c(30, 70)
  ),
  
  "Right-skewed" = list(
    low_value_safe = 10,
    low_value_risky = c(0, 20),
    high_value_safe = 90,
    high_value_risky = c(80, 100),
    decision_safe = 50,
    decision_risky = c(30, 70)
  )
)

exp_skew_tokens_info <- list(

  experiment_name = "exp_skew_tokens",
  
  # Location relative to the R project
  load_location = file.path("data", "raw_data", "exp_skew_tokens"),

  # List of conditions, options, and outcomes
  conditions = conditions,
  
  condition_names = list(lowRank = names(conditions)[[1]], 
                         highRank = names(conditions)[[2]]),
  
  # Specify which options are safe or risky
  safe = c(
    "low_value_safe",
    "high_value_safe",
    "decision_safe"
  ),
  
  risky = c(
    "low_value_risky",
    "high_value_risky",
    "decision_risky"
  ),
  
  # Specify the correct number of rows for each participant for each data type
  correct_rows = list(
    demographics = 1,
    choices = 204,
    ftm = 6,
    percent = 9
  ),
  
  # Calculate number of conditions, options, and outcomes -- automatic
  n_conditions = conditions %>% length(),
  n_options = conditions %>% map(length),
  n_outcomes = conditions %>% 
    map(~ map_dbl(., length) %>% 
          sum())
  
)