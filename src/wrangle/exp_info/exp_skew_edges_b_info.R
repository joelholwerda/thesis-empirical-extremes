
# Experiment info ---------------------------------------------------------

# This information in these files needs to be adapted for each experiment.

# Remember to remove pilot data from the data folder. The script loads all .mat files within the folder.

conditions = list(
  
  "Right-skewed" = list(
    extreme_safe = 50,
    extreme_risky = c(10, 90),
    context_safe = 20,
    context_risky = c(15, 25)
  ),
  
  "Left-skewed" = list(
    extreme_safe = 50,
    extreme_risky = c(10, 90),
    context_safe = 80,
    context_risky = c(75, 85)
  )
)

exp_skew_edges_b_info <- list(
  
  experiment_name = "exp_skew_edges_b",
  
  # Location relative to the R project
  load_location = file.path("data", "raw_data", "exp_skew_edges_b"),

  # List of conditions, options, and outcomes
  conditions = conditions,
  
  # The LHS are the indices (choiceTrialInfo) and the RHS are the names (condition) used in the .mat files
  condition_indices = list(`1` = names(conditions)[[1]], 
                           `2` = names(conditions)[[2]]),
  
  condition_names = list(low = names(conditions)[[1]], 
                         high = names(conditions)[[2]]),
  
  # Indices of elements in .mat files
  matlab_index = list(
    participant = 1,
    age = 2,
    gender = 3,
    start_time = 4,
    condition = 5,
    choices = 11,
    percent_responses = 12,
    ftm_responses = 13,
    percent_labels = 14,
    prize = 17,
    end_time = 18
  ),
  
  # Names (in order) of columns of choiceTrialInfo
  # Changing names might cause indexing issues 
  choice_names = c(
    "condition",
    "trial",
    "block",
    "trial_in_block",
    "trial_type",
    "stimulus_A",
    "stimulus_B",
    "outcome_A",
    "outcome_B",
    "response",
    "feedback",
    "reaction_time"
  ),
  
  # Order of options in percentResps and ftmVals
  memory_order = c(
    "extreme_safe",
    "extreme_risky",
    "context_safe",
    "context_risky"
  ),
  
  # Names (in order) of trial_type indices
  trial_type_names = c(
    "Extreme",
    "Context",
    "Catch",
    "Single"
  ),
  
  # Specify which options are safe or risky
  safe = c(
    "extreme_safe",
    "context_safe"
  ),
  
  risky = c(
    "extreme_risky",
    "context_risky"
  ),
  
  # Specify the correct number of rows for each participant for each data type
  correct_rows = list(
    demographics = 1,
    choices = 300,
    ftm = 4,
    percent = 6
  ),
  
  # Calculate number of conditions, options, and outcomes -- automatic
  n_conditions = conditions %>% length(),
  n_options = conditions %>% map(length),
  n_outcomes = conditions %>% 
    map(~ map_dbl(., length) %>% 
          sum())
  
)