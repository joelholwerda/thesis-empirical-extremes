
# Experiment info ---------------------------------------------------------

# This information in these files needs to be adapted for each experiment.

# Remember to remove pilot data from the data folder. The script loads all .mat files within the folder.

conditions = list(
  
  "Left-skewed" = list(
    shared_safe = 50,
    shared_risky = c(40, 60),
    context_safe = 80,
    context_risky = c(70, 90),
    worst = 0,
    best = 100
  ),
  
  "Right-skewed" = list(
    shared_safe = 50,
    shared_risky = c(40, 60),
    context_safe = 20,
    context_risky = c(10, 30),
    worst = 0,
    best = 100
  )
)

exp_skew_centre_info <- list(

  experiment_name = "exp_skew_centre",
  
  # Location relative to the R project
  load_location = file.path("data",  "raw_data", "exp_skew_centre"),

  # List of conditions, options, and outcomes
  conditions = conditions,
  
  # The LHS are the indices (choiceTrialInfo) and the RHS are the names (condition) used in the .mat files
  condition_indices = list(`1` = names(conditions)[[1]], 
                           `2` = names(conditions)[[2]]),
  
  condition_names = list(lowRank = names(conditions)[[1]], 
                         highRank = names(conditions)[[2]]),
  
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
    "shared_safe",
    "shared_risky",
    "context_safe",
    "context_risky",
    "worst",
    "best"
  ),
  
  # Names (in order) of trial_type indices
  trial_type_names = c(
    "Shared",
    "Context",
    "Catch",
    "Single"
  ),
  
  # Specify which options are safe or risky
  safe = c(
    "shared_safe",
    "context_safe",
    "worst",
    "best"
  ),
  
  risky = c(
    "shared_risky",
    "context_risky"
  ),
  
  # Specify the correct number of rows for each participant for each data type
  correct_rows = list(
    demographics = 1,
    choices = 330,
    ftm = 6,
    percent = 8
  ),
  
  # Calculate number of conditions, options, and outcomes -- automatic
  n_conditions = conditions %>% length(),
  n_options = conditions %>% map(length),
  n_outcomes = conditions %>% 
    map(~ map_dbl(., length) %>% 
          sum())
  
)
