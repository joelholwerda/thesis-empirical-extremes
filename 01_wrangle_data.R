# Experiments ------------------------------------------------------------------

# 1. `exp_skew_centre`: Manipulated rank and distance from the average by including an additional pair above or below the shared pair
# 2. `exp_variance`: Manipulated distance from the average by manipulating the variance of each option
# 3. `exp_skew_edges`: Manipulated distance from the average by including outcomes close to the best or worst outcome
# 4. `exp_skew_tokens`: Manipulated token-based extremity while controlling the number of types
# 5. `exp_skew_types`: Manipulated type-based extremity while controlling the number of tokens
# 6. `exp_temporal`: Manipulated alternating or random presentation of gains and losses

# Setup ------------------------------------------------------------------------

set.seed(1234)

library(tidyverse)
library(lubridate)
library(forcats)
library(R.matlab) 
library(jsonlite)
library(standardize)
library(rlang)
library(here)

# Source experiment info files and wrangling functions
here::here("src", "wrangle", "exp_info", "exp_skew_centre_info.R") %>% source()
here::here("src", "wrangle", "exp_info", "exp_variance_info.R") %>% source()
here::here("src", "wrangle", "exp_info", "exp_skew_edges_a_info.R") %>% source()
here::here("src", "wrangle", "exp_info", "exp_skew_edges_b_info.R") %>% source()
here::here("src", "wrangle", "exp_info", "exp_skew_tokens_info.R") %>% source()
here::here("src", "wrangle", "exp_info", "exp_skew_types_info.R") %>% source()
here::here("src", "wrangle", "exp_info", "exp_temporal_info.R") %>% source()
here::here("src", "wrangle", "import_mat.R") %>% source()
here::here("src", "wrangle", "import_csv.R") %>% source()
here::here("src", "wrangle", "wrangle_all_mat.R") %>% source()
here::here("src", "wrangle", "wrangle_all_csv.R") %>% source()
here::here("src", "wrangle", "format_conditions.R") %>% source()
here::here("src", "wrangle", "table_to_string.R") %>% source()
here::here("src", "wrangle", "detect_missing_memory_data.R") %>% source()
here::here("src", "wrangle", "detect_incomplete_data.R") %>% source()
here::here("src", "wrangle", "summarise_demographics.R") %>% source()
here::here("src", "wrangle", "summarise_exclusions.R") %>% source()
here::here("src", "wrangle", "generate_catch_permutations.R") %>% source()
here::here("src", "utilities", "save_tidy_data.R") %>% source()

# Column names of categorical variables
categorical_variables <- c(
  "experiment", 
  "condition", 
  "trial_type", 
  "stimulus", 
  "option_value", 
  "stimulus_value", 
  "response_recode", 
  "response_recode_value"
)

temp <- list()

# Skewed distributions (shared centre) -----------------------------------------

# Import data from .mat files and do initial wrangling with generic wrangling functions
exp_skew_centre <- import_mat(exp_skew_centre_info) %>% 
  wrangle_all_mat(exp_skew_centre_info)

# Recode choices 
exp_skew_centre$choices_summary <- exp_skew_centre$choices_summary %>% 
  filter(!trial_type %in% c("Extreme", "Catch", "Single")) %>% 
  mutate(
    option_value = case_when(
      ((condition == "Left-skewed" & trial_type == "Shared") | 
          (condition == "Right-skewed" & trial_type == "Context")) ~ "Low value",
      ((condition == "Left-skewed" & trial_type == "Context") | 
          (condition == "Right-skewed" & trial_type == "Shared")) ~ "High value"
    ) %>% 
      format_conditions()
  )

# Recode ftm responses (e.g. responses that were not experienced as "Other")
exp_skew_centre$ftm_recode <- exp_skew_centre$ftm %>% 
  filter(stimulus %in% c("shared_risky", "context_risky")) %>% 
  mutate(
    response_recode = case_when(
      condition == "Left-skewed" & stimulus == "shared_risky" & response == 60 |
        condition == "Left-skewed" & stimulus == "context_risky" & response == 70 |
        condition == "Right-skewed" & stimulus == "shared_risky" & response == 40 |
        condition == "Right-skewed" & stimulus == "context_risky" & response == 30 ~ "Non-extreme",
      condition == "Left-skewed" & stimulus == "shared_risky" & response == 40 |
        condition == "Left-skewed" & stimulus == "context_risky" & response == 90 |
        condition == "Right-skewed" & stimulus == "shared_risky" & response == 60 |
        condition == "Right-skewed" & stimulus == "context_risky" & response == 10 ~ "Extreme",
      TRUE ~ "Other"
    ) %>% 
      as.factor(),
    response_recode_value = case_when(
      response %in% c(10, 40, 70) & response_recode != "Other" ~ "Worse",
      response %in% c(30, 60, 90) & response_recode != "Other" ~ "Better",
      TRUE ~ "Other"
    ) %>% 
      format_conditions(),
    stimulus = stimulus %>% 
      str_replace(pattern = "_risky", replacement = "") %>%
      str_replace_all(pattern = "_", replacement = " ") %>% 
      str_to_title() %>% 
      factor(),
    stimulus_value = case_when(
      ((condition == "Left-skewed" & stimulus == "Shared") | 
          (condition == "Right-skewed" & stimulus == "Context")) ~ "Low value",
      ((condition == "Left-skewed" & stimulus == "Context") | 
          (condition == "Right-skewed" & stimulus == "Shared")) ~ "High value"
    ) %>% 
      format_conditions()
  )

# Recode percentage responses 
exp_skew_centre$percent_recode <- exp_skew_centre$percent %>% 
  filter(stimulus %in% c("shared_risky", "context_risky")) %>%
  mutate(
    label = case_when(
      condition == "Left-skewed" & label %in% c(40, 90) ~ "extreme",
      condition == "Left-skewed" & label %in% c(60, 70) ~ "nonextreme",
      condition == "Right-skewed" & label %in% c(10, 60) ~ "extreme",
      condition == "Right-skewed" & label %in% c(30, 40) ~ "nonextreme"
    )
  ) %>% 
  spread(key = label, value = response) %>% 
  mutate(
    stimulus = stimulus %>% 
      str_replace(pattern = "_risky", replacement = "") %>%
      str_replace(pattern = "_", replacement = " ") %>% 
      str_to_title(),
    stimulus_value = case_when(
      ((condition == "Left-skewed" & stimulus == "Shared") | 
          (condition == "Right-skewed" & stimulus == "Context")) ~ "Low value",
      ((condition == "Left-skewed" & stimulus == "Context") | 
          (condition == "Right-skewed" & stimulus == "Shared")) ~ "High value"
    ) %>% 
      format_conditions(),
    extreme_minus_non = extreme - `nonextreme`,
    better_minus_worse = case_when(
      stimulus_value == "Low value" ~ `nonextreme` - extreme,
      stimulus_value == "High value" ~ extreme - `nonextreme`
    )
  )

# Exclude participants than chose the better option on less than 60 percent of catch trials
failed_catch <- exp_skew_centre$choices %>%
  filter(trial_type == "Catch") %>% 
  mutate(
    chose_better = case_when(
      # Response determines whether they chose option_A or option_B
      response == 1 & outcome_A > outcome_B | response == 2 & outcome_A < outcome_B ~ TRUE,
      response == 1 & outcome_A < outcome_B | response == 2 & outcome_A > outcome_B ~ FALSE
    )
  ) %>%
  group_by(participant, condition) %>% 
  summarise(prop_better = mean(chose_better)) %>% 
  ungroup() %>% 
  # Filter out participants that scored higher than 0.6
  filter(prop_better < 0.6)

# Use function to summarise demographics data
# There was no missing data but four participants were excluded based on performance
exp_skew_centre$demographics_summary <- summarise_demographics(
  demographics_data = exp_skew_centre$demographics,
  incomplete_choices =  detect_incomplete_data(exp_skew_centre, n_choices = 330, n_ftm = 6, n_percent = 8),
  missing_memory_data = detect_missing_memory_data(exp_skew_centre),
  failed_catch = failed_catch
)

# Combine into a vector of participants to exclude
exp_skew_centre$exclusions <- summarise_exclusions(exp_skew_centre$demographics_summary)

### Format categorical variables ----

# Format and append contrasts for each categorical variable in each tibble
exp_skew_centre[names(exp_skew_centre) != "exclusions"] <- exp_skew_centre[names(exp_skew_centre) != "exclusions"] %>% map(
  ~ mutate_at(
    .x, 
    vars(any_of(categorical_variables)), 
    ~ format_conditions(.) %>% named_contr_sum(., return_contr = FALSE)
  )
)

# Save wrangled data to .csv files in the tidy_data folder
save_tidy_data(exp_skew_centre)

# Variance ---------------------------------------------------------------------

# Import data from .mat files and do initial wrangling with generic wrangling functions
exp_variance <- import_mat(exp_variance_info) %>% 
  wrangle_all_mat(exp_variance_info)

# Filter context and catch trials from choices_summary
exp_variance$choices_summary <- exp_variance$choices_summary %>% filter(trial_type %in% c("High value", "Low value"))

# Recode ftm responses (e.g. responses that were not experienced as "Other")
exp_variance$ftm_recode <- exp_variance$ftm %>% 
  filter(stimulus %in% c("low_value_risky", "high_value_risky")) %>%
  mutate(
    response_recode = case_when(
      condition == "Low variance" & stimulus == "low_value_risky" & response == 30 |
        condition == "Low variance" & stimulus == "high_value_risky" & response == 70 |
        condition == "High variance" & stimulus == "low_value_risky" & response == 40 |
        condition == "High variance" & stimulus == "high_value_risky" & response == 60 ~ "Non-extreme",
      condition == "Low variance" & stimulus == "low_value_risky" & response == 20 |
        condition == "Low variance" & stimulus == "high_value_risky" & response == 80 |
        condition == "High variance" & stimulus == "low_value_risky" & response == 10 |
        condition == "High variance" & stimulus == "high_value_risky" & response == 90 ~ "Extreme",
      TRUE ~ "Other"
    ) %>%
      as.factor(),
    response_recode_value = case_when(
      stimulus == "low_value_risky" & response_recode == "Extreme" |
        stimulus == "high_value_risky" & response_recode == "Non-extreme" ~ "Worse",
      stimulus == "low_value_risky" & response_recode == "Non-extreme" |
        stimulus == "high_value_risky" & response_recode == "Extreme" ~ "Better",
      TRUE ~ "Other"
    ) %>%
      as.factor(),
    stimulus = stimulus %>% 
      str_replace(pattern = "_risky", replacement = "") %>%
      str_replace_all(pattern = "_", replacement = " ") %>% 
      format_conditions()
  )

# Recode percentage responses 
exp_variance$percent_recode <- exp_variance$percent %>% 
  filter(stimulus %in% c("low_value_risky", "high_value_risky")) %>%
  mutate(
    label = case_when(
      label %in% c(10, 20, 60, 70) ~ "worse",
      label %in% c(30, 40, 80, 90) ~ "better"
    )
  ) %>% 
  spread(key = label, value = response) %>% 
  mutate(
    better_minus_worse = better - worse,
    extreme_minus_non = case_when(
      stimulus == "low_value_risky" ~ worse - better,
      stimulus == "high_value_risky" ~ better - worse
    ),
    stimulus = stimulus %>% 
      str_replace(pattern = "_risky", replacement = "") %>%
      str_replace(pattern = "_", replacement = " ") %>% 
      format_conditions()
  )

### Wrangle participants ----

# Exclude participants than chose the better option on less than 60 percent of 
# choices between the Low- and High-average options
failed_catch <- exp_variance$choices %>%
  filter(
    trial_type == "Catch",
    # Don't include choices involving the extreme options (that have different SD)
    !stimulus_A %in% c("extreme_safe", "extreme_risky"),
    !stimulus_B %in% c("extreme_safe", "extreme_risky")
  ) %>% 
  mutate(
    chose_better = case_when(
      # Response determines whether they chose option_A or option_B
      response == 1 & outcome_A > outcome_B | response == 2 & outcome_A < outcome_B ~ TRUE,
      response == 1 & outcome_A < outcome_B | response == 2 & outcome_A > outcome_B ~ FALSE
    )
  ) %>%
  group_by(participant, condition) %>% 
  summarise(prop_better = mean(chose_better)) %>% 
  ungroup() %>% 
  # Filter out participants that scored higher than 0.6
  filter(prop_better < 0.6)

# Use function to summarise demographics data
# There was no missing data but four participants were excluded based on performance
exp_variance$demographics_summary <- summarise_demographics(
  demographics_data = exp_variance$demographics,
  incomplete_choices = detect_incomplete_data(exp_variance, n_choices = 360, n_ftm = 6, n_percent = 9),
  missing_memory_data = detect_missing_memory_data(exp_variance),
  failed_catch = failed_catch
)

# Combine into a vector of participants to exclude
exp_variance$exclusions <- summarise_exclusions(exp_variance$demographics_summary)

### Format categorical variables ----

# Format and append contrasts for each categorical variable in each tibble
exp_variance[names(exp_variance) != "exclusions"] <- exp_variance[names(exp_variance) != "exclusions"] %>% map(
  ~ mutate_at(
    .x, 
    vars(any_of(categorical_variables)), 
    ~ format_conditions(.) %>% named_contr_sum(., return_contr = FALSE)
  )
)

# Save wrangled data to .csv files in the tidy_data folder
save_tidy_data(exp_variance)


# Skewed distributions (shared edges) ------------------------------------------

# Import data from .mat files and do initial wrangling with generic wrangling functions
exp_skew_edges_a <- import_mat(exp_skew_edges_a_info) %>% 
  wrangle_all_mat(exp_skew_edges_a_info)

exp_skew_edges_b <- import_mat(exp_skew_edges_b_info) %>% 
  wrangle_all_mat(exp_skew_edges_b_info)

# Combine experiment 1a and 1b
exp_skew_edges <- map2(
  .x = exp_skew_edges_a, 
  .y = exp_skew_edges_b, 
  ~ bind_rows(
    "Experiment a" = .x, 
    "Experiment b" = .y, 
    .id = "experiment"
  )
)

# Recode experiment as a factor and indicate whether participants were in experiment A or B
exp_skew_edges$demographics <- exp_skew_edges$demographics %>% 
  mutate(
    experiment = experiment %>% factor(),
    participant = case_when(
      experiment == "Experiment a" ~ paste(participant, "a", sep = ""),
      experiment == "Experiment b" ~ paste(participant, "b", sep = "")
    )
  )

exp_skew_edges$choices_summary <- exp_skew_edges$choices_summary %>% 
  mutate(
    experiment = experiment %>% factor(),
    participant = case_when(
      experiment == "Experiment a" ~ paste(participant, "a", sep = ""),
      experiment == "Experiment b" ~ paste(participant, "b", sep = "")
    ),
    option_value = case_when(
      trial_type == "Context" & condition == "Right-skewed" ~ "High value",
      trial_type == "Context" & condition == "Left-skewed" ~ "Low value",
      TRUE ~ as.character(trial_type)
    ) %>% as.factor()
  )

exp_skew_edges$ftm <- exp_skew_edges$ftm %>% 
  mutate(
    experiment = experiment %>% factor(),
    participant = case_when(
      experiment == "Experiment a" ~ paste(participant, "a", sep = ""),
      experiment == "Experiment b" ~ paste(participant, "b", sep = "")
    )
  )

exp_skew_edges$percent <- exp_skew_edges$percent %>% 
  mutate(
    experiment = experiment %>% factor(),
    participant = case_when(
      experiment == "Experiment a" ~ paste(participant, "a", sep = ""),
      experiment == "Experiment b" ~ paste(participant, "b", sep = "")
    )
  )

# Recode ftm responses for the extreme risky option (e.g. responses that were not experienced as "Other")
exp_skew_edges$ftm_recode <- exp_skew_edges$ftm %>% 
  filter(stimulus %in% c("extreme_risky", "context_risky")) %>% 
  mutate(
    # Label outcomes that were not experienced for the context risky option as "other"
    response_points = case_when(
      experiment == "Experiment a" & response %in% c(10, 20, 40, 60, 80, 90) |
        experiment == "Experiment b" & response %in% c(10, 15, 25, 75, 85, 90) ~ paste(response, "Points"),
      TRUE ~ "Other") %>% 
      as.factor(),
    # Translate into whether the participant recalled the extreme or non-extreme outcome
    response_recode = case_when(
      stimulus == "context_risky" & condition == "Right-skewed" & response_points %in% c("15 Points", "20 Points") |
        stimulus == "context_risky" & condition == "Left-skewed" & response_points %in% c("80 Points", "85 Points") |
        stimulus == "extreme_risky" & condition == "Right-skewed" & response_points == "90 Points" | 
        stimulus == "extreme_risky" & condition == "Left-skewed" & response_points == "10 Points" ~ "Extreme",
      stimulus == "context_risky" & condition == "Right-skewed" & response_points %in% c("25 Points", "40 Points") |
        stimulus == "context_risky" & condition == "Left-skewed" & response_points %in% c("60 Points", "75 Points") |
        stimulus == "extreme_risky" & condition == "Right-skewed" & response_points == "10 Points" | 
        stimulus == "extreme_risky" & condition == "Left-skewed" & response_points == "90 Points" ~ "Non-extreme",
      TRUE ~ "Other"
    ) %>% 
      as.factor(),
    stimulus_value = case_when(
      stimulus == "context_risky" & condition == "Right-skewed" ~ "High value",
      stimulus == "context_risky" & condition == "Left-skewed" ~ "Low value",
      TRUE ~ "Extreme"
    ) %>% format_conditions(),
    response_recode_value = case_when(
      condition == "Right-skewed" & response_recode ==  "Extreme" | 
        condition == "Left-skewed" & response_recode ==  "Non-extreme" ~ "Better",
      condition == "Right-skewed" & response_recode ==  "Non-extreme" | 
        condition == "Left-skewed" & response_recode ==  "Extreme" ~ "Worse",
      TRUE ~ "Other"
    ) %>% format_conditions(),
    stimulus = case_when(stimulus == "extreme_risky" ~ "Shared", stimulus == "context_risky" ~ "Context")
  )

# Recode percentage responses for the context risky option
exp_skew_edges$percent_recode <- exp_skew_edges$percent %>% 
  filter(stimulus %in% c("context_risky", "extreme_risky")) %>%
  mutate(
    label = case_when(
      label %in% c(10, 15, 20, 60, 75) ~ "worse",
      label %in% c(25, 40, 80, 85, 90) ~ "better"
    )
  ) %>%
  spread(key = label, value = response) %>% 
  mutate(
    # Translate raw percentage responses into the responses for the better minus worse outcome
    # and into the percentage reported for the extreme minus non-extreme outcomes
    better_minus_worse = better - worse,
    extreme_minus_non = case_when(
      condition == "Right-skewed" ~ worse - better,
      condition == "Left-skewed" ~ better - worse
    ),
    stimulus_value = case_when(
      stimulus == "context_risky" & condition == "Right-skewed" ~ "High value",
      stimulus == "context_risky" & condition == "Left-skewed" ~ "Low value",
      TRUE ~ "Extreme"
    ) %>% format_conditions(),
    stimulus = case_when(stimulus == "extreme_risky" ~ "Shared", stimulus == "context_risky" ~ "Context")
  )

### Wrangle participants ----

# Check for incomplete data and exclusions - there were none in this experiment
incomplete_choices <- exp_skew_edges$choices_summary %>% 
  group_by(participant) %>% 
  summarise(n = sum(n_choices)) %>% 
  ungroup() %>% 
  filter(
    !(str_detect(participant, "a") & n == 200 | 
        str_detect(participant, "b") & n == 260)
  )

# Use function to summarise demographics data
# None of the options was strictly better than the others so performance-based exclusions were not calculated
exp_skew_edges$demographics_summary <- summarise_demographics(
  demographics_data = exp_skew_edges$demographics,
  incomplete_choices = incomplete_choices,
  missing_memory_data = detect_missing_memory_data(exp_skew_edges)
)

# Combine into a vector of participants to exclude - there were none in this experiment
exp_skew_edges$exclusions <- summarise_exclusions(exp_skew_edges$demographics_summary)

# Summarise demographics for part A and B separately for inclusion in methods
exp_skew_edges_a$demographics_summary <- summarise_demographics(
  demographics_data = exp_skew_edges_a$demographics,
  incomplete_choices = incomplete_choices,
  missing_memory_data = detect_missing_memory_data(exp_skew_edges_a)
)

exp_skew_edges_b$demographics_summary <- summarise_demographics(
  demographics_data = exp_skew_edges_b$demographics,
  incomplete_choices = incomplete_choices,
  missing_memory_data = detect_missing_memory_data(exp_skew_edges_b)
)

### Format categorical variables ----

# Format and append contrasts for each categorical variable in each tibble
exp_skew_edges[names(exp_skew_edges) != "exclusions"] <- exp_skew_edges[names(exp_skew_edges) != "exclusions"] %>% map(
  ~ mutate_at(
    .x, 
    vars(any_of(categorical_variables)), 
    ~ format_conditions(.) %>% named_contr_sum(., return_contr = FALSE)
  )
)

# Save wrangled data to .csv files in the tidy_data folder
save_tidy_data(exp_skew_edges)

# Skewed distributions (tokens) ------------------------------------------------

# Import data from .csv files and do initial wrangling with generic wrangling functions
exp_skew_tokens <- import_csv(exp_skew_tokens_info) %>% 
  wrangle_all_csv(exp_skew_tokens_info)

exp_skew_tokens$choices_summary <- exp_skew_tokens$choices_summary %>% 
  mutate(
    trial_type = case_when(
      as.character(trial_type) == "Decision" ~ "Shared",
      TRUE ~ as.character(trial_type)
    ) %>% format_conditions(),
    option_value = case_when(
      as.character(trial_type) == "Shared" ~ "Medium value",
      TRUE ~ as.character(trial_type)
    ) %>% factor(levels = c("Low value", "Medium value", "High value"), ordered = TRUE)
  )

# Recode ftm responses (e.g. responses that were not experienced as "Other")
exp_skew_tokens$ftm_recode <- exp_skew_tokens$ftm %>% 
  filter(
    stimulus %in% c("low_risky", "high_risky", "decision_risky")
  ) %>% 
  mutate(
    response_recode_value = case_when(
      response_clean %in% c(0, 30, 80) ~ "Worse",
      response_clean %in% c(20, 70, 100) ~ "Better",
      TRUE ~ "Other"
    ) %>% 
      format_conditions(),
    stimulus = case_when(
      stimulus == "low_risky" ~ "Low Value",
      stimulus == "high_risky" ~ "High Value",
      stimulus == "decision_risky" ~ "Shared"
    ) %>% 
      format_conditions(),
    stimulus_value = case_when(
      as.character(stimulus) == "Shared" ~ "Medium value",
      TRUE ~ as.character(stimulus)
    ) %>% factor(levels = c("Low value", "Medium value", "High value"), ordered = TRUE)
  )

# Recode percentage responses 
exp_skew_tokens$percent_recode <- exp_skew_tokens$percent %>%
  select(-response, -label) %>%
  mutate(response_clean = as.numeric(response_clean)) %>% 
  spread(key = label_type, value = response_clean) %>% 
  mutate(
    better_minus_worse = high - low,
    stimulus = case_when(
      stimulus == "low_risky" ~ "Low Value",
      stimulus == "high_risky" ~ "High Value",
      stimulus == "decision_risky" ~ "Shared"
    ) %>% 
      format_conditions(),
    stimulus_value = case_when(
      as.character(stimulus) == "Shared" ~ "Medium value",
      TRUE ~ as.character(stimulus)
    ) %>% factor(levels = c("Low value", "Medium value", "High value"), ordered = TRUE)
  )

### Wrangle participants ----

# Check for participants that did not finish the task (this data is excluded at this
# point because it was causing trouble with the catch trial assessment).
missing_memory_data <- detect_missing_memory_data(exp_skew_tokens)
incomplete_choices <- detect_incomplete_data(exp_skew_tokens, n_choices = 204, n_ftm = 6, n_percent = 9)
temp_exclusions <- full_join(missing_memory_data, incomplete_choices) %>% filter(!is.na(participant))

# Exclude participants than chose the better option on less than 60 percent of 
# catch trials. The options presented on catch trials was not recorded so the
# percentage of choices for the better option is recovered using the feedback

# Generate data.frame with the number of choices for the better option that are
# consistent with the number of choices for each option.
catch_permutations <- generate_catch_permutations()

exp_skew_tokens$catch_summary <- exp_skew_tokens$choices %>%
  filter(trial_type == "Catch", !participant %in% temp_exclusions$participant) %>% 
  mutate(
    # Specify which catch trials the participant chose each option
    chose_low_safe = ifelse(feedback == 10, TRUE, FALSE),
    chose_low_risky = ifelse(feedback %in% c(0, 20), TRUE, FALSE),
    chose_medium_safe = ifelse(feedback == 50, TRUE, FALSE),
    chose_medium_risky = ifelse(feedback %in% c(30, 70), TRUE, FALSE),
    chose_high_safe = ifelse(feedback == 90, TRUE, FALSE),
    chose_high_risky = ifelse(feedback %in% c(80, 100), TRUE, FALSE)
  ) %>%
  group_by(participant, condition) %>%
  # Count the number of choices for each option
  summarise(
    n_low_safe = sum(chose_low_safe),
    n_low_risky = sum(chose_low_risky),
    n_medium_safe = sum(chose_medium_safe),
    n_medium_risky = sum(chose_medium_risky),
    n_high_safe = sum(chose_high_safe),
    n_high_risky = sum(chose_high_risky)
  ) %>% 
  ungroup() %>% 
  # Initialise columns
  mutate(
    min_prop_better = NA,
    max_prop_better = NA
  )

# Loop through each participant and retrieve the rows from catch_permutations that
# are compatible with the number of choices they made for each option
for (participant in 1:dim(exp_skew_tokens$catch_summary)[1]) {
  n_better_possible <- catch_permutations[
    catch_permutations$n_low_safe == exp_skew_tokens$catch_summary$n_low_safe[participant] &
      catch_permutations$n_low_risky == exp_skew_tokens$catch_summary$n_low_risky[participant] &
      catch_permutations$n_medium_safe == exp_skew_tokens$catch_summary$n_medium_safe[participant] &
      catch_permutations$n_medium_risky == exp_skew_tokens$catch_summary$n_medium_risky[participant] &
      catch_permutations$n_high_safe == exp_skew_tokens$catch_summary$n_high_safe[participant] &
      catch_permutations$n_high_risky == exp_skew_tokens$catch_summary$n_high_risky[participant],
  ]
  
  # Determine the min and max values that are compatible with their choices. This
  # usually results in a narrow range for participants who performed well and a larger
  # range for participants further away from the edges. Incidentally, this pattern is
  # reminiscent of the edge resolution effects found in discrimination studies
  exp_skew_tokens$catch_summary$min_prop_better[participant] <- min(n_better_possible$n_better) / 24
  exp_skew_tokens$catch_summary$max_prop_better[participant] <- max(n_better_possible$n_better) / 24
}

# Filter out participants that definitely scored higher than 0.6
failed_catch <- exp_skew_tokens$catch_summary %>% 
  filter(min_prop_better < 0.6) %>%
  select(participant, condition, min_prop_better, max_prop_better)

# Check for participants that failed the colour-blindness questionnaire
failed_colour_blindness <- exp_skew_tokens$demographics %>% 
  filter(colour_blindness != 8) %>% 
  select(participant, condition, colour_blindness)

# Convert age from character to numeric
exp_skew_tokens$demographics <- exp_skew_tokens$demographics %>%  mutate(age = age %>% as.numeric())

# Use function to summarise demographics data
# Data was missing for 7 participants and data from 3 was excluded because they 
# failed the colour blindness test and 7 were excluded for failing the catch trials
exp_skew_tokens$demographics_summary <- summarise_demographics(
  demographics_data = exp_skew_tokens$demographics,
  incomplete_choices = incomplete_choices,
  missing_memory_data = missing_memory_data,
  failed_catch = failed_catch,
  failed_colour_blindness = failed_colour_blindness
)

# Combine into a vector of participants to exclude
exp_skew_tokens$exclusions <- summarise_exclusions(exp_skew_tokens$demographics_summary)

### Format categorical variables ----

# Format and append contrasts for each categorical variable in each tibble
exp_skew_tokens[names(exp_skew_tokens) != "exclusions"] <- exp_skew_tokens[names(exp_skew_tokens) != "exclusions"] %>% map(
  ~ mutate_at(
    .x, 
    vars(any_of(categorical_variables[!categorical_variables %in% c("option_value", "stimulus_value")])), 
    ~ format_conditions(.) %>% named_contr_sum(., return_contr = FALSE)
  )
)

# Save wrangled data to .csv files in the tidy_data folder
save_tidy_data(exp_skew_tokens)

# Skewed distributions (types) -------------------------------------------------

# Import data from .csv files and do initial wrangling with generic wrangling functions
exp_skew_types <- import_csv(exp_skew_types_info) %>% 
  wrangle_all_csv(exp_skew_types_info)

# Recode ftm responses (e.g. responses that were not experienced as "Other")
exp_skew_types$ftm_recode <- exp_skew_types$ftm %>% 
  filter(stimulus == "risky") %>%
  mutate(
    response_recode = case_when(
      response_clean == 30 ~ "30 Points",
      response_clean == 70 ~ "70 Points",
      TRUE ~ "Other"
    ) %>% 
      as.factor(),
    response_recode_value = case_when(
      response_clean == 30 ~ "Worse",
      response_clean == 70 ~ "Better",
      TRUE ~ "Other"
    ) %>% 
      format_conditions(),
    stimulus = "Shared" %>% format_conditions()
  )

# Recode percentage responses 
exp_skew_types$percent_recode <- exp_skew_types$percent %>%
  select(-stimulus, -response) %>%
  spread(key = label, value = response_clean) %>% 
  mutate(
    better_minus_worse = `70` - `30`, 
    stimulus = "Shared" %>% format_conditions()
  )

### Wrangle participants ----

# Participant 13837 exited the experiment after completion but before submitting
# for payment and therefore had to be added manually and has no prize data
participant13837 <- tibble(
  participant = "13837",
  condition = "Right-skewed",
  age = "36",
  gender = "Male"
)

exp_skew_types$demographics <- full_join(exp_skew_types$demographics, participant13837) %>% 
  mutate(age = age %>% as.numeric(), condition = condition %>% format_conditions())

# Exclude participants than chose the better option on less than 60 percent of 
# choices between the Context and Worst options
failed_catch <- exp_skew_types$choices %>%
  filter(trial_type == "Context") %>% 
  mutate(
    chose_better = choice == "context"
  ) %>%
  group_by(participant, condition) %>% 
  summarise(prop_better = mean(chose_better)) %>% 
  ungroup() %>% 
  # Filter out participants that scored higher than 0.6
  filter(prop_better < 0.6)

# Check for participants that failed the colour-blindness questionnaire
failed_colour_blindness <- exp_skew_types$demographics %>% 
  filter(colour_blindness != 8) %>% 
  select(participant, condition, colour_blindness)

# Use function to summarise demographics data
# Data was missing for 5 participants and data from 1 was excluded because they failed the colour blindness test
exp_skew_types$demographics_summary <- summarise_demographics(
  demographics_data = exp_skew_types$demographics,
  incomplete_choices =  detect_incomplete_data(exp_skew_types, n_choices = 220, n_ftm = 4, n_percent = 2),
  missing_memory_data = detect_missing_memory_data(exp_skew_types),
  failed_catch = failed_catch,
  failed_colour_blindness = failed_colour_blindness
)

# Combine into a vector of participants to exclude
exp_skew_types$exclusions <- summarise_exclusions(exp_skew_types$demographics_summary)

### Format categorical variables ----

# Format and append contrasts for each categorical variable in each tibble
exp_skew_types[names(exp_skew_types) != "exclusions"] <- exp_skew_types[names(exp_skew_types) != "exclusions"] %>% map(
  ~ mutate_at(
    .x, 
    vars(any_of(categorical_variables[categorical_variables != "stimulus"])), 
    ~ format_conditions(.) %>% named_contr_sum(., return_contr = FALSE)
  )
)

# Save wrangled data to .csv files in the tidy_data folder
save_tidy_data(exp_skew_types)

# Temporal ---------------------------------------------------------------------

# Import data from .mat files and do initial wrangling with generic wrangling functions
exp_temporal <- import_mat(exp_temporal_info) %>% 
  wrangle_all_mat(exp_temporal_info)

# Recode choices
exp_temporal$choices_summary <- exp_temporal$choices_summary %>%
  filter(trial_type != "Single") %>% 
  mutate(
    option_value = case_when(
      trial_type == "Losses" ~ "Low value",
      trial_type == "Gains" ~ "High value"
    ) %>% 
      format_conditions()
  )

# Recode ftm responses (e.g. responses that were not experienced as "Other")
exp_temporal$ftm_recode <- exp_temporal$ftm %>% 
  filter(stimulus %in% c("losses_risky", "gains_risky")) %>% 
  mutate(
    response_recode = case_when(
      stimulus == "losses_risky" & response == -40 ~ "Extreme",
      stimulus == "losses_risky" & response == -10 ~ "Non-extreme",
      stimulus == "gains_risky" & response == 60 ~ "Non-extreme",
      stimulus == "gains_risky" & response == 90 ~ "Extreme",
      TRUE ~ "Other"
    ),
    response_recode_value = case_when(
      stimulus == "losses_risky" & response_recode == "Extreme" |
        stimulus == "gains_risky" & response_recode == "Non-extreme" ~ "Worse",
      stimulus == "losses_risky" & response_recode == "Non-extreme" |
        stimulus == "gains_risky" & response_recode == "Extreme" ~ "Better",
      TRUE ~ "Other"
    ) %>% 
      format_conditions(),
    stimulus = stimulus %>% 
      str_replace(pattern = "_risky", replacement = "") %>%
      str_replace_all(pattern = "_", replacement = " ") %>% 
      str_to_title() %>% 
      factor(),
    stimulus_value = case_when(
      stimulus == "Losses" ~ "Low value",
      stimulus == "Gains" ~ "High value"
    ) %>% 
      format_conditions()
  )

# Recode percentage responses 
exp_temporal$percent_recode <- exp_temporal$percent %>% 
  filter(stimulus %in% c("losses_risky", "gains_risky")) %>%
  mutate(
    label = case_when(
      label %in% c(-40, 90) ~ "extreme",
      label %in% c(-10, 60) ~ "non-extreme"
    )
  ) %>% 
  spread(key = label, value = response) %>% 
  mutate(
    stimulus = stimulus %>% 
      str_replace(pattern = "_risky", replacement = "") %>%
      str_replace(pattern = "_", replacement = " ") %>% 
      str_to_title(),
    stimulus_value = case_when(
      stimulus == "Losses" ~ "Low value",
      stimulus == "Gains" ~ "High value"
    ) %>% 
      format_conditions(),
    extreme_minus_non = extreme - `non-extreme`,
    better_minus_worse = case_when(
      stimulus_value == "Low value" ~ `non-extreme` - extreme,
      stimulus_value == "High value" ~ extreme - `non-extreme`
    )
  )

### Wrangle participants ----

# Check for incomplete data and exclusions -- No data from participants was excluded
# There were no catch trials so no participants were excluded based on performance
exp_temporal$demographics_summary <- summarise_demographics(
  demographics_data = exp_temporal$demographics,
  incomplete_choices =  detect_incomplete_data(exp_temporal, n_choices = 200, n_ftm = 4, n_percent = 6),
  missing_memory_data = detect_missing_memory_data(exp_temporal)
)

# Combine into a vector of participants to exclude -- No data from participants was excluded
exp_temporal$exclusions <- summarise_exclusions(exp_temporal$demographics_summary)

### Format categorical variables ----

# Format and append contrasts for each categorical variable in each tibble
exp_temporal[names(exp_temporal) != "exclusions"] <- exp_temporal[names(exp_temporal) != "exclusions"] %>% map(
  ~ mutate_at(
    .x, 
    vars(any_of(categorical_variables)), 
    ~ format_conditions(.) %>% named_contr_sum(., return_contr = FALSE)
  )
)

# Save wrangled data to .csv files in the tidy_data folder
save_tidy_data(exp_temporal)

# Combine skewness manipulations -----------------------------------------------

skew_temp <- list()
skewness_experiments <- list()

### Combine choices data ----

skew_temp$choices$centre <- exp_skew_centre$choices_summary %>% 
  filter(trial_type == "Shared") %>% 
  mutate(participant = participant %>% as.character() %>% str_c("c")) %>% 
  select(-option_value)

skew_temp$choices$edge <- exp_skew_edges$choices_summary %>% 
  filter(trial_type == "Extreme") %>% 
  mutate(part = experiment %>%  str_sub(-1)) %>% 
  select(-option_value, -experiment)

skew_temp$choices$tokens <- exp_skew_tokens$choices_summary %>% 
  filter(trial_type == "Decision") %>% 
  mutate(participant = participant %>% as.character() %>% str_c("d")) %>% 
  select(-option_value)

skew_temp$choices$types <- exp_skew_types$choices_summary %>% 
  filter(trial_type == "Shared") %>% 
  mutate(participant = participant %>% as.character() %>% str_c("e"))

skewness_experiments$choices <- bind_rows(
  skew_temp$choices$centre, 
  skew_temp$choices$edge, 
  skew_temp$choices$tokens, 
  skew_temp$choices$types, 
  .id = "experiment"
) %>% 
  mutate(
    experiment = case_when(
      is.na(part) ~ experiment, 
      TRUE ~ experiment %>% str_c(part))
  ) %>% 
  select(-part)

### Combine first to mind data ----

skew_temp$ftm$centre <- exp_skew_centre$ftm_recode %>% 
  filter(stimulus == "Shared") %>% 
  mutate(participant = participant %>% as.character() %>% str_c("c")) %>% 
  select(-stimulus_value)

skew_temp$ftm$edge <- exp_skew_edges$ftm_recode %>% 
  filter(stimulus == "Shared") %>% 
  mutate(part = experiment %>%  str_sub(-1)) %>% 
  select(-stimulus_value, -experiment)

skew_temp$ftm$tokens <- exp_skew_tokens$ftm_recode %>% 
  filter(stimulus == "Decision") %>% 
  mutate(participant = participant %>% as.character() %>% str_c("d")) %>% 
  select(-stimulus_value)

skew_temp$ftm$types <- exp_skew_types$ftm_recode %>% 
  filter(stimulus == "Shared") %>% 
  mutate(participant = participant %>% as.character() %>% str_c("e"))

skewness_experiments$ftm <- bind_rows(
  skew_temp$ftm$centre, 
  skew_temp$ftm$edge, 
  skew_temp$ftm$tokens, 
  skew_temp$ftm$types, 
  .id = "experiment"
) %>% 
  mutate(
    experiment = case_when(
      is.na(part) ~ experiment, 
      TRUE ~ experiment %>% str_c(part))
  ) %>% 
  select(-part)

### Combine percentage estimate data ----

skew_temp$percent$centre <- exp_skew_centre$percent_recode %>% 
  filter(stimulus == "Shared") %>% 
  mutate(participant = participant %>% as.character() %>% str_c("c")) %>% 
  select(-stimulus_value)

skew_temp$percent$edge <- exp_skew_edges$percent_recode %>% 
  filter(stimulus == "Shared") %>% 
  mutate(part = experiment %>%  str_sub(-1)) %>% 
  select(-stimulus_value, -experiment)

skew_temp$percent$tokens <- exp_skew_tokens$percent_recode %>% 
  filter(stimulus == "Decision") %>% 
  mutate(participant = participant %>% as.character() %>% str_c("d")) %>% 
  select(-stimulus_value)

skew_temp$percent$types <- exp_skew_types$percent_recode %>% 
  filter(stimulus == "Shared") %>% 
  mutate(participant = participant %>% as.character() %>% str_c("e"))

skewness_experiments$percent <- bind_rows(
  skew_temp$percent$centre, 
  skew_temp$percent$edge, 
  skew_temp$percent$tokens, 
  skew_temp$percent$types, 
  .id = "experiment"
) %>% 
  mutate(
    experiment = case_when(
      is.na(part) ~ experiment, 
      TRUE ~ experiment %>% str_c(part))
  ) %>% 
  select(-part)

### Format categorical variables ----

# Format and append contrasts for each categorical variable in each tibble
skewness_experiments[names(skewness_experiments) != "exclusions"] <- skewness_experiments[names(skewness_experiments) != "exclusions"] %>% map(
  ~ mutate_at(
    .x, 
    vars(any_of(categorical_variables[categorical_variables != "stimulus"])), 
    ~ format_conditions(.) %>% named_contr_sum(., return_contr = FALSE)
  )
)

# Save wrangled data to .csv files in the tidy_data folder
save_tidy_data(skewness_experiments)

# Summarise demographics -------------------------------------------------------

experiment_names <-  c(
  "exp_skew_centre", 
  "exp_variance",
  "exp_skew_edges",
  "exp_skew_edges_a",
  "exp_skew_edges_b", 
  "exp_skew_tokens", 
  "exp_skew_types", 
  "exp_temporal"
)

demographic_summaries <- list()

# Get the demographics summaries for each experiment (in a list)
demographic_summaries$experiments <- mget(experiment_names) %>% 
  map(~ .$demographics_summary) %>% 
  # Bind together the demographics summaries for each experiment (in a tibble)
  bind_rows(.id = "experiment")

# Get demographics data for each experiment (in a list)
combined_demographics_data <- mget(experiment_names) %>% 
  map(~ .$demographics) %>% 
  # Bind together the demographics data (in a tibble)
  bind_rows(.id = "experiment")

# Create overall summary for all experiments combined
demographic_summaries$overall <-  combined_demographics_data %>% 
  summarise_demographics() %>% 
  select(-n_per_condition)

# Create summary for university experiments
demographic_summaries$university <- combined_demographics_data %>% 
  filter(experiment %in% experiment_names[c(1, 2, 3, 8)]) %>% 
  summarise_demographics() %>% 
  select(-n_per_condition)

# Create summary for MTurk participants
demographic_summaries$mturk <- combined_demographics_data %>% 
  filter(experiment %in% experiment_names[c(6, 7)]) %>% 
  summarise_demographics() %>% 
  select(-n_per_condition)

save_tidy_data(demographic_summaries)
