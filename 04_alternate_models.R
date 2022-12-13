## Experiments
# 1. `exp_skew_centre`: Manipulated rank (and average) by including an additional pair above or below shared pair.
# 2. `exp_variance`: Manipulated distance from the average using variance.
# 3. `exp_skew_edges`: Manipulated distance from the average by including outcomes close to the best or worst outcome.
# 4. `exp_skew_tokens`: Manipulated rank (and average) while controlling for types.
# 5. `exp_skew_types`: Manipulated rank while controlling for average and tokens.
# 6. `exp_temporal`: Manipulated alternating or random presentation of gains and losses.

# Setup ------------------------------------------------------------------------

set.seed(1234)

library(tidyverse)
library(parallel)
library(rlang)
library(brms)
library(bayestestR)
library(tidybayes)
library(bayesplot)
library(here)

here::here("src", "utilities", "save_tidy_data.R") %>% source()
here::here("src", "utilities", "load_tidy_data.R") %>% source()
here::here("src", "model", "save_model.R") %>% source()
here::here("src", "model", "pp_check_percent.R") %>% source()
here::here("src", "model", "generate_simo_priors.R") %>% source()
here::here("src", "model", "run_mcmc_diagnostics.R") %>% source()
here::here("src", "model", "standardise.R") %>% source()
here::here("src", "model", "conditional_hypothesis.R") %>% source()

temp <- list()

# Load data ----

if (!exists("data_wrangled_in_current_session")) {
  
  data_names <-  c(
    "exp_skew_centre", 
    "exp_variance",
    "exp_skew_edges",
    "exp_skew_edges",
    "exp_skew_tokens", 
    "exp_skew_types", 
    "exp_temporal"
  )
  
  load_tidy_data(data_names)
  
}

# Options ----

# Setting quick_version to TRUE allows faster but less precise parameter estimation by reducing the number of samples taken in the brms models. Set to FALSE to reproduce published values.
# Setting overwrite_saved_models to TRUE ensures that the Stan models are run every time instead of loading a cached version. In order to save time, this should be set to FALSE unless changes have been made to the model. 
# Set run_diagnostics to TRUE to create additional diagnostic plots for the brms models (e.g., rank histograms, posterior predictive checks). Even if FALSE, the default warnings (e.g., divergences, rhat) will still be displayed if applicable.
# A few Boost and Rcpp warning messages might be generated when the C++ code for the Stan model is compiling, but these are nothing to worry about (see [here](https://discourse.mc-stan.org/t/boost-and-rcppeigen-warnings-for-r-package-using-stan/3478)).
# The linear regression residuals for the percentage estimate responses were not normally distributed. Instead, many people favoured reporting round numbers (e.g., 25, 50, 75). To check that the results of the linear regression models were not influenced by this violated assumption we also modelled the results as ordinal categories rounded to the nearest 5 percent (-100, -95, -90... 95, 100). These models take considerably longer to run than most of the other models and the results are not included in the paper because the results of both methods were very similar and the linear regression results are easier to explain. Only set run_ordinal to TRUE if you are interested in comparing the results of the linear and ordinal models.
# The number of cores used for the Stan models will be the number of available cores minus the value of the reserved_cores variable (or one if the number of reserved cores is greater than or equal to the number of available cores).

options(quick_version = TRUE)
options(overwrite_saved_models = TRUE)
run_diagnostics = FALSE
run_ordinal = FALSE
reserved_cores = 2

# Convert reserved cores into used cores
cores <- max(c(detectCores() - reserved_cores, 1))

# Run as many chains as there are available cores
chains <- cores

# Set default contrast type to -1, 1 for factors
options(contrasts = c("contr.sum", "contr.poly"))

## Set priors ----

priors <- list()

# Priors for intercept and slopes
priors$basic <- c(
  prior(student_t(7, 0, 0.5), class = Intercept),
  prior(student_t(7, 0, 0.5), class = b)
)

# Prior for standard deviation of varying intercepts
priors$varying_intercepts <- c(prior(student_t(7, 0, 0.5), class = sd))

# Prior for correlation between varying intercepts and slopes
priors$varying_slopes <- prior(lkj(4), class = cor)

# Prior for standard deviation in linear models
priors$sigma <- prior(student_t(7, 0, 1), class = sigma)

# Prior for simplex parameters of monotonic models (simo)
# simo class priors generally need to be set individually for each coef
# generate_simo_priors() generates priors for all simo parameters of the model using priors$simo
priors$simo <- prior(dirichlet(3), class = simo)

# Priors for distributional models of the standard deviation
priors$basic_sigma <- mutate(priors$basic, dpar = "sigma")

# Prior for monotonic slope (SD = 0.25 is roughly the same scale as other parameters)
priors$mooption_value <- prior(student_t(7, 0, 0.25), class = b, coef = mooption_value)
priors$mostimulus_value <- prior(student_t(7, 0, 0.25), class = b, coef = mostimulus_value)
priors$mostimulus_value_sigma <- prior(student_t(7, 0, 0.25), class = b, coef = mostimulus_value, dpar = "sigma")

# Priors for first to mind parameters
priors$basic_ftm <- map_df(
  c("muBetter", "muOther"), 
  ~ mutate(priors$basic, dpar = .)
)

priors$varying_intercepts_ftm <- map_df(
  c("muBetter", "muOther"), 
  ~ mutate(priors$varying_intercepts, dpar = .)
)

priors$mostimulus_value_ftm <- map_df(
  c("muBetter", "muOther"), 
  ~ mutate(priors$mostimulus_value, dpar = .)
)

# Fit models to the experimental data ------------------------------------------

## 1. Skewed distributions (shared centre) -------------------------------------

### Option value ----

#### Choices ----

exp_skew_centre$models$choices_value <- brm(
  risky_count | trials(n_choices) ~ 1 + option_value * condition + (1 + option_value | participant),
  data = exp_skew_centre$choices_summary %>% filter(!participant %in% exp_skew_centre$exclusions),
  family = binomial,
  prior = c(priors$basic, priors$varying_intercepts, priors$varying_slopes),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 4000, floor(150000 / chains) + 2000),
  file = save_model("choices_value", "exp_skew_centre"),
  thin = ifelse(getOption("quick_version"), 1, 5),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_skew_centre$diagnostics$choices_value <- run_mcmc_diagnostics(
    exp_skew_centre$models$choices_value, 
    type = "choices", 
    between_condition = condition, 
    within_condition = option_value
  )
}

## Test hypotheses based on model
exp_skew_centre$hypotheses$choices_value <- conditional_hypothesis(
  exp_skew_centre$models$choices_value, 
  hypothesis = "High value > Low value", 
  effect_variables = "option_value", 
  conditional_variables = "none"
)

#### First to mind ----

exp_skew_centre$models$ftm_value <- brm(
  response_recode_value  ~ 1 + stimulus_value * condition + (1 + stimulus_value || participant),
  data = exp_skew_centre$ftm_recode %>% filter(!participant %in% exp_skew_centre$exclusions),
  family = categorical(refcat = "Worse"),
  prior = c(priors$basic_ftm, priors$varying_intercepts_ftm),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 2000, floor(20000 / chains) + 2000),
  file = save_model("ftm_value", "exp_skew_centre"),
  control = list(adapt_delta = 0.9),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_skew_centre$diagnostics$ftm_value <- run_mcmc_diagnostics(
    exp_skew_centre$models$ftm_value, 
    type = "ftm", 
    between_condition = condition, 
    within_condition = stimulus_value
  )
}

## Test hypotheses based on model
exp_skew_centre$hypotheses$ftm_value <- conditional_hypothesis(
  exp_skew_centre$models$ftm_value, 
  hypothesis = "High value > Low value", 
  effect_variables = "stimulus_value", 
  conditional_variables = "none",
  outcome_level = "Better"
)

#### Percentage estimates ----

exp_skew_centre$models$percent_value <- brm(
  bf(
    # Varying slope removed from maximal model for convergence issues (divergences and low ESS for SD)
    # Nonetheless, these models produced very similar estimates for the parameters of interest
    better_minus_worse ~ 1 + stimulus_value * condition + (1 | participant),
    sigma ~ 1 + stimulus_value * condition
  ),
  data = exp_skew_centre$percent_recode %>% 
    filter(!participant %in% exp_skew_centre$exclusions) %>% 
    mutate(better_minus_worse = standardise(better_minus_worse)),
  prior = c(
    priors$basic, 
    priors$varying_intercepts,
    priors$basic_sigma
  ),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 2000, floor(20000 / chains) + 2000),
  file = save_model("percent_value", "exp_skew_centre"),
  control = list(adapt_delta = 0.99),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_skew_centre$diagnostics$percent_value <- run_mcmc_diagnostics(
    exp_skew_centre$models$percent_value, 
    type = "percent", 
    between_condition = condition, 
    within_condition = stimulus_value, 
    responses = better_minus_worse
  )
}

## Test hypotheses based on model
exp_skew_centre$hypotheses$percent_value <- conditional_hypothesis(
  exp_skew_centre$models$percent_value, 
  hypothesis = "High value > Low value", 
  effect_variables = "stimulus_value", 
  conditional_variables = "none"
)

# Ordinal regression (cumulative logit) on percentage estimates rounded to nearest 5 points
if (run_ordinal) {
  exp_skew_centre$models$percent_value_ordinal <- brm(
    ordinal ~ 1 + stimulus_value * condition + (1 + stimulus_value | participant), 
    data = exp_skew_centre$percent_recode %>% 
      mutate(
        # Round to nearest 5 points
        ordinal = better_minus_worse %>% 
          cut_width(width = 5, center = 0, labels = FALSE) %>% as.numeric(),
        # Ordered factor with name based on central value
        ordinal = ((ordinal - 1) * 5 - 100)
      ) %>% 
      filter(
        !participant %in% exp_skew_centre$exclusions
      ) %>% 
      mutate(ordinal = factor(ordinal, levels = seq(from = min(ordinal), to = max(ordinal), by = 5), ordered = TRUE)),  
    family = cumulative,
    c(priors$basic, priors$varying_intercepts, priors$varying_slopes), 
    file = save_model("percent_value_ordinal", "exp_skew_centre"),
    cores = cores, 
    chains = chains,
    backend = "cmdstanr"
  )
}

### Skewness ----

#### Choices ----

exp_skew_centre$models$choices_shared <- brm(
  risky_count | trials(n_choices) ~ 1 + condition + (1 | participant),
  data = exp_skew_centre$choices_summary %>% 
    filter(trial_type == "Shared", !participant %in% exp_skew_centre$exclusions),
  family = binomial,
  prior = c(priors$basic, priors$varying_intercepts),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 4000, floor(150000 / chains) + 2000),
  file = save_model("choices_shared", "exp_skew_centre"),
  thin = ifelse(getOption("quick_version"), 1, 5),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_skew_centre$diagnostics$choices_shared <- run_mcmc_diagnostics(
    exp_skew_centre$models$choices_shared, 
    type = "choices", 
    between_condition = condition
  )
}

## Test hypotheses based on model
exp_skew_centre$hypotheses$choices_shared <- conditional_hypothesis(
  exp_skew_centre$models$choices_shared, 
  hypothesis = "Right-skewed > Left-skewed", 
  effect_variables = "condition", 
  conditional_variables = "none"
)

#### First to mind ----

exp_skew_centre$models$ftm_shared <- brm(
  response_recode_value ~ 1 + condition,
  data = exp_skew_centre$ftm_recode %>% filter(
    stimulus == "Shared", !participant %in% exp_skew_centre$exclusions),
  family = categorical(refcat = "Worse"),
  prior = priors$basic_ftm,
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 2000, floor(15000 / chains) + 2000),
  file = save_model("ftm_shared", "exp_skew_centre"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_skew_centre$diagnostics$ftm_shared <- run_mcmc_diagnostics(
    exp_skew_centre$models$ftm_shared, 
    type = "ftm", 
    between_condition = condition
  )
}

## Test hypotheses based on model
exp_skew_centre$hypotheses$ftm_shared <- conditional_hypothesis(
  exp_skew_centre$models$ftm_shared, 
  hypothesis = "Right-skewed > Left-skewed", 
  effect_variables = "condition", 
  conditional_variables = "none",
  outcome_level = "Better"
)

#### Percentage estimates ----

exp_skew_centre$models$percent_shared <- brm(
  bf(better_minus_worse ~ 1 + condition, sigma ~ 1 + condition),
  data = exp_skew_centre$percent_recode %>% 
    filter(stimulus == "Shared", !participant %in% exp_skew_centre$exclusions) %>% 
    mutate(better_minus_worse = standardise(better_minus_worse)),
  prior = c(priors$basic, priors$basic_sigma),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 2000, floor(15000 / chains) + 2000),
  file = save_model("percent_shared", "exp_skew_centre"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_skew_centre$diagnostics$percent_shared <- run_mcmc_diagnostics(
    exp_skew_centre$models$percent_shared, 
    type = "percent", 
    between_condition = condition, 
    responses = better_minus_worse
  )
}

## Test hypotheses based on model
exp_skew_centre$hypotheses$percent_shared <- conditional_hypothesis(
  exp_skew_centre$models$percent_shared, 
  hypothesis = "Right-skewed > Left-skewed", 
  effect_variables = "condition", 
  conditional_variables = "none"
)

# Ordinal regression (cumulative logit) on percentage estimates rounded to nearest 5 points
if (run_ordinal) {
  exp_skew_centre$models$percent_shared_ordinal <- brm(
    ordinal ~ 1 + condition, 
    data = exp_skew_centre$percent_recode %>% 
      mutate(
        # Round to nearest 5 points
        ordinal = better_minus_worse %>% 
          cut_width(width = 5, center = 0, labels = FALSE) %>% as.numeric(),
        # Ordered factor with name based on central value
        ordinal = ((ordinal - 1) * 5 - 100)
      ) %>% 
      filter(
        !participant %in% exp_skew_centre$exclusions,
        stimulus == "Shared"
      ) %>% 
      mutate(ordinal = factor(ordinal, levels = seq(from = min(ordinal), to = max(ordinal), by = 5), ordered = TRUE)), 
    family = cumulative,
    prior = priors$basic, 
    control = list(adapt_delta = 0.99),
    file = save_model("percent_shared_ordinal", "exp_skew_centre"),
    cores = cores, 
    chains = chains,
    backend = "cmdstanr"
  )
}

### Save hypotheses ----

save_tidy_data(
  exp_skew_centre$hypotheses, 
  parent_folder = here("output", "hypotheses"), 
  data_name = "exp_skew_centre",
  save_csv = FALSE
)

## 2. Variance -----------------------------------------------------------------

### Option value and variance ----

#### Choices ----

exp_variance$models$choices <- brm(
  risky_count | trials(n_choices) ~ 1 + trial_type * condition + (1 + trial_type | participant),
  data = exp_variance$choices_summary %>% filter(!participant %in% exp_variance$exclusions),
  family = binomial,
  prior = c(priors$basic, priors$varying_intercepts, priors$varying_slopes),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 3000, floor(100000 / chains) + 2000),
  file = save_model("choices", "exp_variance"),
  thin = ifelse(getOption("quick_version"), 1, 3),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_variance$diagnostics$choices <- run_mcmc_diagnostics(
    exp_variance$models$choices, 
    type = "choices", 
    between_condition = condition, 
    within_condition = trial_type
  )
}

## Test hypotheses based on model
exp_variance$hypotheses$choices_value <- conditional_hypothesis(
  exp_variance$models$choices, 
  hypothesis = "High value > Low value", 
  effect_variables = "trial_type", 
  conditional_variables = "none"
)

exp_variance$hypotheses$choices_variance <- conditional_hypothesis(
  exp_variance$models$choices, 
  hypothesis = "High value:High variance > High value:Low variance", 
  effect_variables = c("trial_type", "condition"), 
  conditional_variables = "none"
)

#### First to mind ----

exp_variance$models$ftm <- brm(
  response_recode_value ~ 1 + stimulus * condition + (1 + stimulus || participant),
  data = exp_variance$ftm_recode %>% 
    filter(!participant %in% exp_variance$exclusions),
  family = categorical(refcat = "Worse"),
  prior = c(priors$basic_ftm, priors$varying_intercepts_ftm),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 2000, floor(20000 / chains) + 2000),
  file = save_model("ftm", "exp_variance"),
  control = list(adapt_delta = 0.9),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_variance$diagnostics$ftm <- run_mcmc_diagnostics(
    exp_variance$models$ftm, 
    type = "ftm", 
    between_condition = condition, 
    within_condition = stimulus
  )
}

## Test hypotheses based on model
exp_variance$hypotheses$ftm_value <- conditional_hypothesis(
  exp_variance$models$ftm, 
  hypothesis = "High value > Low value", 
  effect_variables = "stimulus", 
  conditional_variables = "none",
  outcome_level = "Better"
)

exp_variance$hypotheses$ftm_variance <- conditional_hypothesis(
  exp_variance$models$ftm, 
  hypothesis = "High value:High variance > High value:Low variance", 
  effect_variables = c("stimulus", "condition"), 
  conditional_variables = "none",
  outcome_level = "Better"
)

#### Percentage estimates ----

exp_variance$models$percent <- brm(
  bf(
    # Varying slope removed from maximal model for convergence issues (divergences and low ESS for SD)
    # Nonetheless, these models produced very similar estimates for the parameters of interest
    better_minus_worse ~ 1 + stimulus * condition + (1 | participant),
    sigma ~ 1 + stimulus * condition
  ),
  data = exp_variance$percent_recode %>% 
    filter(!participant %in% exp_variance$exclusions) %>% 
    mutate(better_minus_worse = standardise(better_minus_worse)),
  prior = c(
    priors$basic, 
    priors$varying_intercepts,
    priors$basic_sigma
  ),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 2000, floor(20000 / chains) + 2000),
  file = save_model("percent", "exp_variance"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_variance$diagnostics$percent <- run_mcmc_diagnostics(
    exp_variance$models$percent, 
    type = "percent", 
    between_condition = condition, 
    within_condition = stimulus, 
    responses = better_minus_worse
  )
}

## Test hypotheses based on model
exp_variance$hypotheses$percent_value <- conditional_hypothesis(
  exp_variance$models$percent, 
  hypothesis = "High value > Low value", 
  effect_variables = "stimulus", 
  conditional_variables = "none"
)

exp_variance$hypotheses$percent_variance <- conditional_hypothesis(
  exp_variance$models$percent, 
  hypothesis = "High value:High variance > High value:Low variance", 
  effect_variables = c("stimulus", "condition"), 
  conditional_variables = "none"
)

# Ordinal regression (cumulative logit) on percentage estimates rounded to nearest 5 points
if (run_ordinal) {
  exp_variance$models$percent_ordinal <- brm(
    ordinal ~ 1 + stimulus * condition + (1 | participant), 
    data = exp_variance$percent_recode %>% 
      mutate(
        # Round to nearest 5 points
        ordinal = better_minus_worse %>% 
          cut_width(width = 5, center = 0, labels = FALSE) %>% as.numeric(),
        # Ordered factor with name based on central value
        ordinal = ((ordinal - 1) * 5 - 100)
      ) %>% 
      filter(
        !participant %in% exp_variance$exclusions
      ) %>% 
      mutate(ordinal = factor(ordinal, levels = seq(from = min(ordinal), to = max(ordinal), by = 5), ordered = TRUE)), 
    family = cumulative,
    prior = c(priors$basic, priors$varying_intercepts), 
    control = list(adapt_delta = 0.99),
    file = save_model("percent_ordinal", "exp_variance"),
    cores = cores, 
    chains = chains,
    backend = "cmdstanr"
  )
}

### Save hypotheses ----

save_tidy_data(
  exp_variance$hypotheses, 
  parent_folder = here("output", "hypotheses"), 
  data_name = "exp_variance",
  save_csv = FALSE
)

## 3. Skewed distributions (shared edges) ----

### Option value ----

#### Choices ----

exp_skew_edges$models$choices_value <- brm(
  risky_count | trials(n_choices) ~ 1 + experiment * option_value + (1 | participant), 
  data = exp_skew_edges$choices_summary %>% 
    filter(trial_type == "Context", !participant %in% exp_skew_edges$exclusions) %>% 
    mutate(option_value = named_contr_sum(option_value, return_contr = FALSE)),
  family = binomial,
  prior = priors$basic,
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 4000, floor(150000 / chains) + 2000),
  file = save_model("choices_value", "exp_skew_edges"),
  thin = ifelse(getOption("quick_version"), 1, 5),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_skew_edges$diagnostics$choices_value <- run_mcmc_diagnostics(
    exp_skew_edges$models$choices_value, 
    type = "choices", 
    between_condition = experiment, 
    within_condition = option_value
  )
}

## Test hypotheses based on model
exp_skew_edges$hypotheses$choices_value <- conditional_hypothesis(
  exp_skew_edges$models$choices_value, 
  hypothesis = "High value > Low value", 
  effect_variables = "option_value", 
  conditional_variables = "none"
)

#### First to mind ----

exp_skew_edges$models$ftm_value <- brm(
  response_recode_value ~ 1 + experiment * stimulus_value,
  data = exp_skew_edges$ftm_recode %>% 
    filter(!participant %in% exp_skew_edges$exclusions, stimulus == "Context") %>% 
    mutate(stimulus_value = named_contr_sum(stimulus_value, return_contr = FALSE)),
  family = categorical(refcat = "Worse"),
  prior = priors$basic_ftm,
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 2000, floor(20000 / chains) + 2000),
  file = save_model("ftm_value", "exp_skew_edges"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_skew_edges$diagnostics$ftm_value <- run_mcmc_diagnostics(
    exp_skew_edges$models$ftm_value, 
    type = "ftm", 
    between_condition = experiment, 
    within_condition = stimulus_value
  )
}

## Test hypotheses based on model
exp_skew_edges$hypotheses$ftm_value <- conditional_hypothesis(
  exp_skew_edges$models$ftm_value, 
  hypothesis = "High value > Low value", 
  effect_variables = "stimulus_value", 
  conditional_variables = "none",
  outcome_level = "Better"
)

#### Percentage estimates ----

exp_skew_edges$models$percent_value <- brm(
  bf(
    better_minus_worse ~ 1 + experiment * stimulus_value, 
    sigma ~ 1 + experiment * stimulus_value
  ),
  data = exp_skew_edges$percent_recode %>% 
    filter(!participant %in% exp_skew_edges$exclusions, stimulus == "Context") %>% 
    mutate(
      better_minus_worse = standardise(better_minus_worse),
      stimulus_value = named_contr_sum(stimulus_value, return_contr = FALSE)
    ),
  prior = c(priors$basic, priors$basic_sigma),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 2000, floor(20000 / chains) + 2000),
  file = save_model("percent_value", "exp_skew_edges"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_skew_edges$diagnostics$percent_value <- run_mcmc_diagnostics(
    exp_skew_edges$models$percent_value, 
    type = "percent", 
    between_condition = experiment, 
    within_condition = option_value, 
    responses = better_minus_worse
  )
}

## Test hypotheses based on model
exp_skew_edges$hypotheses$percent_value <- conditional_hypothesis(
  exp_skew_edges$models$percent_value, 
  hypothesis = "High value > Low value", 
  effect_variables = "stimulus_value", 
  conditional_variables = "none"
)

# Ordinal regression (cumulative logit) on percentage estimates rounded to nearest 5 points
if (run_ordinal) {
  exp_skew_edges$models$percent_value_ordinal <- brm(
    ordinal ~ 1 + experiment * option_value, 
    data = exp_skew_edges$percent_recode %>% 
      mutate(
        # Round to nearest 5 points
        ordinal = better_minus_worse %>% 
          cut_width(width = 5, center = 0, labels = FALSE) %>% as.numeric(),
        # Ordered factor with name based on central value
        ordinal = ((ordinal - 1) * 5 - 100)
      ) %>% 
      filter(
        !participant %in% exp_skew_edges$exclusions,
        stimulus == "Context"
      ) %>% 
      mutate(ordinal = factor(ordinal, levels = seq(from = min(ordinal), to = max(ordinal), by = 5), ordered = TRUE)), 
    family = cumulative,
    prior = priors$basic, 
    control = list(adapt_delta = 0.99),
    file = save_model("percent_value_ordinal", "exp_skew_edges"),
    cores = cores, 
    chains = chains,
    backend = "cmdstanr"
  )
}

### Skewness ----

#### Choices ----

exp_skew_edges$models$choices_shared <- brm(
  risky_count | trials(n_choices) ~ 1 + experiment * condition + (1 | participant), 
  data = exp_skew_edges$choices_summary %>% 
    filter(trial_type == "Extreme", !participant %in% exp_skew_edges$exclusions),
  family = binomial,
  prior = c(priors$basic, priors$varying_intercepts),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 4000, floor(150000 / chains) + 2000),
  file = save_model("choices_shared", "exp_skew_edges"),
  thin = ifelse(getOption("quick_version"), 1, 5),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_skew_edges$diagnostics$choices_shared <- run_mcmc_diagnostics(
    exp_skew_edges$models$choices_shared, 
    type = "choices", 
    between_condition = experiment, 
    within_condition = condition
  )
}

## Test hypotheses based on model
exp_skew_edges$hypotheses$choices_shared <- conditional_hypothesis(
  exp_skew_edges$models$choices_shared, 
  hypothesis = "Right-skewed > Left-skewed", 
  effect_variables = "condition", 
  conditional_variables = "none"
)

#### First to mind ----

exp_skew_edges$models$ftm_shared <- brm(
  response_recode_value ~ 1 + experiment * condition,
  data = exp_skew_edges$ftm_recode %>% 
    filter(!participant %in% exp_skew_edges$exclusions, stimulus == "Shared"),
  family = categorical(refcat = "Worse"),
  prior = priors$basic_ftm,
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 2000, floor(20000 / chains) + 2000),
  file = save_model("ftm_shared", "exp_skew_edges"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_skew_edges$diagnostics$ftm_shared <- run_mcmc_diagnostics(
    exp_skew_edges$models$ftm_shared, 
    type = "ftm", 
    between_condition = experiment, 
    within_condition = condition
  )
}

## Test hypotheses based on model
exp_skew_edges$hypotheses$ftm_shared <- conditional_hypothesis(
  exp_skew_edges$models$ftm_shared, 
  hypothesis = "Right-skewed > Left-skewed", 
  effect_variables = "condition", 
  conditional_variables = "none",
  outcome_level = "Better"
)

#### Percentage estimates ----

exp_skew_edges$models$percent_shared <- brm(
  bf(
    better_minus_worse  ~ 1 + experiment * condition,
    sigma  ~ 1 + experiment * condition
  ),
  data = exp_skew_edges$percent_recode %>% 
    filter(!participant %in% exp_skew_edges$exclusions, stimulus == "Shared") %>% 
    mutate(better_minus_worse = standardise(better_minus_worse)),
  prior = c(priors$basic, priors$basic_sigma),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 2000, floor(20000 / chains) + 2000),
  file = save_model("percent_shared", "exp_skew_edges"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_skew_edges$diagnostics$percent_shared <- run_mcmc_diagnostics(
    exp_skew_edges$models$percent_shared, 
    type = "percent", 
    between_condition = experiment, 
    within_condition = condition
  )
}

## Test hypotheses based on model
exp_skew_edges$hypotheses$percent_shared <- conditional_hypothesis(
  exp_skew_edges$models$percent_shared, 
  hypothesis = "Right-skewed > Left-skewed", 
  effect_variables = "condition", 
  conditional_variables = "none"
)

# Ordinal regression (cumulative logit) on percentage estimates rounded to nearest 5 points
if (run_ordinal) {
  exp_skew_edges$models$percent_shared_ordinal <- brm(
    ordinal ~ 1 + experiment * condition, 
    data = exp_skew_edges$percent_recode %>% 
      mutate(
        # Round to nearest 5 points
        ordinal = better_minus_worse %>% 
          cut_width(width = 5, center = 0, labels = FALSE) %>% as.numeric(),
        # Ordered factor with name based on central value
        ordinal = ((ordinal - 1) * 5 - 100)
      ) %>% 
      filter(
        !participant %in% exp_skew_edges$exclusions,
        stimulus == "Shared"
      ) %>% 
      mutate(ordinal = factor(ordinal, levels = seq(from = min(ordinal), to = max(ordinal), by = 5), ordered = TRUE)), 
    family = cumulative,
    control = list(adapt_delta = 0.99),
    file = save_model("percent_shared_ordinal", "exp_skew_edges"),
    cores = cores, 
    chains = chains,
    backend = "cmdstanr"
  )
}

### Save hypotheses ----

save_tidy_data(
  exp_skew_edges$hypotheses, 
  parent_folder = here("output", "hypotheses"), 
  data_name = "exp_skew_edges",
  save_csv = FALSE
)

## 4. Skewed distributions (tokens) --------------------------------------------

### Option value ----

#### Choices ----

temp$exp_skew_tokens$choices_value$formula <- bf(
  risky_count | trials(n_choices) ~ 1 + mo(option_value) * condition + 
    (1 + mo(option_value) | participant), 
  family = binomial
)

temp$exp_skew_tokens$choices_value$data <- exp_skew_tokens$choices_summary %>% 
  filter(trial_type != "Catch", !participant %in% exp_skew_tokens$exclusions)

exp_skew_tokens$models$choices_value <- brm(
  temp$exp_skew_tokens$choices_value$formula,
  data = temp$exp_skew_tokens$choices_value$data,
  prior = c(
    priors$basic, 
    priors$varying_intercepts, 
    priors$varying_slopes,
    priors$mooption_value,
    generate_simo_priors(priors$simo, temp$exp_skew_tokens$choices_value)
  ),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 4000, floor(150000 / chains) + 2000),
  file = save_model("choices_value", "exp_skew_tokens"),
  thin = ifelse(getOption("quick_version"), 1, 5),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_skew_tokens$diagnostics$choices_value <- run_mcmc_diagnostics(
    exp_skew_tokens$models$choices_value, 
    type = "choices", 
    between_condition = condition, 
    within_condition = option_value
  )
}

## Test hypotheses based on model
exp_skew_tokens$hypotheses$choices_value <- conditional_hypothesis(
  exp_skew_tokens$models$choices_value, 
  hypothesis = "option_value > 0", 
  effect_variables = "option_value", 
  conditional_variables = "none"
)

#### First to mind ----

temp$exp_skew_tokens$ftm_value$formula <- bf(
  response_recode_value  ~ 1 + mo(stimulus_value) * condition + 
    (1 + mo(stimulus_value) || participant),
  family = categorical(refcat = "Worse")
)

temp$exp_skew_tokens$ftm_value$data <- exp_skew_tokens$ftm_recode %>% 
  filter(!participant %in% exp_skew_tokens$exclusions)

exp_skew_tokens$models$ftm_value <- brm(
  temp$exp_skew_tokens$ftm_value$formula,
  data = temp$exp_skew_tokens$ftm_value$data,
  prior = c(
    priors$basic_ftm, 
    priors$varying_intercepts_ftm,
    priors$mostimulus_value_ftm,
    generate_simo_priors(priors$simo, temp$exp_skew_tokens$ftm_value)
  ),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 2000, floor(20000 / chains) + 2000),
  file = save_model("ftm_value", "exp_skew_tokens"),
  control = list(adapt_delta = 0.9),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_skew_tokens$diagnostics$ftm_value <- run_mcmc_diagnostics(
    exp_skew_tokens$models$ftm_value, 
    type = "ftm", 
    between_condition = condition, 
    within_condition = stimulus_value
  )
}

## Test hypotheses based on model
exp_skew_tokens$hypotheses$ftm_value <- conditional_hypothesis(
  exp_skew_tokens$models$ftm_value, 
  hypothesis = "stimulus_value > 0", 
  effect_variables = "stimulus_value", 
  conditional_variables = "none",
  outcome_level = "Better"
)

#### Percentage estimates ----

temp$exp_skew_tokens$percent_value$formula <- bf(
  # Varying slope removed from maximal model for convergence issues (divergences and low ESS for SD)
  # Nonetheless, these models produced very similar estimates for the parameters of interest
  better_minus_worse ~ 1 + mo(stimulus_value) * condition + (1 | participant),  
  sigma ~ 1 + mo(stimulus_value) * condition
)

temp$exp_skew_tokens$percent_value$data <- exp_skew_tokens$percent_recode %>% 
  filter(!participant %in% exp_skew_tokens$exclusions) %>% 
  mutate(better_minus_worse = standardise(better_minus_worse))

exp_skew_tokens$models$percent_value <- brm(
  temp$exp_skew_tokens$percent_value$formula,
  data = temp$exp_skew_tokens$percent_value$data,
  prior = c(
    priors$basic, 
    priors$varying_intercepts,
    priors$basic_sigma,
    priors$mostimulus_value,
    priors$mostimulus_value_sigma,
    generate_simo_priors(priors$simo, temp$exp_skew_tokens$percent_value)
  ),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 2000, floor(15000 / chains) + 2000),
  file = save_model("percent_value", "exp_skew_tokens"),
  control = list(adapt_delta = 0.9),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_skew_tokens$diagnostics$percent_value <- run_mcmc_diagnostics(
    exp_skew_tokens$models$percent_value, 
    type = "percent", 
    between_condition = condition, 
    within_condition = stimulus_value
  )
}

## Test hypotheses based on model
exp_skew_tokens$hypotheses$percent_value <- conditional_hypothesis(
  exp_skew_tokens$models$percent_value, 
  hypothesis = "stimulus_value > 0", 
  effect_variables = "stimulus_value", 
  conditional_variables = "none"
)

# Ordinal regression (cumulative logit) on percentage estimates rounded to nearest 5 points
if (run_ordinal) {
  temp$exp_skew_tokens$percent_value_ordinal$formula <- bf(
    ordinal ~ 1 + mo(stimulus_value) * condition + (1 | participant), 
    family = cumulative
  )
  
  temp$exp_skew_tokens$percent_value_ordinal$data <- exp_skew_tokens$percent_recode %>% 
    mutate(
      # Round to nearest 5 points
      ordinal = better_minus_worse %>% 
        cut_width(width = 5, center = 0, labels = FALSE) %>% as.numeric(),
      # Ordered factor with name based on central value
      ordinal = ((ordinal - 1) * 5 - 100)
    ) %>% 
    filter(
      !participant %in% exp_skew_tokens$exclusions
    ) %>% 
    mutate(
      ordinal = factor(
        ordinal, 
        levels = seq(from = min(ordinal),  to = max(ordinal), by = 5),
        ordered = TRUE
      )
    )
  
  exp_skew_tokens$models$percent_value_ordinal <- brm(
    temp$exp_skew_tokens$percent_value_ordinal$formula, 
    data = temp$exp_skew_tokens$percent_value_ordinal$data, 
    prior = c(
      priors$basic, 
      priors$varying_intercepts, 
      priors$varying_slopes,
      priors$mostimulus_value,
      generate_simo_priors(priors$simo, temp$exp_skew_tokens$percent_value_ordinal)
    ), 
    file = save_model("percent_value_ordinal", "exp_skew_tokens"),
    cores = cores, 
    chains = chains,
    backend = "cmdstanr"
  )
}

### Skewness ----

#### Choices ----

exp_skew_tokens$models$choices_shared <- brm(
  risky_count | trials(n_choices) ~ 1 + condition + (1 | participant),
  data = exp_skew_tokens$choices_summary %>% filter(
    trial_type == "Shared", 
    !participant %in% exp_skew_tokens$exclusions
  ),
  family = binomial,
  prior = c(priors$basic, priors$varying_intercepts),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 4000, floor(200000 / chains) + 2000),
  file = save_model("choices_shared", "exp_skew_tokens"),
  thin = ifelse(getOption("quick_version"), 1, 6),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_skew_tokens$diagnostics$choices_shared <- run_mcmc_diagnostics(
    exp_skew_tokens$models$choices_shared, 
    type = "choices", 
    between_condition = condition
  )
}

## Test hypotheses based on model
exp_skew_tokens$hypotheses$choices_shared <- conditional_hypothesis(
  exp_skew_tokens$models$choices_shared, 
  hypothesis = "Right-skewed > Left-skewed", 
  effect_variables = "condition", 
  conditional_variables = "none"
)

#### First to mind ----

exp_skew_tokens$models$ftm_shared <- brm(
  response_recode_value ~ 1 + condition,
  data = exp_skew_tokens$ftm_recode %>% filter(
    stimulus == "Shared", 
    !participant %in% exp_skew_tokens$exclusions
  ),
  family = categorical(refcat = "Worse"),
  prior = priors$basic_ftm,
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 2000, floor(15000 / chains) + 2000),
  file = save_model("ftm_shared", "exp_skew_tokens"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_skew_tokens$diagnostics$ftm_shared <- run_mcmc_diagnostics(
    exp_skew_tokens$models$ftm_shared, 
    type = "ftm", 
    between_condition = condition
  )
}

## Test hypotheses based on model
exp_skew_tokens$hypotheses$ftm_shared <- conditional_hypothesis(
  exp_skew_tokens$models$ftm_shared, 
  hypothesis = "Right-skewed > Left-skewed", 
  effect_variables = "condition", 
  conditional_variables = "none",
  outcome_level = "Better"
)

#### Percentage estimates ----

exp_skew_tokens$models$percent_shared <- brm(
  bf(better_minus_worse ~ 1 + condition, sigma ~ 1 + condition),
  data = exp_skew_tokens$percent_recode %>% filter(
    stimulus == "Shared", 
    !participant %in% exp_skew_tokens$exclusions
  ),
  prior = c(priors$basic, priors$basic_sigma),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 2000, floor(15000 / chains) + 2000),
  file = save_model("percent_shared", "exp_skew_tokens"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_skew_tokens$diagnostics$percent_shared <- run_mcmc_diagnostics(
    exp_skew_tokens$models$percent_shared, 
    type = "percent", 
    between_condition = condition
  )
}

## Test hypotheses based on model
exp_skew_tokens$hypotheses$percent_shared <- conditional_hypothesis(
  exp_skew_tokens$models$percent_shared, 
  hypothesis = "Right-skewed > Left-skewed", 
  effect_variables = "condition", 
  conditional_variables = "none"
)

# Ordinal regression (cumulative logit) on percentage estimates rounded to nearest 5 points
if (run_ordinal) {
  exp_skew_tokens$models$percent_shared_ordinal <- brm(
    ordinal ~ 1 + condition, 
    data = exp_skew_tokens$percent_recode %>% 
      mutate(
        # Round to nearest 5 points
        ordinal = better_minus_worse %>% 
          cut_width(width = 5, center = 0, labels = FALSE) %>% as.numeric(),
        # Ordered factor with name based on central value
        ordinal = ((ordinal - 1) * 5 - 100)
      ) %>% 
      filter(
        !participant %in% exp_skew_tokens$exclusions,
        stimulus == "Shared"
      ) %>% 
      mutate(ordinal = factor(ordinal, levels = seq(from = min(ordinal), to = max(ordinal), by = 5), ordered = TRUE)), 
    family = cumulative,
    prior = priors$basic, 
    file = save_model("percent_shared_ordinal", "exp_skew_tokens"),
    cores = cores, 
    chains = chains,
    backend = "cmdstanr"
  )
}

### Save hypotheses ----

save_tidy_data(
  exp_skew_tokens$hypotheses, 
  parent_folder = here("output", "hypotheses"), 
  data_name = "exp_skew_tokens",
  save_csv = FALSE
)

## 5. Skewed distributions (types) ---------------------------------------------

### Skewness ----

#### Choices ----

exp_skew_types$models$choices <- brm(
  risky_count | trials(n_choices) ~ 1 + condition + (1 | participant),
  data = exp_skew_types$choices_summary %>% filter(
    trial_type == "Shared",
    !participant %in% exp_skew_types$exclusions
  ),
  family = binomial,
  prior = c(priors$basic, priors$varying_intercepts),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 4000, floor(150000 / chains) + 2000),
  file = save_model("choices", "exp_skew_types"),
  thin = ifelse(getOption("quick_version"), 1, 5),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_skew_types$diagnostics$choices <- run_mcmc_diagnostics(
    exp_skew_types$models$choices, 
    type = "choices", 
    between_condition = condition
  )
}

## Test hypotheses based on model
exp_skew_types$hypotheses$choices_shared <- conditional_hypothesis(
  exp_skew_types$models$choices, 
  hypothesis = "Right-skewed > Left-skewed", 
  effect_variables = "condition", 
  conditional_variables = "none"
)

#### First to mind ----

exp_skew_types$models$ftm <- brm(
  response_recode_value ~ 1 + condition,
  data = exp_skew_types$ftm_recode %>% filter(!participant %in% exp_skew_types$exclusions),
  family = categorical(refcat = "Worse"),
  prior = priors$basic_ftm,
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 2000, floor(15000 / chains) + 2000),
  file = save_model("ftm", "exp_skew_types"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_skew_types$diagnostics$ftm <- run_mcmc_diagnostics(
    exp_skew_types$models$ftm, 
    type = "ftm", 
    between_condition = condition
  )
}

## Test hypotheses based on model
exp_skew_types$hypotheses$ftm_shared <- conditional_hypothesis(
  exp_skew_types$models$ftm, 
  hypothesis = "Right-skewed > Left-skewed", 
  effect_variables = "condition", 
  conditional_variables = "none",
  outcome_level = "Better"
)

#### Percentage estimates ----

exp_skew_types$models$percent <- brm(
  bf(better_minus_worse ~ 1 + condition, sigma ~ 1 + condition),
  data = exp_skew_types$percent_recode %>% 
    filter(!participant %in% exp_skew_types$exclusions) %>% 
    mutate(better_minus_worse = standardise(better_minus_worse)),
  prior = c(priors$basic, priors$basic_sigma),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 2000, floor(15000 / chains) + 2000),
  file = save_model("percent", "exp_skew_types"),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_skew_types$diagnostics$percent <- run_mcmc_diagnostics(
    exp_skew_types$models$percent, 
    type = "percent", 
    between_condition = condition
  )
}

## Test hypotheses based on model
exp_skew_types$hypotheses$percent_shared <- conditional_hypothesis(
  exp_skew_types$models$percent, 
  hypothesis = "Right-skewed > Left-skewed", 
  effect_variables = "condition", 
  conditional_variables = "none"
)

# Ordinal regression (cumulative logit) on percentage estimates rounded to nearest 5 points
if (run_ordinal) {
  exp_skew_types$models$percent_shared_ordinal <- brm(
    ordinal ~ 1 + condition, 
    data = exp_skew_types$percent_recode %>% 
      mutate(
        # Round to nearest 5 points
        ordinal = better_minus_worse %>% 
          cut_width(width = 5, center = 0, labels = FALSE) %>% as.numeric(),
        # Ordered factor with name based on central value
        ordinal = ((ordinal - 1) * 5 - 100)
      ) %>% 
      filter(
        !participant %in% exp_skew_types$exclusions
      ) %>% 
      mutate(ordinal = factor(ordinal, levels = seq(from = min(ordinal), to = max(ordinal), by = 5), ordered = TRUE)), 
    family = cumulative,
    prior = priors$basic, 
    control = list(adapt_delta = 0.99),
    file = save_model("percent_ordinal", "exp_skew_types"),
    cores = cores, 
    chains = chains,
    backend = "cmdstanr"
  )
}

### Save hypotheses ----

save_tidy_data(
  exp_skew_types$hypotheses, 
  parent_folder = here("output", "hypotheses"), 
  data_name = "exp_skew_types",
  save_csv = FALSE
)

## 6. Temporal -----------------------------------------------------------------

### Option value and temporal order ----

#### Choices ----

exp_temporal$models$choices <- brm(
  risky_count | trials(n_choices) ~ 1 + option_value * condition + (1 + option_value | participant),
  data = exp_temporal$choices_summary %>% filter(!participant %in% exp_temporal$exclusions),
  family = binomial,
  prior = c(priors$basic, priors$varying_intercepts, priors$varying_slopes),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 3000, floor(150000 / chains) + 2000),
  file = save_model("choices", "exp_temporal"),
  thin = ifelse(getOption("quick_version"), 1, 5),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_temporal$diagnostics$choices <- run_mcmc_diagnostics(
    exp_temporal$models$choices, 
    type = "choices", 
    between_condition = condition, 
    within_condition = option_value
  )
}

## Test hypotheses based on model
exp_temporal$hypotheses$choices_interaction <- conditional_hypothesis(
  exp_temporal$models$choices, 
  hypothesis = "Random:High value > Alternating:High value", 
  effect_variables = c("condition", "option_value"), 
  conditional_variables = "none"
)

exp_temporal$hypotheses$choices_alternating <- conditional_hypothesis(
  exp_temporal$models$choices, 
  hypothesis = "High value > Low value", 
  effect_variables = "option_value", 
  conditional_variables = "condition",
  conditional_levels = "Alternating"
)

#### First to mind ----

exp_temporal$models$ftm <- brm(
  response_recode_value ~ 1 + condition * stimulus_value + (1 + stimulus_value || participant),
  data = exp_temporal$ftm_recode %>% filter(!participant %in% exp_temporal$exclusions),
  family = categorical(refcat = "Worse"),
  prior = c(priors$basic_ftm, priors$varying_intercepts_ftm),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 2000, floor(15000 / chains) + 2000),
  file = save_model("ftm_full", "exp_temporal"),
  control = list(adapt_delta = 0.9),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_temporal$diagnostics$ftm <- run_mcmc_diagnostics(
    exp_temporal$models$ftm, 
    type = "ftm", 
    between_condition = condition, 
    within_condition = stimulus_value
  )
}

## Test hypotheses based on model
exp_temporal$hypotheses$ftm_interaction <- conditional_hypothesis(
  exp_temporal$models$ftm, 
  hypothesis = "Random:High value > Alternating:High value", 
  effect_variables = c("condition", "stimulus_value"), 
  conditional_variables = "none",
  outcome_level = "Better"
)

exp_temporal$hypotheses$ftm_alternating <- conditional_hypothesis(
  exp_temporal$models$ftm, 
  hypothesis = "High value > Low value", 
  effect_variables = "stimulus_value", 
  conditional_variables = "condition",
  conditional_levels = "Alternating",
  outcome_level = "Better"
)

#### Percentage estimates ----

exp_temporal$models$percent <- brm(
  bf(
    # Varying slope removed from maximal model for convergence issues (divergences and low ESS for SD)
    # Nonetheless, these models produced very similar estimates for the parameters of interest
    better_minus_worse ~ 1 + condition * stimulus_value + (1 | participant),
    sigma ~ 1 + condition * stimulus_value
  ),
  data = exp_temporal$percent_recode %>% 
    filter(!participant %in% exp_temporal$exclusions) %>% 
    mutate(better_minus_worse = standardise(better_minus_worse)),
  prior = c(
    priors$basic, 
    priors$varying_intercepts,
    priors$basic_sigma
  ),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 2000, floor(15000 / chains) + 2000),
  file = save_model("percent_full", "exp_temporal"),
  control = list(adapt_delta = 0.9),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Generate rank histogram, density plots, and posterior predictive plots
if (run_diagnostics) {
  exp_temporal$diagnostics$percent <- run_mcmc_diagnostics(
    exp_temporal$models$percent, 
    type = "percent", 
    between_condition = condition, 
    within_condition = stimulus_value
  )
}

## Test hypotheses based on model
exp_temporal$hypotheses$percent_interaction <- conditional_hypothesis(
  exp_temporal$models$percent, 
  hypothesis = "Random:High value > Alternating:High value", 
  effect_variables = c("condition", "stimulus_value"), 
  conditional_variables = "none"
)

exp_temporal$hypotheses$percent_alternating <- conditional_hypothesis(
  exp_temporal$models$percent, 
  hypothesis = "High value > Low value", 
  effect_variables = "stimulus_value", 
  conditional_variables = "condition",
  conditional_levels = "Alternating"
)

# Ordinal regression (cumulative logit) on percentage estimates rounded to nearest 5 points
if (run_ordinal) {
  exp_temporal$models$percent_full_ordinal <- brm(
    ordinal ~ 1 + condition * stimulus_value + (1 + stimulus_value | participant), 
    data = exp_temporal$percent_recode %>% 
      mutate(
        # Round to nearest 5 points
        ordinal = better_minus_worse %>% 
          cut_width(width = 5, center = 0, labels = FALSE) %>% as.numeric(),
        # Ordered factor with name based on central value
        ordinal = ((ordinal - 1) * 5 - 100)
      ) %>% 
      filter(
        !participant %in% exp_temporal$exclusions
      ) %>% 
      mutate(ordinal = factor(ordinal, levels = seq(from = min(ordinal), to = max(ordinal), by = 5), ordered = TRUE)), 
    family = cumulative,
    prior = c(priors$basic, priors$varying_intercepts, priors$varying_slopes), 
    file = save_model("percent_full_ordinal", "exp_temporal"),
    control = list(adapt_delta = 0.99),
    cores = cores, 
    chains = chains,
    backend = "cmdstanr"
  )
}

### Save hypotheses ----

save_tidy_data(
  exp_temporal$hypotheses, 
  parent_folder = here("output", "hypotheses"), 
  data_name = "exp_temporal",
  save_csv = FALSE
)

## Combined skewness manipulations ---------------------------------------------

### Skewness ----

#### Choices ----

skewness_experiments$models$choices <- brm(
  risky_count | trials(n_choices) ~ 1 + experiment * condition + (1 | participant), 
  data = skewness_experiments$choices,
  family = binomial,
  prior =  c(priors$basic, priors$varying_intercepts),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 4000, floor(100000 / chains) + 2000),
  file = save_model("choices", "skewness_experiments"),
  control = list(adapt_delta = 0.99),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Test hypotheses based on model
skewness_experiments$hypotheses$choices_skewness <- conditional_hypothesis(
  skewness_experiments$models$choices, 
  hypothesis = "Right-skewed > Left-skewed", 
  effect_variables = "condition", 
  conditional_variables = "none"
)

#### First to mind ----

skewness_experiments$models$ftm <- brm(
  response_recode_value ~ 1 + experiment * condition,
  data = skewness_experiments$ftm,
  family = categorical(refcat = "Worse"),
  prior =  priors$basic_ftm,
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 2000, floor(20000 / chains) + 2000),
  file = save_model("ftm", "skewness_experiments"),
  control = list(adapt_delta = 0.99),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Test hypotheses based on model
skewness_experiments$hypotheses$ftm_skewness <- conditional_hypothesis(
  skewness_experiments$models$ftm, 
  hypothesis = "Right-skewed > Left-skewed", 
  effect_variables = "condition", 
  conditional_variables = "none"
)

#### Percentage estimates ----

skewness_experiments$models$percent <- brm(
  bf(
    better_minus_worse  ~ 1 + experiment * condition,
    sigma  ~ 1 + experiment * condition
  ),
  data = skewness_experiments$percent,
  prior = c(priors$basic, priors$basic_sigma),
  warmup = ifelse(getOption("quick_version"), 1000, 2000),
  iter = ifelse(getOption("quick_version"), 2000, floor(20000 / chains) + 2000),
  file = save_model("percent", "skewness_experiments"),
  control = list(adapt_delta = 0.99),
  cores = cores,
  chains = chains,
  backend = "cmdstanr"
)

## Test hypotheses based on model
skewness_experiments$hypotheses$percent_skewness <- conditional_hypothesis(
  skewness_experiments$models$percent, 
  hypothesis = "Right-skewed > Left-skewed", 
  effect_variables = "condition", 
  conditional_variables = "none"
)

### Save hypotheses ----

save_tidy_data(
  skewness_experiments$hypotheses,
  parent_folder = here("output", "hypotheses"),
  data_name = "skewness_experiments",
  save_csv = FALSE
)
