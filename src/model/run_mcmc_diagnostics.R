run_mcmc_diagnostics <- function(brms_object, type, between_condition, within_condition = "none", responses = "none", n_samples = 50) {
  
  # Quote the arguments
  responses_quoted <- ensym(responses)
  between_quoted <- ensym(between_condition)
  within_quoted <- ensym(within_condition)
  
  # Check type argument
  if (!type %in% c("choices", "ftm", "percent")) {stop("type must be either 'choices', 'ftm', or 'percent'.")}
  
  diagnostics <- list()
  
  diagnostics$rank_histogram <- brms_object %>% mcmc_rank_hist(regex_pars = "b_|sd_|cor_") +
    theme(strip.text.y = element_text(angle = 0, hjust = 0))
  
  diagnostics$density <- brms_object %>% mcmc_dens_overlay(regex_pars = "b_|sd_|cor_")
  
  
  if (type == "choices") {
    if (expr_text(within_quoted) != "none") {
      brms_object$data <- brms_object$data %>% mutate(combined_condition = paste(!!between_quoted, !!within_quoted, sep = ", "))
      diagnostics$posterior_predictive <- brms_object %>% 
        brms::pp_check(type = "dens_overlay_grouped", group = "combined_condition", ndraws = 50)
    } else {
      diagnostics$posterior_predictive <- brms_object %>% 
        brms::pp_check(type = "dens_overlay_grouped", group = between_quoted, ndraws = 50)
    }
  } else if (type == "ftm") {
    if (expr_text(within_quoted) != "none") {
      brms_object$data <- brms_object$data %>% mutate(combined_condition = paste(!!between_quoted, !!within_quoted, sep = ", "))
      diagnostics$posterior_predictive <- brms_object %>% 
        brms::pp_check(type = "bars_grouped", group = "combined_condition", ndraws = 50)
    } else {
      diagnostics$posterior_predictive <- brms_object %>% 
        brms::pp_check(type = "bars_grouped", group = between_quoted, ndraws = 50)
    }
  } else if (type == "percent") {
    diagnostics$posterior_predictive <- pp_check_percent(
      brms_object, 
      !!responses_quoted, 
      !!between_quoted, 
      !!within_quoted
    )
  }
  
  return(diagnostics)
  
}
