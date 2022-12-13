wrangle_percent_csv <- function(raw_data, exp_info){
  
  if (exp_info$experiment_name == "exp_skew_types") {
    percent <- raw_data$percent %>% 
      mutate(
        `30` = map(responses, ~ fromJSON(.x)[1]) %>% unlist() %>% as.character(),
        `70` = map(responses, ~ fromJSON(.x)[2]) %>% unlist() %>% as.character()
      ) %>% 
      select(
        participant,
        condition,
        stimulus = option,
        `30`,
        `70`
      ) %>% 
      gather(key = "label", value = "response", `30`, `70`) %>% 
      mutate(response_clean = response %>% str_remove("%") %>% as.numeric())
    
  } else if (exp_info$experiment_name == "exp_skew_tokens") {
    percent <- raw_data$percent %>% 
      mutate(
        json = map(responses, ~ fromJSON(.x)),
        low = case_when(
          option == "low_risky" ~ map(json, ~ paste("0", .x[["0 points:"]])),
          option == "decision_risky" ~ map(json, ~ paste("30", .x[["30 points:"]])),
          option == "high_risky" ~ map(json, ~ paste("80", .x[["80 points:"]]))
        ) %>% unlist(),
        high = case_when(
          option == "low_risky" ~ map(json, ~ paste("20", .x[["20 points:"]])),
          option == "decision_risky" ~ map(json, ~ paste("70", .x[["70 points:"]])),
          option == "high_risky" ~ map(json, ~ paste("100", .x[["100 points:"]]))
        ) %>% unlist(),
        other = map(json, ~ paste("Other", .x[["Other:"]])) %>% unlist()
      ) %>% 
      gather(key = label_type, value = response, low, high, other) %>%
      mutate(
        label = map(response, ~ str_split(.x, " ", simplify = TRUE)[[1]]) %>% unlist(),
        response = map(response, ~ str_split(.x, " ", simplify = TRUE)[[2]]) %>% unlist(),
        response_clean = case_when(
          response == "" ~ "0",
          TRUE ~ response %>% str_remove("%")
        )
      ) %>% 
      select(
        participant,
        condition,
        stimulus = option,
        label,
        label_type,
        response,
        response_clean
      )
  }
  
  # Reformat condition names and convert to factors
  percent <- percent %>% 
    mutate(
      # Relabel the conditions based on skewness rather than rank of the shared option
      condition = case_when(
        condition == "lowRank" ~ "Left-skewed",
        condition == "highRank" ~ "Right-skewed",
        TRUE ~ condition
      ) %>% 
        format_conditions(),
    )
  
  # Test that the data was processed correctly and that there is no incomplete or duplicate data
  test_correct_dimensions(percent, exp_info, "percent")
  
  return(percent)
  
}