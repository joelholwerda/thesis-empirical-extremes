# Function that converts the conditions to an ordered factor (low, high) in title case
format_conditions <- function(string) {
  
  # Change from camel or snake case to sentence case
  sentence_case_string <- string %>% 
    # Matches upper case letters not preceded by a space, an upper case letter, or the start of the string and adds a space
    str_replace_all("(?<!([:space:]|[:upper:]|^))[:upper:]", function(match){return(paste("", match))}) %>% 
    str_replace_all("_", " ") %>% 
    str_to_sentence()
  
  # If Low/Left or High/Right appears in sentence_case_string convert to factor with Low/Left and High/Right as first levels
  low_or_high <- sentence_case_string %>% str_count("Low|High") %>% sum()
  left_or_right <- sentence_case_string %>% str_count("Left|Right") %>% sum()
  
  if (!is.na(low_or_high) & low_or_high > 0) {
    levels <- unique(sentence_case_string)
    low_index <- str_which(levels, "Low")
    high_index <- str_which(levels, "High")
    all_indexes <- 1:length(sentence_case_string)
    other_indexes <- all_indexes[c(-low_index, -high_index)]
    levels_ordered = c(levels[low_index], levels[high_index], levels[other_indexes])
    factor = factor(sentence_case_string, levels_ordered)
    
  } else if (!is.na(low_or_high) & left_or_right > 0) {
  
    levels <- unique(sentence_case_string)
    left_index <- str_which(levels, "Left")
    right_index <- str_which(levels, "Right")
    all_indexes <- 1:length(sentence_case_string)
    other_indexes <- all_indexes[c(-left_index, -right_index)]
    levels_ordered = c(levels[left_index], levels[right_index], levels[other_indexes])
    factor = factor(sentence_case_string, levels_ordered)
  
  # Otherwise convert to factor with default levels
  } else {
    
    factor = factor(sentence_case_string)
    
  }
  
  return(factor)
}

