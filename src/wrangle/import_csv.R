import_csv <- function(exp_info){
  
  data_types <- c("demographics", "bandit", "ftm", "percentage")
  
  for (data in data_types) {
    path <- here::here(exp_info$load_location, data)
    
    files <- file.path(path, list.files(path = path, pattern = "*.csv"))
    
    temp_data <- files %>%
      map(read_csv, col_types = cols()) %>% 
      bind_rows()
    
    assign(data, temp_data)
    
  }
  
  experiment_data <- list(
    participant_info = demographics,
    choices = bandit,
    ftm = ftm,
    percent = percentage
  )
}