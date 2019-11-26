applyStandartizationForFeature <- function(feature_name,
                                           subject_data,
                                           trimming_limit = 4)
{
  filtering_criteria <- !is.na(relevant_data[,feature_name]) & 
    relevant_data[,feature_name] > 0
  
  feature_data <- tibble(
    eid = subject_data$eid[filtering_criteria],
    sex = subject_data$Sex[filtering_criteria],
    value = relevant_data[,feature_name][filtering_criteria])
  
  # Log
  feature_data <- 
    feature_data %>% 
    filter(value > 0) %>% 
    mutate(log_value = log(value))
  
  # Removing outliers
  feature_data$trimmed_log_value <- 
    Winsorize(feature_data$log_value, 
    probs = c(0.01,0.99))
  
  feature_data <- tibble(
    eid = feature_data$eid,
    sex = feature_data$sex,
    value = feature_data$trimmed_log_value)
  
  feature_data <- 
    feature_data %>% 
    normalizeResiduales()
  
  return(feature_data)
}