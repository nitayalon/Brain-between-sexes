applyStandartizationForFeature <- function(feature_name,
                                           subject_data,
                                           trimming_limit = 4)
{
  filtering_criteria <- !is.na(relevant_data[,feature_name]) & 
    relevant_data[,feature_name] > 0
  
  raw_feature_data <- tibble(
    eid = subject_data$eid[filtering_criteria],
    sex = subject_data$Sex[filtering_criteria],
    value = relevant_data[,feature_name][filtering_criteria])
  
  # Log
  logged_feature_data <- 
    raw_feature_data %>% 
    mutate(value = round(value, 4)) %>% 
    filter(value > 0) %>% 
    mutate(log_value = log(value))
  
  feature_data <- tibble(
    eid = logged_feature_data$eid,
    sex = logged_feature_data$sex,
    value = logged_feature_data$log_value)

  return(feature_data)
}