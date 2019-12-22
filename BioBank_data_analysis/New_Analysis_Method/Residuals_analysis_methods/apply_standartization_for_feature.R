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
    filter(value > 0) %>% 
    mutate(log_value = log(value))
  
  # Removing outliers
  logged_feature_data$trimmed_log_value <- 
    Winsorize(logged_feature_data$log_value,
              minval = quantile(logged_feature_data$log_value, 0.001),
              maxval = quantile(logged_feature_data$log_value, 0.999))
  
  final_feature_data <- tibble(
    eid = logged_feature_data$eid,
    sex = logged_feature_data$sex,
    value = logged_feature_data$trimmed_log_value)
  
  feature_data <- 
    final_feature_data %>% 
    normalizeResiduales()
  
  return(feature_data)
}