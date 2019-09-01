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
  
  upper_and_lower <- quantile(feature_data$value, c(0.025,0.975))
  estimated_std <- (upper_and_lower[2] - upper_and_lower[1]) / 4
  feature_data$value <- Winsorize(feature_data$value, 
                                  minval = mean(feature_data$value) - trimming_limit * estimated_std,
                                  maxval = mean(feature_data$value) + trimming_limit * estimated_std, 
                                  probs = c(0.025,0.975))
  feature_data <- 
    feature_data %>% 
    normalizeResiduales()
  return(feature_data)
}