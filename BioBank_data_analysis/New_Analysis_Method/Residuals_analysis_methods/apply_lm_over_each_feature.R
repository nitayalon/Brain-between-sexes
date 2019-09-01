applyLinearModelOverBrainFeature <- function(feature_name,
                                             subject_data,
                                             total_brain_volume_data,
                                             trimming_limit = 4)
{
  filtering_criteria <- !is.na(relevant_data[,feature_name]) & 
    relevant_data[,feature_name] > 0
  
  feature_data <- tibble(
    eid = subject_data$eid[filtering_criteria],
    sex = subject_data$Sex[filtering_criteria],
    value = relevant_data[,feature_name][filtering_criteria])
  
  data_for_lm <- tibble(y = feature_data$value,
                        x1 = total_brain_volume_data[filtering_criteria,1],
                        x2 = total_brain_volume_data[filtering_criteria,2])
  # Log
  data_for_lm_log_scale <- data_for_lm %>% 
    filter(y > 0) %>% 
    mutate(log_y = log(y), log_x1 = log(x1), log_x2 = log(x2))
  
  # Removing outliers
  upper_and_lower <- quantile(data_for_lm_log_scale$log_y, c(0.025,0.975))
  estimated_std <- (upper_and_lower[2] - upper_and_lower[1]) / 4
  data_for_lm_log_scale$trimmed_log_y <- Winsorize(data_for_lm_log_scale$log_y, 
                             minval = mean(data_for_lm_log_scale$log_y) - trimming_limit * estimated_std,
                             maxval = mean(data_for_lm_log_scale$log_y) + trimming_limit * estimated_std, 
                             probs = c(0.025,0.975))

  residual_model <- lm(trimmed_log_y ~ log_x1 + log_x2, 
                             data = data_for_lm_log_scale)
  
  residuals <- tibble(
    eid = feature_data$eid,
    sex = feature_data$sex,
    value = residual_model$residuals)
  
  standardized_residuals <- 
    residuals %>% 
    normalizeResiduales()
  
  return(standardized_residuals)
}