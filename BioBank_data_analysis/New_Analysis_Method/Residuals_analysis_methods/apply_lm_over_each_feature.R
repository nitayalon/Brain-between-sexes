applyLinearModelOverBrainFeature <- function(feature_name,
                                             trimming_limit = 3,
                                             subject_data)
{
  filtering_criteria <- !is.na(relevant_data[,feature_name]) & 
    relevant_data[,feature_name] > 0
  
  feature_data <- tibble(
    eid = subject_data$eid[filtering_criteria],
    sex = subject_data$Sex[filtering_criteria],
    value = relevant_data[,feature_name][filtering_criteria])
  
  total_brain_volume_data <- 
    bio.bank.data[filtering_criteria,] %>% 
    select("X25010.2.0","X25004.2.0")  
  
  data_for_lm <- tibble(y = feature_data$value,
                        x1 = total_brain_volume_data[,1],
                        x2 = total_brain_volume_data[,2])
  data_for_lm_log_scale <- 
    data_for_lm %>% 
    filter(y > 0) %>% 
    mutate(log_y = log(y), log_x1 = log(x1), log_x2 = log(x2))
  
  lm_for_first_feature <- lm(log_y ~ log_x1 + log_x2, 
                             data = data_for_lm_log_scale)
  
  residuals_first_feature <- tibble(
    eid = feature_data$eid,
    sex = feature_data$sex,
    value = lm_for_first_feature$residuals)
  
  residuals_first_feature <- 
    residuals_first_feature %>% 
    normalizeResiduales()
  return(residuals_first_feature)
}