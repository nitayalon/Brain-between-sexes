applyStandartizationForFeature <- function(feature_name,
                                             trimming_limit = 3,
                                             subject_data)
{
  filtering_criteria <- !is.na(relevant_data[,feature_name]) & 
    relevant_data[,feature_name] > 0
  
  feature_data <- tibble(
    eid = subject_data$eid[filtering_criteria],
    sex = subject_data$Sex[filtering_criteria],
    value = relevant_data[,feature_name][filtering_criteria])
  
  feature_data <- 
    feature_data %>% 
    normalizeResiduales()
  return(feature_data)
}