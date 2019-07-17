applyStandartizationForFeature <- function(feature_name,
                                             trimming_limit = 3,
                                             subject_data)
{
  filtering_criteria <- !is.na(full_relevant_data[,feature_name]) & 
    full_relevant_data[,feature_name] > 0
  
  feature_data <- tibble(
    eid = subject_data$eid[filtering_criteria],
    sex = full_relevant_data$Sex[filtering_criteria],
    value = full_relevant_data[,feature_name][filtering_criteria])
  
  feature_data <- 
    feature_data %>% 
    normalizeResiduales()
  return(feature_data)
}