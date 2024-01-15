applyLinearModelOverBrainFeature <- function(feature_name,
                                             subject_data,
                                             total_brain_volume_data,
                                             include_age,
                                             include_sex,
                                             with_normalization=TRUE,
                                             normalized = F)
{
  filtering_criteria <- !is.na(relevant_data[,feature_name]) & 
    relevant_data[,feature_name] > 0
  
  feature_data <- tibble(
    eid = subject_data$eid[filtering_criteria],
    sex = subject_data$sex[filtering_criteria],
    age = total_brain_volume_data[filtering_criteria,]$age,
    total_brain_volume = 
      total_brain_volume_data[filtering_criteria,]$`Volume of ventricular cerebrospinal fluid` +
      total_brain_volume_data[filtering_criteria,]$`Volume of grey matter` + 
      total_brain_volume_data[filtering_criteria,]$`Volume of white matter`,
    original_value = relevant_data[,feature_name][filtering_criteria]) %>% 
    mutate(log_y = log(original_value), 
           log_volume = log(total_brain_volume)) %>% 
    mutate(population_mean = mean(log_y),
           population_std = sd(log_y)) %>% 
    mutate(low_outlier = log_y <= population_mean - 5 * population_std,
           high_outlier = log_y >= population_mean + 5 * population_std)
  if(with_normalization)
  {
    # Apply linear regression
    if(include_age)
    {
      if(include_sex)
      {
        reseidual_model <- lm(log_y ~ log_volume + age + sex*age, 
                                   data = feature_data)
      }
      else
      {
        reseidual_model <- lm(log_y ~ log_volume + age, 
                                   data = feature_data)
      }
    }
    else
    {
      reseidual_model <- lm(log_y ~ log_volume, 
                                 data = feature_data)
    }
  }
  else
  {
    reseidual_model = data.frame(residuals = feature_data$log_y)
  }
  
  standardized_reseiduals <- normalizeResiduales(reseidual_model$residuals, use_standard_data = normalized)
  feature_data$residuals = reseidual_model$residuals
  feature_data$value = standardized_reseiduals
  feature_data$outlier_flag = feature_data$low_outlier | feature_data$high_outlier
  return(feature_data)
}