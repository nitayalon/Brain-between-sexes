#' Applying t-test and cohen's d test for equality of means
#' over relevant data
computeTTestAndCohensDBetweenGender <- function(feature_data
                                              ,gender_data)
{
  full_data <- cbind(feature_data, gender_data)
  full_data <- na.omit(full_data) %>% as.data.frame()
  names(full_data) <- c("value","bio_sex")
  
  full_data <- full_data %>% filter(value > 0)
  
  full_data$value <- logNormalDataPreparation(full_data$value)
  split_to_gender <- prepareDataBioBank(full_data)
  t_test <- t.test(split_to_gender$men, split_to_gender$women)
  cohens_d <- cohen.d(split_to_gender$men, split_to_gender$women)
  return(list(
    t_test = t_test,
    cohens_d = cohens_d
  ))
}