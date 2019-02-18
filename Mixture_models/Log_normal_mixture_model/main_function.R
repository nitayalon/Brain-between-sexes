# This is the main function for computing log-likelihood ratio test for
# mixture of mixture model as part of the brain research 
# the function takes as input a data file and returns a table with all the statistics
library(dplyr)
mainFunctionComputeLikelihoodRatioOverFile <- function(data)
{
  # Sometimes instead fo bio sex there's a Sex column
  if("sex" %in% tolower(names(data)))
  {
    levels(data$sex) <- c("M","F")
    data$bio_sex <- as.numeric(data$sex)
    data <- subset(data, select = -c(sex))
  }
  
  # Adding gender (if missing) 
  men_ind <- data$bio_sex == 1
  women_ind <- data$bio_sex == 2
  
  group_ind_1 <- checkEqualSizes(men_ind,women_ind,length(data$bio_sex))
  # Filter fo equal group sizes
  equal_size_group <- data[c(group_ind_1$men_ind,group_ind_1$women_ind),]   
  
  men_ind <- equal_size_group$bio_sex == 1
  women_ind <- equal_size_group$bio_sex == 2
  group_ind <- data.frame(men_ind = men_ind, women_ind = women_ind)
  # Log and standard normalizing 
  feature_dat <- subset(data,select = -c(bio_sex,age))
  
  select_feature_at_random <- sample(names(feature_dat),1)
  print(select_feature_at_random)
  # MLE - EM/Grid search (per feature)
  results <- computeStatisticsForFeature(feature_dat[,select_feature_at_random],group_ind)  
  # results <- sapply(feature_dat, function(x){computeStatisticsForFeature(x,group_ind)})  
  
  results$EM_results %<>% data.frame()
  rownames(results$EM_results) <- c("x_bar", "y_bar"
                      ,"theta_mas", "theta_fem", "sigma2",
                      "p", "q","llk_delta")
  # rownames(results) <- c("x_bar", "y_bar"
  #                     ,"theta_mas", "theta_fem", "sigma2",
  #                     "p", "q","llk_delta")
  return(list(
    EM_results = results$EM_results,
    EM_Data = results$EM_data
     ))
}


computeStatisticsForFeature <- function(feature,group_ind)
{
  stopifnot(sum(group_ind$men_ind) == sum(group_ind$women_ind))
  stopifnot(all(feature > 0))
  
  logged_feature <- log(feature)
  standardize_feature <- scale(logged_feature)
  
  men_data <- standardize_feature[which(group_ind$men_ind),]
  women_data <- standardize_feature[which(group_ind$women_ind),]
  feature_results <- computeParametersAndLogLikelihoodRatioPerFeature(men_data,women_data)
  # hist(men_data, col = rgb(1,0,0,alpha = 0.2))
  # hist(women_data, col = rgb(1,0,1,alpha = 0.2),add = T)
  EM_data <- cbind(men_data,women_data) %>% 
    data.frame()
  
  names(EM_data) <- c("mem","women")
  
  results <- list(
    EM_results = c(
    feature_results$x_bar,
    feature_results$y_bar,
    feature_results$theta_mas,
    feature_results$theta_fem,
    feature_results$sigma_2,
    feature_results$p,
    feature_results$q,
    feature_results$llk_ratio),
    EM_data = EM_data
  )
  return(results)
} 
