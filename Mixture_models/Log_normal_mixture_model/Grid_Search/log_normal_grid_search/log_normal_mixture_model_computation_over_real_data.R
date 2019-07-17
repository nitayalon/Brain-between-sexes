# This is the main function for computing log-likelihood ratio test for
# mixture of mixture model as part of the brain research 
# the function takes as input a data file and returns a table with all the statistics
library(dplyr)

logNormalMixtureModelComputationOverRealData <- function(data,
                                                         single_feature = T,
                                                         apply_log = F)
{
  names(data) <- tolower(names(data))
  # Sometimes instead fo bio sex there's a Sex column
  if("sex" %in% names(data))
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
  feature_dat <- data[ , !(names(data) %in% to_remove_list)]
  
  if(single_feature)
  {
    select_feature_at_random <- sample(names(feature_dat),1)
    standard_data <- standardazieData(feature_dat[,select_feature_at_random],group_ind,apply_log)  
    # MLE - EM/Grid search (per feature)
    log_noraml_grid_search_results <- logNormalGridSearch(standard_data$standard_data)
    return(
      list(
      feature_name = select_feature_at_random,
      grid_search_results = log_noraml_grid_search_results,
      data_means = standard_data$gender_data
        )
      )
  }
  else
  {
    full_data_results <- list()
    llr_per_feature <- list()
    for(i in 1:length(names(feature_dat)))
    {
      feature_name = names(feature_dat)[i]
      standard_data <- standardazieData(feature_dat[,feature_name],group_ind,apply_log)  
      # MLE - EM/Grid search (per feature)
      log_noraml_grid_search_results <- logNormalGridSearch(standard_data$standard_data)
      MLE_ind <- which.min(-1 * log_noraml_grid_search_results$llrt_over_grid$llk)
      MLE <- log_noraml_grid_search_results$llrt_over_grid[MLE_ind,]
      full_data_results[[i]] = 
          list(
          feature_name = feature_name,
          MLE = MLE,
          grid_search_results = log_noraml_grid_search_results,
          data_means = standard_data$gender_data
        )
      llr_per_feature[[i]] <- c(feature_name,MLE$llr)
    }
    names(llr_per_feature) <- c("Feature","llr")
    llr_per_feature$llr <- as.numeric(llr_per_feature$llr)
    return(
      list(
        full_data_results = full_data_results,
        llr_per_feature = llr_per_feature
      )
    )
  }
}


standardazieData <- function(feature,group_ind,apply_log = F)
{
  stopifnot(sum(group_ind$men_ind) == sum(group_ind$women_ind))
  stopifnot(all(feature > 0))
  
  ### first sub sample - then log and scale
  sub_sample <- feature[which(group_ind$men_ind | group_ind$women_ind)]
  logged_feature <-
    if(apply_log)
      {
         log(sub_sample)
      }
    else
      {
        sub_sample
      }
  standardize_feature <- scale(logged_feature)
  
  men_data <- standardize_feature[which(group_ind$men_ind),]
  women_data <- standardize_feature[which(group_ind$women_ind),]
  gender_data = data.frame(men_mean = mean(men_data), women_mean = mean(women_data),
                           men_var = var(men_data), women_var = var(women_data))
  
  n <- length(men_data)
  sex <- c(rep(1,n),rep(2,n))
  options(stringsAsFactors=T)
  return(list(
    standard_data = data.frame(val = c(men_data,women_data), sex),
    gender_data = gender_data
  ))
} 
