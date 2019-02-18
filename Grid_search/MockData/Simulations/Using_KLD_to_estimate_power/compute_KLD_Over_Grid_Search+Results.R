#' In this script we'll apply the KLD under alternative over the feature data
#' and the MLE computed by the grid search
computeKLDOverFile <- function(data_file, grid_search_results){
  
  feature_list <- names(data_file)[-1]       ### This is the list of brain feature
  by_gender <- splitDataByGender(data_file)  ### Deviding the data into men and women
  KLD_per_feature <- data.frame()
  
  for(feature in feature_list){
    men_feature_data <- by_gender$men_data[,feature]
    women_feature_data <- by_gender$women_data[,feature]
    men_parameter <- computeGroupData(men_feature_data)
    women_parameter <- computeGroupData(women_feature_data)
    ### Data for the KLD computation:
    data_for_KLD <- grid_search_results[grid_search_results$feature_list == feature,]
    p <- data_for_KLD$p
    theta_men <- data_for_KLD$Theta2
    theta_women <- data_for_KLD$Theta1
    male_var <- female_var <- data_for_KLD$Sigma2
    men_mu <- data_for_KLD$x_bar
    men_var <- var(men_feature_data)
    KLD_results_for_feature <- simpleKLDComputation(men_feature_data, p, theta_men, theta_women,male_var,
                                        female_var, men_mu, men_var)
    KLD_per_feature <- rbind(KLD_per_feature,KLD_results_for_feature)
  }
  final_results <- cbind(feature_list, KLD_per_feature)
  return(final_results)
}

