# Method to apply log and Standrtization of the data - pre MLE
applyLogAndStandrtization <- function(feature_vec,group_ind,...){
  
  stopifnot(all(feature_vec > 0))
  stopifnot(length(group_ind$men_ind) == length(group_ind$women_ind))
  
  men_data <- feature_vec[group_ind$men_ind]
  women_data <- feature_vec[group_ind$women_ind]
  pop <- c(men_data,women_data)
  logged_feature <- log(pop)
  standardize_feature <- scale(logged_feature)
  
  return(standardize_feature)
} 
