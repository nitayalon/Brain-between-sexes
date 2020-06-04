bootstrap_data_sample <- function(feature_data){
  m = length(feature_data$men)
  n = length(feature_data$women)
  results = list()
  results$men = sample(feature_data$men, m, replace = T)
  results$women = sample(feature_data$women, n, replace = T)
  return(results)
}