prepareDataForLogTransformation <- function(feature_data) {
  sorted_data <- sort(feature_data)
  second_smallest <- sorted_data[which.max(sorted_data > 0)]
  results <- ifelse(feature_data > second_smallest, feature_data, second_smallest)
  return(results)
}


