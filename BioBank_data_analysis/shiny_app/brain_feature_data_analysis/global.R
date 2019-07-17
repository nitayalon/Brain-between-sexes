removeZeroData <- function(feature_data,feature_name)
{
  return(feature_data[feature_data[feature_name] > 0,])
}

logNormalDataPreparation <- function(feature_data,
                                     trimming = F)
{
  stopifnot(all(feature_data > 0))
  feature_data <- prepareDataForLogTransformation(feature_data)
  logged_data <- log(feature_data)
  scaled_data <- scale(logged_data)
  trimmed_data <- trimmExtreme(scaled_data)
  trimmed_and_scaled <- scale(trimmed_data)
  return(trimmed_and_scaled)
}

prepareDataForLogTransformation <- function(feature_data) {
  sorted_data <- sort(feature_data)
  second_smallest <- sorted_data[which.max(sorted_data > 0)]
  results <- ifelse(feature_data > second_smallest, feature_data, second_smallest)
  return(results)
}


trimmExtreme <- function(scaled_data)
{
  # if(mean(scaled_data) != 0)
  # {
  #   warning("Sure that the data is scaled?")
  # }
  trim_below <- ifelse(scaled_data > -5, scaled_data, -5)
  trim_above <- ifelse(trim_below < 10 , trim_below, 10)
  return(trim_above)
}

plot_mix_comps <- function(x, mu, sigma, lambda) {
  lambda * dnorm(x, mu, sigma)
}