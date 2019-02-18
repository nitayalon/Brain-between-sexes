# Data preparation for hypothesis testing
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

normalDataPreparation <- function(feature_data)
{
  scaled_data <- scale(feature_data)
  trimmed_data <- trimmExtreme(scaled_data)
  trimmed_and_scaled <- scale(trimmed_data)
  return(trimmed_and_scaled)
}

trimmExtreme <- function(scaled_data)
{
  if(mean(scaled_data) != 0)
  {
    warning("Sure that the data is scaled?")
  }
  trim_below <- ifelse(scaled_data > -5, scaled_data, -5)
  trim_above <- ifelse(trim_below < 10 , trim_below, 10)
  return(trim_above)
}