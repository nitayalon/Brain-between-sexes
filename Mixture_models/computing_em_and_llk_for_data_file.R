main <- function(data_file){
  path_to_file <- file.path("~/mastersdegree/Thesis/DAPHNA_JOEL/Data", data_file)
  dat <- read.csv(path_to_file)
  features <- names(dat)
  men_data <- dat[dat$"Gender" == 1,]
  women_data <- dat[dat$"Gender" == 2,]
  results <- data.frame(p = NULL,
                        q = NULL, 
                        llk_ratio = NULL,
                        stringsAsFactors = FALSE)
  for(feature in features[-1]){
    men_feature_data <- men_data[,feature]
    women_feature_data <- women_data[,feature]
    feature_results <- computeParametersAndLogLikelihoodRatioPerFeature(men_feature_data,
                                                                        women_feature_data)
    results <- rbind(results, c(
      feature_results$x_bar,
      feature_results$y_bar,
      feature_results$theta_mas,
      feature_results$theta_fem,
      feature_results$sigma_2,
      feature_results$p,
      feature_results$q,
      feature_results$llk_ratio))
  }
  results <- cbind(rep(data_file, length(features[-1])), features[-1], results)
  names(results) <- c("Data File", "feature","x_bar", "y_bar"
                      ,"theta_mas", "theta_fem", "sigma2",
                      "p", "q","llk_delta")
  return(results)
  }
