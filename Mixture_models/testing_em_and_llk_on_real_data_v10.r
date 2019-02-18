library(data.table)
source("~/mastersdegree/Thesis/DAPHNA_JOEL/Mixture_models/source_script.R")
setwd("~/MastersDegree/Thesis/DAPHNA_JOEL/Results/EM_Results")

main <- function(data_file){
  path_to_file <- file.path("~/mastersdegree/Thesis/DAPHNA_JOEL/Data", data_file)
  dat <- read.csv(path_to_file)
  features <- names(dat)
  men_data <- dat[dat$"Gender" == 1,]
  women_data <- dat[dat$"Gender" == 2,]
  results <- data.frame(feature = NULL, p = NULL, q = NULL, llk_ratio = NULL,
                        stringsAsFactors = FALSE)
  for(feature in features[-1]){
    men_feature_data <- men_data[,feature]
    women_feature_data <- women_data[,feature]
    feature_results <- computeParametersAndLogLikelihoodRatioPerFeature(men_feature_data,
                                                                        women_feature_data)
    browser()
    results <- rbind(results, c(feature,feature_results$p,feature_results$q,
                                feature_results$llk_ratio))
  }
  names(results) <- c("feature", "p", "q","llk_delta")
  return(results)
  }

res <- main("data_GSP_thickness.csv")
