#' In this script our goal is to check if the EM algorithm for mixture model
#' does converge to the MLE or not
#' The tests are as follows:
#' 1. we compute the likelihood at the MLE's
#' 2. we create a small pertubation on the MLE's and compute the llk 
#' 3. validating that the MLE is indeed highest at the MLE

setwd("~/mastersdegree/Thesis/DAPHNA_JOEL/Mixture_models/Log_normal_mixture_model/EM_Results/")
# Extracting EM results
EM_results <- read.csv("GSP_FS_thickness+sex+age.csv",header = T)
# Extracting the original men-women data
EM_Data <- read.csv("GSP_FS_thickness+sex+age_raw_data.csv",header = T)

#### Computing LLK at the MLE:
validateMLE <- function(men_data,women_data,
                        MLE_data,epsilon = 1e-1)
{
  theta_mas = MLE["theta_mas"]
  theta_fem = MLE["theta_fem"]
  p = MLE["p"]
  q = MLE["q"]
  sigma_2 = MLE["sigma2"]
  maximum.llk <- computeLogLikelihood(men,women,theta_mas,theta_fem,p,q,sigma_2)
  llk_results <- data.frame(parameter = rep(0,length(names(MLE_data))),
                            maximum_llk = rep(-maximum.llk,length(names(MLE_data))),
                            left_llk = rep(0,length(names(MLE_data))),
                            right_llk = rep(0,length(names(MLE_data))))
  for(i in 1:length(names(MLE_data)))
  {
    working_param <- MLE_data[i]
    parameter <- MLE_data[-i]
    left_params <- c(parameter,working_param - epsilon)
    right_params <- c(parameter,working_param + epsilon)
    
    left_llk <- computeLogLikelihood(men,women,left_params["theta_mas"]
                                     ,left_params["theta_fem"],
                                     left_params["p"],
                                     left_params["q"],
                                     left_params["sigma2"])
    right_llk <- computeLogLikelihood(men,women,right_params["theta_mas"]
                                     ,right_params["theta_fem"],
                                     right_params["p"],
                                     right_params["q"],
                                     right_params["sigma2"])
    
    llk_results$parameter[i] <- names(MLE_data)[i]
    llk_results$left_llk[i] <- -left_llk
    llk_results$right_llk[i] <- -right_llk
  }
  return(llk_results)
}

res <- validateMLE(men,women,MLE[c("theta_mas","theta_fem","p","q","sigma2")])

