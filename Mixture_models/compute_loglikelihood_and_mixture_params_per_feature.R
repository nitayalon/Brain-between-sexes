computeParametersAndLogLikelihoodRatioPerFeature <- function(men, women){
  n <- length(men)
  m <- length(women)
  mixture_probabilities_and_llk_ratio <- runLikelihoodComputation(men,women,n,m,0.5,0.5, plot = F)
  return(mixture_probabilities_and_llk_ratio)
}
