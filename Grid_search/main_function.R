mainFunctionGridSearch <- function(file_name){
  data <- read.csv(
    file.path("~/mastersdegree/Thesis/DAPHNA_JOEL/Data/",
              file_name))
  mle_per_feature <- computeMLEOverGridMixtureModel(dat)
  return(cbind(file_name, mle_per_feature))
}