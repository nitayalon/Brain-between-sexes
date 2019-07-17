### Validating the llk computation method

source("../../KLD_Computation/generate_sample_from_mixture_model.R")
createMockData <- function(sample_size = 1e3)
{
  params <- createParameters()
  ind <- sample(nrow(params),1)
  parameter_row <- params[ind,]
  obs <- sampleFromMixtureModel(parameter_row$xi, parameter_row$epsilon, parameter_row$delta,
                                parameter_row$p, parameter_row$q, parameter_row$group_variance,sample_size)
  standerdize_data <- scale(c(obs$men,obs$women))
  feature_data <- data.frame(val = standerdize_data, sex = c(rep(1,sample_size),rep(2,sample_size)))  
  return(list(
    true_params = parameter_row,
    feature_data = feature_data))
}

feat <- createMockData()
feat$true_params
res <- logNormalGridSearch(feat$feature_data)
View(res$llrt_over_grid)
