mainFunction <- function(sample_size = 1e3, 
                         create_auto_grid = F,
                         xi = NULL, epsilon = NULL, 
                         delta = NULL){
  ### First - generate the parameters:
  parameters <- createParameters(create_auto_grid,xi,epsilon,delta)
  ### Second - for each row sample observations and compute KLD:
  number_of_samples <- nrow(parameters)
  place_holder <- rep(NA, number_of_samples)
  results_data_frame <- data.frame(xi = place_holder,
                                   epsilon = place_holder,
                                   delta = place_holder,
                                   p = place_holder,
                                   q = place_holder,
                                   male_mean = place_holder,
                                   female_mean = place_holder,
                                   male_var = place_holder,
                                   female_var = place_holder,
                                   men_mean = place_holder,
                                   women_mean = place_holder,
                                   men_var = place_holder,
                                   women_var = place_holder,
                                   pop_mean = place_holder,
                                   pop_var = place_holder,
                                   KLD_pop = place_holder,
                                   KLD_men = place_holder,
                                   KLD_women = place_holder)
  
  for(i in 1:number_of_samples){
    parameter_row = parameters[i,]  
    results_data_frame[i,]$xi = parameter_row$xi   
    results_data_frame[i,]$epsilon = parameter_row$epsilon   
    results_data_frame[i,]$delta = parameter_row$delta   
    results_data_frame[i,]$p = with(parameter_row, (2 * xi + delta) / (2 * xi + epsilon + delta))
    results_data_frame[i,]$q = with(parameter_row, (delta) / (2 * xi + epsilon + delta))
    results_data_frame[i,]$male_mean = with(parameter_row, xi + epsilon)
    results_data_frame[i,]$female_mean = with(parameter_row, -xi - delta)
    results_data_frame[i,]$male_var = with(parameter_row, group_variance)
    results_data_frame[i,]$female_var = with(parameter_row, group_variance)
    results_data_frame[i,]$men_mean = with(parameter_row, xi)
    results_data_frame[i,]$women_mean = with(parameter_row, -xi)
    results_data_frame[i,]$women_var = results_data_frame[i,]$men_var =
      with(parameter_row, 1-xi^2)
    
    samples <- sampleFromMixtureModel(parameter_row$xi, parameter_row$epsilon, parameter_row$delta,
                                      parameter_row$p, parameter_row$q, parameter_row$group_variance,
                                      sample_size)
    validation_df <- validateMarginalDistributions(samples)
    results_data_frame[i,]$pop_mean = validation_df[1]
    results_data_frame[i,]$pop_var = validation_df[2]
    KLD <- computeKLD(samples,parameter_row,method = "Numeric_Integration")
    results_data_frame[i,]$KLD_pop <- KLD$KLD_estimate
    if(KLD$KLD_estimate > 2){
      cat(paste0("Iteration number ",i),"\n")
      cat(paste0("KLD = ",KLD$KLD_estimate),"\n")
    }
    results_data_frame[i,]$KLD_men <- KLD$KLD_estimate_men
    results_data_frame[i,]$KLD_women <- KLD$KLD_estimate_women
  }
  return(results_data_frame)
}