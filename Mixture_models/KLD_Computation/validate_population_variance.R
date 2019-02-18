#' Validating population variance:
#' Following the talk with Isaco, I'm validating the population varinace
validatePopVariance <- function(sample_size = 1e4, 
                                number_of_parameter_set = 200,
                             create_auto_grid = T){
  parameters <- createParameters(create_auto_grid,xi,epsilon,delta)
  parameter_set <- parameters[sample(1:nrow(parameters), number_of_parameter_set, F), ]
  var_mean <- var_var <- rep(NA,number_of_parameter_set)
  chi_square_estimator_vec <- rep(NA,number_of_parameter_set)
  for(r in 1:nrow(parameter_set))
  {
    parameter_row <- parameter_set[r,]
    samples <- sampleFromMixtureModel(parameter_row$xi, parameter_row$epsilon, parameter_row$delta,
                                      parameter_row$p, parameter_row$q, parameter_row$group_variance,
                                      sample_size)
    pop <- c(samples$men, samples$women)
    chi_square_estimator <- sum((pop - mean(pop))^2)
    validation_df <- validateMarginalDistributions(samples)
    var_mean[r] = validation_df[1]
    var_var[r] = validation_df[2]
    chi_square_estimator_vec[r] <- chi_square_estimator
  }
  return(list(var_mean = var_mean
              ,var_var = var_var
              ,chi_square_estimator_vec = chi_square_estimator_vec))
}

pop_variance_100 <- validatePopVariance(number_of_parameter_set = 100)
hist(pop_variance_100$var_mean)
hist(pop_variance_100$var_var)
mean(pop_variance_100$var_mean)
mean(pop_variance_100$var_var)
sd(pop_variance_100$var_mean)
sd(pop_variance_100$var_var)

pop_variance_1000 <- validatePopVariance(number_of_parameter_set = 10000)
mean(pop_variance_1000$var_mean)
mean(pop_variance_1000$var_var)
sd(pop_variance_1000$var_mean)
sd(pop_variance_1000$var_var)


hist(pop_variance_1000$chi_square_estimator_vec,breaks = 200, freq = F)
lines(density(rchisq(1e5,20000)),col = "red")
