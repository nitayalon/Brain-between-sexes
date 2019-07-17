runLikelihoodComputation <- function(men,women,n,m, p_start = 0.8, q_start = 0.2, plot = T){
  
  pop <- c(men, women)
  m1 <- mean(men)
  m2 <- mean(women)
  s1 <- var(men)
  s2 <- var(women)
  s <- ((m - 1) * s1 + (n - 1) * s2) / (n + m - 2)
  
  if(plot){
    plot(density(men), col = 1, main = column)
    lines(density(women), col = 2)
  }
  
  EM_results <- EM(p_start, q_start, s, men, women, m1, m2, 2e3, 0.00001)
  
  full_model <- computeLogLikelihood(men, women, EM_results$mu_1, 
                                     EM_results$mu_2, EM_results$p,EM_results$q,
                                     EM_results$sigma_2)
  partial_model <- computeLogLikelihood(men, women, m1, m2, 1, 0, s)
  
  results <- list(x_bar = m1,
                  y_bar = m2,
                  theta_mas = EM_results$mu_1,
                  theta_fem = EM_results$mu_2,
                  sigma_2 = EM_results$sigma_2,
                  p = EM_results$p,
                  q = EM_results$q,
                  llk_ratio = full_model - partial_model)
  return(results)
}
