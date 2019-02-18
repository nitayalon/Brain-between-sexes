#' This is a replication of the same code for real data
computeMLEOverGridMockData <- function(data){
  options(warn=-1)  
  data_MLE_per_feature <- data.frame()
  
  men_feature_data <- data$men
  women_feature_data <- data$women
  men_parameter <- computeGroupData(men_feature_data)
  women_parameter <- computeGroupData(women_feature_data)
  
  llk_results <- data.frame()
  
  mu <- men_parameter$average
  step_dir <- mu < 0 
  theta_2 <- createOuterLoopGrid(mu, step_dir)
  for(t_2 in theta_2){
    theta_1 <- createInnerLoopGrid(theta_2 = t_2, mu, step_dir)
    cart_prod <- expand.grid(t_2, theta_1)
    ### Compute the parameters
    probs_and_variance <- mapply(function(x,y){computeParameters(x,y,mu)},cart_prod$Var1,
                                 cart_prod$Var2,SIMPLIFY = FALSE)
    probs_and_variance_array <-lapply(probs_and_variance, function(x){data.frame(
      c(x[1], x[2], x[3]))})
    probs_and_variance_data_frame <- data.frame(matrix(
      unlist(probs_and_variance_array), nrow = length(probs_and_variance_array),
      ncol = 3, byrow = T), stringsAsFactors = F)
    ### Compute the loglike
    llk <- mapply(function(x,y){computeLogLikelihoodForFeature(men_feature_data, women_feature_data,
                                                               x$Var1, x$Var2,
                                                               y$X1, y$X2, y$X3, y$X3)}
                  
                  ,split(cart_prod, row(cart_prod))
                  ,split(probs_and_variance_data_frame, row(probs_and_variance_data_frame))
    )
    group_llk <- sum(llk$men_log_likelihood + llk$women_log_likelihood)
    llk_results <- rbind(llk_results,
                         cbind(cart_prod, probs_and_variance_data_frame, group_llk))  
  }
  names(llk_results) <- c("Theta2", "Theta1", "p", "q", "Sigma2", "llk")
  null_hypothesis_llk <- 
    computeLlkNullHypothesis(mu,men_feature_data, women_feature_data)
  ### Combine the results of the grid search MLE's
  mle <- findMleForFeatureFromGridSearch(llk_results) 
  data_MLE_per_feature <- rbind(data_MLE_per_feature, 
                                cbind(mle, llk_ratio = mle$llk - null_hypothesis_llk,
                                      x_bar = mu, y_bar = -mu))
  options(warn=0)
  return(data_MLE_per_feature)
}


