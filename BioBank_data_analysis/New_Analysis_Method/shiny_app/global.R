library(ggplot2)
library(dplyr)

plot_mix_comps <- function(x, mu, sigma, lambda) {
  lambda * dnorm(x, mu, sigma)
}

summarizeFullBrainFeature <- function(full_analysis_results) 
{
  rejection_region_for_wald_test_simple_vs_composite <- qchisq(0.99,3)
  rejection_region_for_wald_test_pure_vs_composite <- qchisq(0.99,2)
  p_value_simple_vs_composite <- 1 - pchisq(full_analysis_results$hypothesis_results$simple_vs_compostie_llrt,3)
  p_value_pure_vs_composite <- 1 - pchisq(full_analysis_results$hypothesis_results$pure_type_vs_mixed_gender_em_results$wilks_statistic,2)
  p_value_list = list(p_value_simple_vs_composite = p_value_simple_vs_composite,
                      p_value_pure_vs_composite = p_value_pure_vs_composite)
  return(p_value_list)  
}

plotFullBrainFeature <- function(full_analysis_results) 
{
  full_analysis_results <- full_analysis_results[[1]]
  stopifnot(names(full_analysis_results) == c("feature_residuals","hypothesis_results"))
  plot_data <- full_analysis_results$feature_residuals
  em_data <- full_analysis_results$hypothesis_results$pure_type_vs_mixed_gender_em_results$alternative_hypothesis$m_parameters
  
  ggplot(plot_data, aes(x=trimmed_z_score, fill=factor(sex))) +
    geom_histogram(aes(y=..density..),
                   bins = 150, 
                   alpha=.8, 
                   position="identity") +
    stat_function(geom = "line", 
                  fun = plot_mix_comps,
                  args = list(mu = em_data$mu_1, sigma = sqrt(em_data$sigma_2_men), lambda = em_data$p),
                  colour = "blue", lwd = 1.2) + 
    stat_function(geom = "line", 
                  fun = plot_mix_comps,
                  args = list(mu = em_data$mu_2, sigma = sqrt(em_data$sigma_2_women) , lambda = 1 - em_data$p),
                  colour = "blue", lwd = 1.2,linetype = "dashed") + 
    stat_function(geom = "line", 
                  fun = plot_mix_comps,
                  args = list(mu = em_data$mu_1, sigma = sqrt(em_data$sigma_2_men), lambda = em_data$q),
                  colour = "red", lwd = 1.0) + 
    stat_function(geom = "line", 
                  fun = plot_mix_comps,
                  args = list(mu = em_data$mu_2, sigma = sqrt(em_data$sigma_2_women) , lambda = 1 - em_data$q),
                  colour = "red", lwd = 1.0,linetype = "dashed") + 
    ggtitle(paste0("Histogram of gender data"))
}

findInterestingRegion <- function(list_of_features, k = 5)
{
  if(length(list_of_features) <= 1)
  {
    return(list_of_features[[1]])
  }
  means_distance <- sapply(list_of_features,
                           function(x){x$hypothesis_results$pure_type_vs_mixed_gender_em_results$alternative_hypothesis$m_parameters$mu_1 - 
                               x$hypothesis_results$pure_type_vs_mixed_gender_em_results$alternative_hypothesis$m_parameters$mu_2})
  return(sort(means_distance, decreasing = T)[1:k])
}
