library(ggplot2)

plot_mix_comps <- function(x, mu, sigma, lambda) {
  lambda * dnorm(x, mu, sigma)
}


plotGenderHistogram <- function(feature_data, feature_name = NULL)
{
  plot_data <- feature_data$feature_residuals
  em_data <- feature_data$hypothesis_results$pure_type_vs_mixed_gender_em_results$alternative_hypothesis$m_parameters
  
  plot_title <- sprintf("Histogram of gender data, feature %s", feature_name)
  
  feature_histogram <- ggplot(plot_data, aes(x=value, fill=factor(sex))) +
    geom_histogram(aes(y=..density..),
                   bins = 100, 
                   alpha=.8, 
                   position="identity") + 
    ggtitle(plot_title) 
  # +
  #   geom_vline(xintercept = c(em_data$mu_1, em_data$mu_2))
   
  feature_histogram <- feature_histogram + 
      stat_function(geom = "line",
                    fun = plot_mix_comps,
                    args = list(mu = em_data$mu_1, sigma = sqrt(em_data$sigma_2_men), lambda = em_data$p),
                    colour = "blue", lwd = 1.2) +
      stat_function(geom = "line",
                    fun = plot_mix_comps,
                    args = list(mu = em_data$mu_2, sigma = sqrt(em_data$sigma_2_women) , lambda = (1 - em_data$p)),
                    colour = "blue", lwd = 1.2,linetype = "dashed") +
      stat_function(geom = "line",
                    fun = plot_mix_comps,
                    args = list(mu = em_data$mu_1, sigma = sqrt(em_data$sigma_2_men), lambda = em_data$q),
                    colour = "red", lwd = 1.0) +
      stat_function(geom = "line",
                    fun = plot_mix_comps,
                    args = list(mu = em_data$mu_2, sigma = sqrt(em_data$sigma_2_women) , lambda = (1 - em_data$q)),
                    colour = "red", lwd = 1.0,linetype = "dashed") 
  return(feature_histogram)
}

