library(ggplot2)

plot_mix_comps <- function(x, mu, sigma, lambda) {
  lambda * dnorm(x, mu, sigma)
}


plotGenderHistogram <- function(plot_data,
                                em_results,
                                feature_name = NULL,
                                p = NULL, q = NULL)
{
  em_data <- em_results$hypothesis_results$mixture_model$m_parameters
  
  plot_title <- sprintf("Histogram of gender data, feature %s", feature_name, p ,q)
  subtitle <- sprintf("p=%s, q=%s", p ,q)
  feature_histogram <- ggplot(plot_data, aes(x=value, fill=factor(sex))) +
    geom_histogram(aes(y=..density..),
                   bins = 100, 
                   alpha=.8, 
                   position="identity") + 
    ggtitle(plot_title, subtitle = subtitle) 
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

