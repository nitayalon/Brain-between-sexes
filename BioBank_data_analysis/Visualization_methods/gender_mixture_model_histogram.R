library(ggplot2)
library(RColorBrewer)
library(dplyr)

plot_mix_comps <- function(x, mu, sigma, lambda) {
  lambda * dnorm(x, mu, sigma)
}


plotGenderHistogram <- function(plot_data,
                                em_results,
                                feature_name = NULL,
                                p = NULL, q = NULL,
                                two_mixtures = TRUE,
                                single_gaussian = F)
{
  em_data <- em_results$hypothesis_results$mixture_model$m_parameters
  myColors <- brewer.pal(5,"Set1")
  names(myColors) <- levels(plot_data$sex)
  colScale <- scale_colour_manual(name = "sex",values = myColors)
  key_statistics = plot_data %>% group_by(sex) %>% summarize(avg = mean(value), std = sd(value))
  plot_title <- sprintf("Histogram of gender data, feature %s", feature_name, p ,q)
  subtitle <- sprintf("p=%s, q=%s", p ,q)
  feature_histogram <- ggplot(plot_data, aes(x=value, fill=factor(sex))) +
    geom_histogram(aes(y=..density..),
                   bins = 100, 
                   alpha=.8, 
                   position="identity") + 
    scale_fill_manual(values = alpha(c('tomato','dodgerblue'),.1)) + 
    ggtitle(plot_title, subtitle = subtitle) 
  if(two_mixtures)
  {
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
  if(single_gaussian)
  {
    feature_histogram <- feature_histogram + 
      stat_function(geom = "line",
                    fun = plot_mix_comps,
                    args = list(mu = key_statistics$avg[2], sigma = sqrt(key_statistics$std[2]), lambda = 1),
                    colour = "blue", lwd = 1.2) +
      stat_function(geom = "line",
                    fun = plot_mix_comps,
                    args = list(mu = key_statistics$avg[1], sigma = sqrt(key_statistics$std[1]), lambda = 1),
                    colour = "red", lwd = 1.0,linetype = "dashed") 
    return(feature_histogram)
  }
  feature_histogram <- feature_histogram + 
    stat_function(geom = "line",
                  fun = plot_mix_comps,
                  args = list(mu = em_data$mu_1, sigma = sqrt(em_data$sigma_2_men), lambda = em_data$p),
                  colour = "blue", lwd = 1.2) +
    stat_function(geom = "line",
                  fun = plot_mix_comps,
                  args = list(mu = em_data$mu_2, sigma = sqrt(em_data$sigma_2_women) , lambda = (1 - em_data$q)),
                  colour = "red", lwd = 1.0,linetype = "dashed") 
  return(feature_histogram)
  
}

