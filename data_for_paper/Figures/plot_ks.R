library(tools)
windowsFonts(A = windowsFont("Arial Black"))
# plot(ks_range, non_extrem__survival, 
#      type = 'l', col = '#A9A9A9', lwd = 2.5, ylab = 'CDF', xlab = 'KS statistic', 
#      cex.lab= 1.5, cex.axis = 1.5, 
#      lty = 5,
#      family="A",
#      font=2,
#      ps = 24,
#      frame.plot = FALSE)
# lines(ks_range, extrem__survival, 
#       type = 'l', col = '#A9A9A9', lwd = 2.5, lty = 3)
# lines(seq(1/400,1,1/400) * sqrt(400), ks_distribution, type = 'l', col = 'black', lwd = 2.5)
# lines(seq(1/dimension,1,1/dimension) * sqrt(dimension), ks_distribution, type = 'l', col = 'black', lwd = 2.5)

create_ks_plot <- function(ks_table, p_value, q_value, condition)
{
  if( condition == "outside")
  {
    filtered_ks_results = ks_table %>% filter(p > p_value & q > q_value |
                                       p < 1 - p_value & q < 1 - q_value)
  }
  else
  {
    filtered_ks_results = ks_table %>% filter(p < p_value & q < q_value |
                                                p > 1 - p_value & q > 1 - q_value)
  }
  pure_type_data = c(filtered_ks_results$men_pure_model_ks, 
                     filtered_ks_results$women_pure_model_ks)
  mixture_data = c(filtered_ks_results$men_mixture_model_ks, 
                     filtered_ks_results$women_mixture_model_ks)
  dimension = length(mixture_data)
  ks_range = seq(0, 15, length.out = dimension)
  ks_distribution = sapply(seq(1/dimension,1,1/dimension), function(x) {cont_ks_c_cdf(x, dimension)})
  pure_type_survival = sapply(1:dimension, function(i) {sum(pure_type_data > ks_range[i])}) / dimension
  mixture_model_survival = sapply(1:dimension, function(i) {sum(mixture_data > ks_range[i])}) / dimension
  # pure_men_survival = sapply(1:dimension, function(i) {sum(filtered_ks_results$men_pure_model_ks > ks_range[i])}) / dimension
  # pure_women_survival = sapply(1:dimension, function(i) {sum(filtered_ks_results$women_pure_model_ks > ks_range[i])}) / dimension
  # mixture_model_men_survival = sapply(1:dimension, function(i) {sum(filtered_ks_results$men_mixture_model_ks > ks_range[i])}) / dimension
  # mixture_model_women_survival = sapply(1:dimension, function(i) {sum(filtered_ks_results$women_mixture_model_ks > ks_range[i])}) / dimension
  
  plot(ks_range, pure_type_survival, type = 'l', col = 'black', lwd = 2.5, 
       ylab = 'CDF', xlab = 'KS statistic', cex.lab=1.5, cex.axis = 1.5,
       main=toTitleCase(condition),ylim = c(0,1), lty = 3)
  lines(ks_range, mixture_model_survival, type = 'l', col = 'black', lwd = 2.5,
        lty = 2)
  lines(seq(1/dimension,1,1/dimension) * sqrt(dimension), ks_distribution, type = 'l', col = 'black', lwd = 2.5,
        lty = 1)
  legend(x=12, y=0.85, legend=c("Pure type", "Mixture model", 'Theoretical KS'),
         lty=c(3,2,1), cex=0.8)
  
  
  # plot(ks_range, pure_men_survival, type = 'l', col = 'blue', lwd = 2.5, ylab = 'CDF', xlab = 'KS statistic', cex.lab=1.5, cex.axis = 1.5,
  #      main=paste("Men", toTitleCase(condition), sep = " "),ylim = c(0,1))
  # lines(ks_range, mixture_model_men_survival, type = 'l', col = 'red', lwd = 2.5)
  # lines(seq(1/dimension,1,1/dimension) * sqrt(dimension), ks_distribution, type = 'l', col = 'black', lwd = 2.5)
  # legend(x=12, y=0.85, legend=c("Pure type", "Mixture model", 'Theoretical KS'),
  #        col=c("blue", "red", "black"), lty=1, cex=0.8)
  # 
  # plot(ks_range, pure_women_survival, type = 'l', col = 'blue', lwd = 2.5, ylab = 'CDF', xlab = 'KS statistic', cex.lab=1.5, cex.axis = 1.5,
  #      ylim = c(0,1), main = paste("Women", toTitleCase(condition), sep = " "))
  # lines(ks_range, mixture_model_women_survival, type = 'l', col = 'red', lwd = 2.5)
  # lines(seq(1/dimension,1,1/dimension) * sqrt(dimension), ks_distribution, type = 'l', col = 'black', lwd = 2.5)
  # legend(x=12, y=0.85, legend=c("Pure type", "Mixture model", 'Theoretical KS'),
  #        col=c("blue", "red", "black"), lty=1, cex=0.8)
  
  
}