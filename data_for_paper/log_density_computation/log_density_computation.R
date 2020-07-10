library(dplyr)
pure_type_feature = 'Volume of grey matter in Postcentral Gyrus (left)'
mixed_typed_type_feature = 'Volume of grey matter in Lateral Occipital Cortex, superior division (left)'

mixed_women = biobank_residuals_data[[mixed_typed_type_feature]] %>% filter(sex == 0) %>% select(value)
mixed_men = biobank_residuals_data[[mixed_typed_type_feature]] %>% filter(sex == 1) %>% select(value)
pure_women = biobank_residuals_data[[pure_type_feature]] %>% filter(sex == 0) %>% select(value)
pure_men = biobank_residuals_data[[pure_type_feature]] %>% filter(sex == 1) %>% select(value)

## Now the theoretical
# Compute the theoretical probabilities
theoretical_density = function(em_parameters, bins = seq(-4,4,0.5)){
	p	= em_parameters$p
        q	= em_parameters$q
        mu_1	= em_parameters$mu_1
	mu_2	= em_parameters$mu_2
	sigma_1	= em_parameters$sigma_2_men
	sigma_2	= em_parameters$sigma_2_women
	men_density = c()	
        women_density = c()
	for(i in 1:(length(bins) - 1)){
	men_density[i] = (p * pnorm(bins[i+1], mu_1, sqrt(sigma_1)) + (1-p) * pnorm(bins[i+1],mu_2,sqrt(sigma_2))) -
(p * pnorm(bins[i], mu_1, sqrt(sigma_1)) + (1-p) * pnorm(bins[i],mu_2,sqrt(sigma_2)))
	women_density[i] = (q * pnorm(bins[i+1], mu_1, sqrt(sigma_1)) + (1-q) * pnorm(bins[i+1],mu_2,sqrt(sigma_2))) -
(q * pnorm(bins[i], mu_1, sqrt(sigma_1)) + (1-q) * pnorm(bins[i],mu_2,sqrt(sigma_2)))
}

	#return(men_density / women_density)
	return(list(women_density = women_density,
		    men_density = men_density))
}

mixed_men_hist = hist(mixed_men$value,plot = F, breaks = seq(-4,4,0.5))
mixed_women_hist = hist(mixed_women$value,plot = F, breaks = seq(-4,4,0.5))
pure_men_hist = hist(pure_men$value,plot = F, breaks = seq(-4,4,0.5))
pure_women_hist = hist(pure_women$value,plot = F, breaks = seq(-4,4,0.5))
# Convert to proportion
# Sum 3 extreme bins
# Play with the location to make the pure types as linear as possible 
pure_men_hist$counts / sum(pure_men_hist$counts)
pure_women_hist$counts / sum(pure_women_hist$counts)
mixed_men_hist$counts
mixed_women_hist$counts
n = length(pure_men_hist$counts)

men_bins = c(sum(pure_men_hist$counts[1:3]),pure_men_hist$counts[4:(n-3)], sum(pure_men_hist$counts[(n-2):n]))
women_bins = c(sum(pure_women_hist$counts[1:3]), pure_women_hist$counts[4:(n-3)], sum(pure_women_hist$counts[(n-2):n]))

mixed_men_bins = c(sum(mixed_men_hist$counts[1:3]),mixed_men_hist$counts[4:(n-3)], sum(mixed_men_hist$counts[(n-2):n]))
mixed_women_bins = c(sum(mixed_women_hist$counts[1:3]), mixed_women_hist$counts[4:(n-3)], sum(mixed_women_hist$counts[(n-2):n]))

y = log((women_bins[-c(1,12)] / sum(women_bins[-c(1,12)])) / (men_bins[-c(1,12)] / sum(men_bins[-c(1,12)])))
x = 1:length(y)
lm(x ~ y)
y_mixed = log((mixed_women_bins / sum(mixed_women_bins)) / (mixed_men_bins / sum(mixed_men_bins)))
plot(y, ylim = c(-3,3), col = 'red')
points(y_mixed, col = 'blue')
y_corrected = y[!is.na(y) & abs(y) < abs(Inf)]
lo <- loess(y~x)
plot(x,y, ylim = c(-1.5,1.5))
lines(predict(lo), col='red', lwd=2)
mixed_theoretical_hist = theoretical_density(biobank_feature_residual_analysis[[mixed_typed_type_feature]]$hypothesis_results$mixture_model$m_parameters)
pure_theoretical_hist = theoretical_density(biobank_feature_residual_analysis[[pure_type_feature]]$hypothesis_results$mixture_model$m_parameters)
plot(log(mixed_theoretical_hist$women_density / mixed_theoretical_hist$men_density), col = 'red', ylim = c(-1,1))
points(log(pure_theoretical_hist$women_density / pure_theoretical_hist$men_density), col = 'blue')

x_len = length(new_res$men_density)
plot(new_res$men_density, col = 'red', xlim = c(0, 10000))
points(new_res$women_density[1:x_len], col = 'blue')
log(new_res$men_density / new_res$women_density[1:x_len])


pure_men_hist = hist(pure_men$value,plot = F, breaks = seq(-4,4,0.5))
pure_women_hist = hist(pure_women$value,plot = F, breaks = seq(-4,4,0.5))
pure_y = log(pure_women_hist$counts / pure_men_hist$counts)

mixed_men_hist = hist(mixed_men$value,plot = F, breaks = seq(-4,4,0.5))
mixed_women_hist = hist(mixed_women$value,plot = F, breaks = seq(-4,4,0.5))
mixed_y = log(mixed_women_hist$counts / mixed_men_hist$counts)

plot(pure_men_hist$breaks[-1], pure_y, col = 'red', type = 'b', ylab = 'log density ratio', xlab = 'X', xlim = c(-4,4))
points(pure_men_hist$breaks[-1],mixed_y, col = 'blue', type = 'b')


# Pure types ratio --------------------------------------------------------
pure_types = c('Volume of grey matter in Postcentral Gyrus (left)',
               'Volume of grey matter in Planum Polare (right)', 
               'Volume of grey matter in Cingulate Gyrus, anterior division (right)')
for(i in 1:3){
  feature = pure_types[i]
  pure_women = biobank_residuals_data[[feature]] %>% filter(sex == 0) %>% select(value)
  pure_men = biobank_residuals_data[[feature]] %>% filter(sex == 1) %>% select(value)
  pure_men_hist = hist(pure_men$value,plot = F, breaks = seq(-4.2,4,0.5))
  pure_women_hist = hist(pure_women$value,plot = F, breaks = seq(-4.2,4,0.5))
  n = length(pure_men_hist$counts)
  men_bins = c(sum(pure_men_hist$counts[1:3]),pure_men_hist$counts[4:(n-3)], sum(pure_men_hist$counts[(n-2):n]))
  women_bins = c(sum(pure_women_hist$counts[1:3]), pure_women_hist$counts[4:(n-3)], sum(pure_women_hist$counts[(n-2):n]))
  if(i==1){
    y = log((women_bins[-c(1,12)] / sum(women_bins[-c(1,12)])) / (men_bins[-c(1,12)] / sum(men_bins[-c(1,12)])))
    x = 1:length(y)
    plot(x,y, col = i, ylim = c(-2,2))
  }
  else{
    points(log((women_bins[-c(1,12)] / sum(women_bins[-c(1,12)])) / (men_bins[-c(1,12)] / sum(men_bins[-c(1,12)]))), col = i)
  }
  
}

