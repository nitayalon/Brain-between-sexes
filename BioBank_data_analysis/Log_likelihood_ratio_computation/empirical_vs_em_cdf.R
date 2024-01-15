library(dplyr)
library(ggplot2)
library(KSgeneral)

theoretical_cdf = function(em_parameters, 
                           pure_type_parameters,
                           bins){
	p	= em_parameters$p
        q	= em_parameters$q
        mu_1	= em_parameters$mu_1
	mu_2	= em_parameters$mu_2
	sigma_1	= em_parameters$sigma_2_men
	sigma_2	= em_parameters$sigma_2_women
	men_pure_type_density = c()
	women_pure_type_density = c()
	men_mixture_model_density = c()	
  women_mixture_model_density = c()
  mu_men = pure_type_parameters$men_mean
  mu_women = pure_type_parameters$women_mean
  sigma_2_men = pure_type_parameters$sigma_2_men
  sigma_2_women = pure_type_parameters$sigma_2_women
	for(i in 1:(length(bins) - 1)){
    men_pure_type_density[i] = pnorm(bins[i+1], mu_men, sqrt(sigma_2_men))
    women_pure_type_density[i] = pnorm(bins[i+1], mu_women, sqrt(sigma_2_women))
    men_mixture_model_density[i] = (p * pnorm(bins[i+1], mu_1, sqrt(sigma_1)) + (1-p) * pnorm(bins[i+1],mu_2,sqrt(sigma_2))) -
(p * pnorm(bins[i], mu_1, sqrt(sigma_1)) + (1-p) * pnorm(bins[i],mu_2,sqrt(sigma_2)))
	  women_mixture_model_density[i] = (q * pnorm(bins[i+1], mu_1, sqrt(sigma_1)) + (1-q) * pnorm(bins[i+1],mu_2,sqrt(sigma_2))) -
(q * pnorm(bins[i], mu_1, sqrt(sigma_1)) + (1-q) * pnorm(bins[i],mu_2,sqrt(sigma_2)))
}
	return(list(
	  men_pure_type_density = men_pure_type_density,
	  women_pure_type_density = women_pure_type_density,
	  women_mixture_model_density = women_mixture_model_density,
    men_mixture_model_density = men_mixture_model_density))
}

hist_range = seq(-7,7, 0.5)
features_for_export_new = names(data_for_analysis)
ks_results = c()

for(x in features_for_export_new){
  em_parameters_for_llk_analysis = data_for_analysis[[x]]$hypothesis_results$mixture_model$m_parameters
  pure_type_parameters_for_llk_analysis = data_for_analysis[[x]]$hypothesis_results$pure_types_model
  feature_theoretical_cdf = theoretical_cdf(em_parameters_for_llk_analysis, pure_type_parameters_for_llk_analysis, hist_range)
  N = data_for_analysis[[x]]$feature_residuals %>% group_by(sex) %>% summarize(count = n())
  empirical_men_cdf = cumsum(hist((data_for_analysis[[x]]$feature_residuals %>% filter(sex == 1, !high_outlier, !low_outlier) %>% select(value))$value, plot = F, breaks = hist_range)$density) / 2
  empirical_women_cdf = cumsum(hist((data_for_analysis[[x]]$feature_residuals %>% filter(sex == 0, !high_outlier, !low_outlier) %>% select(value))$value, plot = F, breaks = hist_range)$density) / 2
  ks_mixture_model_men = max(abs(empirical_men_cdf - cumsum(feature_theoretical_cdf$men_mixture_model_density)) * sqrt(N[2,2])$count)
  ks_mixture_model_women = max(abs(empirical_women_cdf - cumsum(feature_theoretical_cdf$women_mixture_model_density)) * sqrt(N[1,2])$count)
  ks_pure_model_men = max(abs(empirical_men_cdf - feature_theoretical_cdf$men_pure_type_density) * sqrt(N[2,2])$count)
  ks_pure_model_women = max(abs(empirical_women_cdf - feature_theoretical_cdf$women_pure_type_density) * sqrt(N[1,2])$count)
  ks_results = rbind(ks_results, c(ks_mixture_model_men, ks_mixture_model_women, 
                                   ks_pure_model_men, ks_pure_model_women))
}

colnames(ks_results) = c('men_mixture_model_ks', 'women_mixture_model_ks',
                         'men_pure_model_ks', 'women_pure_model_ks')
ks_results = as_tibble(ks_results)
ks_results$Feature = features_for_export_new
ks_results$p = sapply(data_for_analysis, function(x)x$hypothesis_results$mixture_model$m_parameters$p)
ks_results$q = sapply(data_for_analysis, function(x)x$hypothesis_results$mixture_model$m_parameters$q)
ks_results$mu_1 = sapply(data_for_analysis, function(x)x$hypothesis_results$mixture_model$m_parameters$mu_1)
ks_results$mu_2 = sapply(data_for_analysis, function(x)x$hypothesis_results$mixture_model$m_parameters$mu_2)
write.csv(ks_results, 'Human_brain_research/BioBank_data_analysis/Log_likelihood_ratio_computation/ks_results.csv')

ks_results = read.csv('~/BioBank_data_analysis/Log_likelihood_ratio_computation/ks_results.csv')
plot(ecdf(ks_results$KS_men), col = 'blue')
points(ecdf(ks_results$KS_women), col = 'red')
summary(ks_results$KS_men)
sd(ks_results$KS_men)
sum(ks_results$KS_men > 1.4)

hist(ks_results$KS_women, breaks = 50)
summary(ks_results$KS_women)
sd(ks_results$KS_women)
sum(ks_results$KS_women > 1.4)

View(ks_results %>% filter())

ks_max_feature = 'Volume of grey matter in Caudate (left)'
plotGenderHistogram(biobank_residuals_data[[ks_max_feature]],
                    data_for_analysis[[ks_max_feature]],
                    ks_max_feature)
data_for_analysis[[ks_max_feature]]$hypothesis_results$mixture_model$m_parameters

plot(ecdf((table_for_isaco %>% 
             filter(sex == 1) %>%
             select(value.x))[,1]))
points(seq(-4,4,0.5)[-1], cumsum(men_women_em_cdf$men_mixture_model_density), type = 'b', col = 'red')

men_women_em_cdf = theoretical_cdf(data_for_analysis[[features_for_export_new[2]]]$hypothesis_results$mixture_model$m_parameters)
plot(ecdf((table_for_isaco %>% 
filter(sex == 1) %>%
select(value.y))[,1]))
points(seq(-4,4,0.5)[-1], cumsum(men_women_em_cdf$men_mixture_model_density), type = 'b', col = 'red')

plot(ecdf((table_for_isaco %>% 
filter(sex == 0) %>%
select(value.y))[,1]))
points(seq(-4,4,0.5)[-1], cumsum(men_women_em_cdf$women_mixture_model_density), type = 'b', col = 'red')


#' 4000 points between 0,4. Compute the cumsum (x > q)
#' For (1/289. 1) compute KS
ks_distribution = sapply(seq(1/289,1,1/289), function(x) {cont_ks_c_cdf(x, 289)})
ks_range = seq(0,4,length.out = 289)
men_survival = sapply(1:289, function(i) {sum(ks_results$KS_men > ks_range[i])}) / 289
women_survival = sapply(1:289, function(i) {sum(ks_results$KS_women > ks_range[i])}) / 289

plot(ks_range, men_survival, type = 'l', col = 'blue', lwd = 2.5, ylab = 'CDF', xlab = 'KS statistic', cex.lab=1.5, cex.axis = 1.5)
lines(ks_range, women_survival, type = 'l', col = 'red', lwd = 2.5)
lines(seq(1/289,1,1/289) * sqrt(289), ks_distribution, type = 'l', col = 'black', lwd = 2.5)
legend(2.8, y=0.85, legend=c("Men", "Women", 'KS'),
       col=c("blue", "red", "black"), lty=1, cex=0.8)

ks_results %>% 
  mutate(prop_dist = abs(p - q), 
         ks_men_size = (KS_men < 1.3) + 1,
         ks_women_size = (KS_women < 1.3) + 1,
         p_eq_q = Feature %in% q1_q3_features_names * 2 + 1) %>% 
  select(prop_dist, KS_men, KS_women, p_eq_q) %>% 
  ggplot(.,aes(prop_dist)) + 
  geom_point(aes(prop_dist, KS_women, colour = 'Women', shape=p_eq_q)) +
  geom_point(aes(prop_dist, KS_men, colour = 'Men', shape=p_eq_q)) + 
  scale_shape_identity() + 
  theme(axis.text = element_text(size = 20)) + theme(axis.title = element_text(size = 20))
plot_range = c(-3.0,3.0)
plot(ks_results$mu_1, ks_results$mu_2, col = ifelse(ks_results$KS_men > 2 | ks_results$KS_women > 2, 'red', 'green'),
     cex.lab=1.5, cex.axis = 1.5, xlab = 'Mu1', ylab = 'Mu2', xlim = plot_range, ylim = plot_range)
abline(h = c(-1,0.82), col = 'black')
abline(v = c(-0.71,0.7), col = 'black')

ks_results_low_mean = ks_results %>% 
  filter(mu_1 > -1 & mu_1 < 0.8 & mu_2 > -0.71 & mu_2 < 0.7)

ks_results_high_mean = ks_results %>% 
  filter(mu_1 < -1 | mu_1 > 0.8 | mu_2 < -0.71 | mu_2 > 0.7)

ks_distribution = sapply(seq(1/289,1,1/289), function(x) {cont_ks_c_cdf(x, 289)})
ks_range = seq(0,4,length.out = 289)
men_survival = sapply(1:289, function(i) {sum(ks_results_low_mean$KS_men > ks_range[i])}) / nrow(ks_results_low_mean)
women_survival = sapply(1:289, function(i) {sum(ks_results_low_mean$KS_women > ks_range[i])}) / nrow(ks_results_low_mean)

plot(ks_range, men_survival, type = 'l', col = 'blue', lwd = 2.5, ylab = 'CDF', xlab = 'KS statistic', cex.lab=1.5, cex.axis = 1.5)
lines(ks_range, women_survival, type = 'l', col = 'red', lwd = 2.5)
lines(seq(1/289,1,1/289) * sqrt(289), ks_distribution, type = 'l', col = 'black', lwd = 2.5)
legend(2.8, y=0.85, legend=c("Men", "Women", 'KS'),
       col=c("blue", "red", "black"), lty=1, cex=0.8)

plot(ks_results$p - ks_results$q, ks_results$KS_men, type = 'p', cex = 0.9, col = 'blue', pch = 3, cex.lab=1.5, cex.axis = 1.5,
     ylab = 'KS statistic', xlab = 'p - q')
points(ks_results$p - ks_results$q, ks_results$KS_women, cex = 0.9, col = 'red', pch = 4)
abline(h = 1.358, col = 'black')

ks_results %>% 
  filter(Feature %in% q1_q3_features_names) %>% 
  select(KS_men, KS_women)


# adding cdf plots of the heighest KS feature -----------------------------

max_ks_feature = ks_results$Feature[which.max(ks_results$KS_men)]

sort(ks_results$KS_men,decreasing = T)[1:20]
sort(ks_results$KS_women,decreasing = T)[1:10]

tenth_ks_feature = (ks_results$Feature[order(ks_results$KS_men,decreasing = T)])[10]

em_parameters_for_llk_analysis = data_for_analysis[[max_ks_feature]]$hypothesis_results$mixture_model$m_parameters
feature_theoretical_cdf = theoretical_cdf(em_parameters_for_llk_analysis)
N = biobank_residuals_data[[max_ks_feature]] %>% group_by(sex) %>% summarize(count = n())
empirical_men_cdf = cumsum(hist((biobank_residuals_data[[max_ks_feature]] %>% filter(sex == 1) %>% select(value))$value, plot = F, breaks = seq(-7,7,0.05))$density) / 20
empirical_women_cdf = cumsum(hist((biobank_residuals_data[[max_ks_feature]] %>% filter(sex == 0) %>% select(value))$value, plot = F, breaks = seq(-7,7,0.05))$density) / 20

plot(seq(-7,7,0.05)[-1], cumsum(feature_theoretical_cdf$men_mixture_model_density), col = 'blue', type = 'l', lwd = 1.5,
     ylab = 'CDF', xlab = 'X',cex.lab=1.5, cex.axis = 1.5,)
lines(seq(-7,7,0.05)[-1], cumsum(feature_theoretical_cdf$women_mixture_model_density), col = 'red', type = 'l', lwd = 1.5)
lines(seq(-7,7,0.05)[-1], empirical_men_cdf, col = 'blue', type = 'l',lty=2, lwd = 1.5)
lines(seq(-7,7,0.05)[-1], empirical_women_cdf, col = 'red', type = 'l', lty=2, lwd = 1.5)
legend(-5, 0.8, legend=c("Men_th", "Women_th","Men_em", "Women_em"),
       col=c("blue", "red"), lty=c(1,1,2,2), cex=0.8)

empirical_men_pdf = hist((biobank_residuals_data[[max_ks_feature]] %>% filter(sex == 1) %>% select(value))$value, plot = F, breaks = seq(-7,7,0.05))
empirical_women_pdf = hist((biobank_residuals_data[[max_ks_feature]] %>% filter(sex == 0) %>% select(value))$value, plot = F, breaks = seq(-7,7,0.05))

plot(empirical_men_pdf, col = 'blue', ylim = c(0,300))
plot(empirical_women_pdf, col = 'red', add = T)
lines(seq(-7,7,0.05)[-1], feature_theoretical_cdf$men_mixture_model_density, col = 'blue', type = 'l', lwd = 1.5,
     ylab = 'CDF', xlab = 'X',cex.lab=1.5, cex.axis = 1.5,)
lines(seq(-7,7,0.05)[-1], feature_theoretical_cdf$women_mixture_model_density, col = 'red', type = 'l', lwd = 1.5)
legend(-5, 0.8, legend=c("Men_th", "Women_th","Men_em", "Women_em"),
       col=c("blue", "red"), lty=c(1,1,2,2), cex=0.8)


biobank_residuals_data[[max_ks_feature]] %>% group_by(sex) %>% summarize(avg = mean(value))
plot(empirical_men_cdf - empirical_women_cdf)
plotGenderHistogram(biobank_residuals_data[[max_ks_feature]],
                    data_for_analysis[[max_ks_feature]], max_ks_feature)
plotGenderHistogram(biobank_residuals_data[[tenth_ks_feature]],
                    data_for_analysis[[tenth_ks_feature]], tenth_ks_feature)


# Plot p-q vs KS ----------------------------------------------------------

ks_results %>% 
  mutate(high_mean_p = ifelse(mu_1 < 0,1-p,p),
         high_mean_q = ifelse(mu_1 < 0,1-q,q),
         KS_men_bins = cut(KS_men, c(0,2,5))) %>% 
ggplot(., aes(high_mean_p,high_mean_q,col = KS_men_bins)) +
  geom_point()

ks_results_extreme_squares = ks_results %>% 
  mutate(high_mean_p = ifelse(mu_1 < 0,1-p,p),
  high_mean_q = ifelse(mu_1 < 0,1-q,q),
         KS_men_bins = cut(KS_men, c(0,2,5))) %>% 
  filter(high_mean_p > 0.75 & high_mean_q > 0.75 | high_mean_p < 0.25 & high_mean_q < 0.25) 

ks_results_non_extreme_squares = ks_results %>% 
  filter(!(Feature %in% ks_results_extreme_squares$Feature))

non_extereme_sample = c(rep(ks_results_non_extreme_squares$KS_men,201),rep(ks_results_non_extreme_squares$KS_women,201))
extereme_sample = c(rep(ks_results_extreme_squares$KS_men,88),rep(ks_results_extreme_squares$KS_women,88))

ks_range = seq(0,4,length.out = 400)
non_extrem__survival = sapply(1:400, function(i) {sum(non_extereme_sample > ks_range[i])}) / 80802
extrem__survival = sapply(1:400, function(i) {sum(extereme_sample > ks_range[i])}) / 15488
ks_distribution = sapply(seq(1/400,1,1/400), function(x) {cont_ks_c_cdf(x, 400)})

plot(ks_range, non_extrem__survival, type = 'l', col = 'blue', lwd = 2.5, ylab = 'CDF', xlab = 'KS statistic', cex.lab=1.5, cex.axis = 1.5)
lines(ks_range, extrem__survival, type = 'l', col = 'red', lwd = 2.5)
lines(seq(1/289,1,1/289) * sqrt(289), ks_distribution, type = 'l', col = 'black', lwd = 2.5)
lines(seq(1/400,1,1/400) * sqrt(400), ks_distribution, type = 'l', col = 'black', lwd = 2.5)


# Adding extra analysis for Science paper ---------------------------------

length(intersect(ks_results_extreme_squares$Feature,
                 names(pure_type_features_residuals)))
ks_results_extreme_squares %>% 
  filter(Feature %in% q1_q3_features$feature) %>% 
  select(high_mean_p, high_mean_q) %>% 
  filter(high_mean_p > 0.75 & high_mean_q > 0.75) %>% 
  filter(high_mean_p < high_mean_q)
ks_results_extreme_squares %>% 
  filter(Feature %in% q1_q3_features$feature) %>% 
  select(high_mean_p, high_mean_q) %>% 
  filter(high_mean_p < 0.25 & high_mean_q < 0.25) %>% 
  filter(high_mean_p > high_mean_q)
