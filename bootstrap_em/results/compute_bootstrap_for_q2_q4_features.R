load('~/Desktop/biobank_redis_for_em.RData')
load('~/Desktop/biobank_feature_residual_analysis.RData')
library(beepr)
feature_name <- 'Mean FA in retrolenticular part of internal capsule on FA skeleton (left)'
bootstrap_results <- biobank_bootstrap_em(biobank_residuals_data[[feature_name]],
                     biobank_feature_residual_analysis[[feature_name]]$hypothesis_results$mixture_model$m_parameters)
beep('mario')
plot(bootstrap_results$bootstrap_results[,1], main ='scatter of bootstrap', ylab = 'p_hat')
points(biobank_feature_residual_analysis[[feature_name]]$hypothesis_results$mixture_model$m_parameters$p, col = 'red')
hist(bootstrap_results$bootstrap_results[,1])
summary(bootstrap_results$bootstrap_results[,1])
biobank_feature_residual_analysis[[feature_name]]$hypothesis_results$mixture_model$m_parameters$p
sd(bootstrap_results$bootstrap_results[,1])

plot(bootstrap_results$bootstrap_results[,2], main ='scatter of bootstrap', ylab = 'q_hat')
points(biobank_feature_residual_analysis[[feature_name]]$hypothesis_results$mixture_model$m_parameters$q, col = 'red')
hist(bootstrap_results$bootstrap_results[,2])
summary(bootstrap_results$bootstrap_results[,2])
biobank_feature_residual_analysis[[feature_name]]$hypothesis_results$mixture_model$m_parameters$q
sd(bootstrap_results$bootstrap_results[,2])

plot(bootstrap_results$bootstrap_results[,3], main ='scatter of bootstrap', ylab = 'mu_1_hat')
points(biobank_feature_residual_analysis[[feature_name]]$hypothesis_results$mixture_model$m_parameters$mu_1, col = 'red')
hist(bootstrap_results$bootstrap_results[,3])
summary(bootstrap_results$bootstrap_results[,3])
biobank_feature_residual_analysis[[feature_name]]$hypothesis_results$mixture_model$m_parameters$mu_1
sd(bootstrap_results$bootstrap_results[,3])

plot(bootstrap_results$bootstrap_results[,4], main ='scatter of bootstrap', ylab = 'mu_2_hat')
points(biobank_feature_residual_analysis[[feature_name]]$hypothesis_results$mixture_model$m_parameters$mu_2, col = 'red')
hist(bootstrap_results$bootstrap_results[,4])
summary(bootstrap_results$bootstrap_results[,4])
biobank_feature_residual_analysis[[feature_name]]$hypothesis_results$mixture_model$m_parameters$mu_2
sd(bootstrap_results$bootstrap_results[,4])


hist(bootstrap_results$bootstrap_results[,5])
biobank_feature_residual_analysis[[feature_name]]$hypothesis_results$mixture_model$m_parameters$sigma_2_men
summary(bootstrap_results$bootstrap_results[,5])
sd(bootstrap_results$bootstrap_results[,5])

hist(bootstrap_results$bootstrap_results[,6])
biobank_feature_residual_analysis[[feature_name]]$hypothesis_results$mixture_model$m_parameters$sigma_2_women
summary(bootstrap_results$bootstrap_results[,6])
sd(bootstrap_results$bootstrap_results[,6])

# p_delta_ratio = p_progress / lag(p_progress, 1)
# q_delta_ratio = q_progress / lag(q_progress, 1)
# mu_1_delta_ratio = mu_1_progress / lag(mu_1_progress, 1)
# mu_2_delta_ratio = mu_2_progress / lag(mu_2_progress, 1)
# progress_ratio = data.frame(p_delta_ratio = p_delta_ratio,
#                             p_delta_ratio = p_delta_ratio,
#                             mu_1_delta_ratio = mu_1_delta_ratio,
#                             mu_2_delta_ratio = mu_2_delta_ratio)