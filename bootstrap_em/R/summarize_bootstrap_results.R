summarizeBootstrapResults <- function(feature_name, bootstrap_results){
  mainDir = "~/Human_brain_research/bootstrap_em/results"
  dir.create(file.path(mainDir, feature_name), showWarnings = FALSE)
  setwd(file.path(mainDir, feature_name))
  parameters_list = c("p","q","mu_1","mu_2","sigma_2_men","sigma_2_women")
  hist(bootstrap_results$bootstrap_results[,i], main = parameter, breaks = 20, xlim = c(0,1))
  for(i in 1:length(parameters_list)){
    parameter = parameters_list[i]
    jpeg(sprintf("%s_hat_plot.jpg",parameter), width = 350, height = 350)
    plot(bootstrap_results$bootstrap_results[,i], main =sprintf('scatter of %s bootstrap',parameter))
    points(biobank_feature_residual_analysis[[feature_name]]$hypothesis_results$mixture_model$m_parameters[[parameter]], col = 'red')
    dev.off()
    jpeg(sprintf("%s_hat_hist.jpg",parameter), width = 350, height = 350)
    hist(bootstrap_results$bootstrap_results[,i], main = parameter, breaks = 20, xlim = c(0,1))
    abline(v = biobank_feature_residual_analysis[[feature_name]]$hypothesis_results$mixture_model$m_parameters[[parameter]], col = 'red')
    dev.off()
  }
  save(bootstrap_results, file = sprintf("%s.RData",feature_name))
}