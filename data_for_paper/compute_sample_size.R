library(mixtools)
library(ggplot2)
library(dplyr)

nullHypothesisIsTrueLlk <- function(sample_size){
  alternative_llk <- NULL
  counter = 0
  while(is.null(alternative_llk))
  {
    data <- rnorm(sample_size, 0,1)  
    x_bar = mean(data)
    s_2 = sum((data - mean(data))^2) / sample_size
    null_hypothesis_llk <- sum(dnorm(data,mean(data),sqrt(s_2),log = T))
    theoretical_llk = -sample_size/2 - sample_size/2*log(s_2) - 
      sample_size/2*log(2*pi)
    alternative_llk <- tryCatch(
      {normalmixEM(data, lambda = 0.9,
                                   maxit = 10000,
                                   arbmean = TRUE,
                                   arbvar = FALSE,
                                   fast = TRUE)},
                                warning=function(e){
                                  NULL
                                },
      error=function(e){
        NULL
      })
    counter = counter + 1
    if(counter %% 10 == 0){
      cat(sprintf('Iteration number %i',counter))
    }
    if(counter >= 100)
    {
      break
    }
  }
  return(list(null_hypothesis_llk = null_hypothesis_llk,
              alternative_llk = alternative_llk$loglik,
              em_params = list(p = alternative_llk$lambda,
                               location = alternative_llk$mu,
                               scale = alternative_llk$sigma),
              theoretical_llk = theoretical_llk,
              llrt = 2 * (alternative_llk$loglik - null_hypothesis_llk)))
}

alternativeHypothesisIsTrueLlk <- function(alternative_parameters, 
                                           sample_size){
  alternative_llk <- NULL
  ind <- runif(sample_size) > alternative_parameters$p
  low_distribution <- rnorm(sum(!ind), alternative_parameters$mu_2, sqrt(alternative_parameters$sigma_2_men))
  high_distribution <- rnorm(sum(ind), alternative_parameters$mu_1, sqrt(alternative_parameters$sigma_2_men))
  data <- c(low_distribution,high_distribution)
  null_hypothesis_llk <- sum(dnorm(data,mean(data),sd(data),log = T))
  alternative_llk <- tryCatch(
    {normalmixEM(data, lambda = alternative_parameters$p,
                                   maxit = 10000,
                                   arbmean = TRUE,
                                   arbvar = TRUE,
                                   fast = TRUE)},
                                warning=function(e){
                                  NULL
                                },
      error=function(e){
        NULL
      })
  return(list(null_hypothesis_llk = null_hypothesis_llk,
              alternative_llk = alternative_llk$loglik,
              em_params = list(p = alternative_llk$lambda,
                               location = alternative_llk$mu,
                               scale = alternative_llk$sigma),
              llrt = 2 * (alternative_llk$loglik - null_hypothesis_llk)))
}

n_reps <- 1000
h1_parameters <- 
  biobank_feature_standard_analysis[[two_mixture_model_volume_features[1]]]$hypothesis_results$mixture_model$m_parameters
llrt_1000 <- lapply(1:n_reps, function(x){nullHypothesisIsTrueLlk(1000)})
llrt_5000 <- lapply(1:n_reps, function(x){nullHypothesisIsTrueLlk(5000)})
llrt_8000 <- lapply(1:n_reps, function(x){nullHypothesisIsTrueLlk(8000)})
llrt_1000_llrt <- sapply(llrt_1000, function(x){max(0,x$llrt)})
llrt_5000_llrt <- sapply(llrt_5000, function(x){max(0,x$llrt)})
llrt_8000_llrt <- sapply(llrt_8000, function(x){max(0,x$llrt)})
llrt_1000_h1 <- lapply(1:n_reps, function(x){alternativeHypothesisIsTrueLlk(h1_parameters, 1000)})
llrt_5000_h1 <- lapply(1:n_reps, function(x){alternativeHypothesisIsTrueLlk(h1_parameters, 5000)})
llrt_8000_h1 <- lapply(1:n_reps, function(x){alternativeHypothesisIsTrueLlk(h1_parameters, 8000)})

h0_1000_samples_llrt <- data.frame(data = llrt_1000_llrt,
                                   sample_size = rep(1000,length(llrt_1000_llrt)), 
                                   hypothesis = rep('H0',length(llrt_1000_llrt)))
h0_5000_samples_llrt <- data.frame(data = llrt_5000_llrt,
                                   sample_size = rep(5000,length(llrt_5000_llrt)), 
                                   hypothesis = rep('H0',length(llrt_5000_llrt)))
h0_8000_samples_llrt <- data.frame(data = llrt_8000_llrt,
                                   sample_size = rep(8000,length(llrt_8000_llrt)), 
                                   hypothesis = rep('H0',length(llrt_8000_llrt)))

h1_1000_samples_llrt <- data.frame(data = llrt_1000_h1_llrt,
                                   sample_size = rep(1000,length(llrt_1000_h1_llrt)), 
                                   hypothesis = rep('H1',length(llrt_1000_h1_llrt)))
h1_5000_samples_llrt <- data.frame(data = llrt_5000_h1_llrt,
                                   sample_size = rep(5000,length(llrt_8000_h1_llrt)), 
                                   hypothesis = rep('H1',length(llrt_8000_h1_llrt)))
h1_8000_samples_llrt <- data.frame(data = llrt_8000_h1_llrt,
                                   sample_size = rep(8000,length(llrt_8000_h1_llrt)), 
                                   hypothesis = rep('H1',length(llrt_8000_h1_llrt)))
h0_and_h1_power_plot_data <- rbind(h0_1000_samples_llrt, h0_5000_samples_llrt, h0_8000_samples_llrt,
                                   h1_1000_samples_llrt, h1_5000_samples_llrt, h1_8000_samples_llrt)

h0_and_h1_power_plot_data$sample_size = factor(h0_and_h1_power_plot_data$sample_size)
h0_and_h1_power_plot_data_no_zeros <- h0_and_h1_power_plot_data %>% 
  filter(data > 0)
ggplot(h0_and_h1_power_plot_data_no_zeros, aes(x = data, color = sample_size,linetype = hypothesis)) + 
  stat_ecdf(geom = "step") + 
  xlab("X") + 
  ggtitle("Emprical CDF of log-likelihood ratio", sub = "H0 and H1") +
  geom_vline(xintercept = quantile(h0_5000_samples_llrt$data, 0.95), linetype = 'dotted', color = 'blue')

