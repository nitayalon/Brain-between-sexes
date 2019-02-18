#' Following Isaco's mail, in this script we'll compute the KLD over
#' a cartesian product of p and mu in order to find out the required sample size
#' to make a discovery if the alternative is true

# sample data for each pair p-mu
mu <- seq(0.01, 0.25, by = 0.01)
p <- seq(0.51,0.9,by = 0.01)
parameters <- expand.grid(mu,p)
variance <- apply(parameters, 1, function(x){computeSexVariance(x[1],x[2])})

KLD.results <- sampleAndComputeKldForPair(1e5, parameters, variance, "laplace")

variance_feasible_index <- variance > 0 
KLD_feasible <- KLD.results$KLD != Inf & KLD.results$KLD != -Inf
results <- cbind(parameters[KLD_feasible,],
                 theta[KLD_feasible], 
                 KLD.results$KLD, KLD.results$pop_variance)
names(results) <- c("mu","p","theta","KLD","Population_Variance")
results <- results[results$KLD != Inf & results$KLD != -Inf,]
# Analysis ----------------------------------------------------------------

library(plotly)
hist(results$KLD, 
     breaks = seq(min(results$KLD) - 1,max(results$KLD)+1,length.out = 100))

KLD.vs.p.mu <- plot_ly(x = ~results$mu, y = ~results$p, z = ~-results$KLD,
             color = ~-results$KLD) %>% 
  add_markers(marker=list(sizeref=1)) %>%
  layout(scene = list(xaxis = list(title = 'Mu'),
                      yaxis = list(title = 'P'),
                      zaxis = list(title = 'KLD')))
KLD.vs.p.mu

theta.vs.p.mu <- plot_ly(x = ~results$mu, y = ~results$p, z = ~results$theta,
             color = ~results$theta) %>% 
  add_markers(marker=list(sizeref=1)) %>%
  layout(scene = list(xaxis = list(title = 'Mu'),
                      yaxis = list(title = 'P'),
                      zaxis = list(title = 'Theta')))
theta.vs.p.mu

summary(-results$KLD)
write.csv(results, "~/mastersdegree/Thesis/DAPHNA_JOEL/Results/Reports/
          KLD_computation_over_grid_07_05_2018.csv")
