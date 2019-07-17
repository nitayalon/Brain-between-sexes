source("~/mastersdegree/Thesis/DAPHNA_JOEL/Mixture_models/KLD_Computation/generate_sample_from_mixture_model.R")
source("~/mastersdegree/Thesis/DAPHNA_JOEL/Mixture_models/KLD_Computation/KLD_computation.R")
source("~/mastersdegree/Thesis/DAPHNA_JOEL/Mixture_models/KLD_Computation/main_function.R")
source("~/mastersdegree/Thesis/DAPHNA_JOEL/Mixture_models/KLD_Computation/validate_sample_means_and_variance.R")

df <- mainFunction(sample_size = 1e3,create_auto_grid = T,xi, epsilon,delta)
write.csv(df,"~/mastersdegree/Thesis/DAPHNA_JOEL/Results/KLD/KLD_computation_16_08_2018.csv")

# Result analysis ---------------------------------------------------------
library(plotly)
hist(df$KLD_pop, freq = F)
discovery.region <- df[df$KLD_pop > 4,]
hist(discovery.region$xi + discovery.region$epsilon)
hist(-discovery.region$xi - discovery.region$delta)
hist(discovery.region$pop_mean)
hist(discovery.region$pop_var)

### ploting KLD vs the male and female means - full data
kld.vs.female.and.male <- data.frame(
  male_mean = df$xi + df$epsilon,
  female_mean = -df$xi - df$delta,
  KLD = df$KLD_pop,
  variance = cut(df$pop_var, breaks = c(0,0.95,1.05,2))
)

kld.vs.female.and.male.full.data.plot <- plot_ly(kld.vs.female.and.male,
                                       x = ~male_mean,
                                       y = ~female_mean,
                                       z = ~KLD,
                                       color = ~variance) %>% 
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = 'Male Mean'),
                      yaxis = list(title = 'Feamle Mean'),
                      zaxis = list(title = 'KLD')))
kld.vs.female.and.male.full.data.plot

### ploting KLD vs the male and female means - discovery region
kld.vs.female.and.male.data <- data.frame(
  male_mean = discovery.region$xi + discovery.region$epsilon,
  female_mean = -discovery.region$xi - discovery.region$delta,
  KLD = discovery.region$KLD_pop
)

kld.vs.female.and.male.plot <- plot_ly(kld.vs.female.and.male.data,
                                       x = ~male_mean,
                                       y = ~female_mean,
                                       z = ~KLD) %>% 
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = 'Male Mean'),
                      yaxis = list(title = 'Feamle Mean'),
                      zaxis = list(title = 'KLD')))
kld.vs.female.and.male.plot

### ploting the maximal KLD for each value of xi
kld.vs.xi <- data.frame(
  xi = df$xi,
  KLD = df$KLD_pop
)

p <- plot_ly(kld.vs.xi,
                           x = ~xi,
                           y = ~KLD)
kld.vs.xi.plot  <- subplot(
    p %>% add_markers(alpha = 0.2),
    p %>% add_histogram2d()
  )
kld.vs.xi.plot  
