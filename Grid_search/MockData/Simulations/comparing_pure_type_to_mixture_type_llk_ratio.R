mu = 0.2; p = 1; q = 0; sigma_2 = 0.95; m = 1e6

pure <- sampleMockData(mu,-mu,p,q,sigma_2,m)
var(c(pure$men,pure$women))
var(pure$men)
var(pure$women)
summary(pure$men)
summary(pure$women)

norm.pop <- normalizingData(pure$men,pure$women)
summary(norm.pop$men)
summary(norm.pop$women)
summary(c(norm.pop$women, norm.pop$men))
var(c(norm.pop$men,
      norm.pop$women))
var(norm.pop$men)
var(norm.pop$men)

pure.men.norm <- hist(norm.pop$men
                      ,breaks = seq(-5.5,5.5,length.out = 100), plot = F)
pure.women.norm <- hist(norm.pop$women
                        ,breaks = seq(-5.5,5.5,length.out = 100), plot = F)

pure.men <- hist(pure$men,breaks = seq(-5.5,5.5,length.out = 100), plot = F)
pure.women <- hist(pure$women,breaks = seq(-5.5,5.5,length.out = 100), plot = F)
plot(pure.men, col = "blue")
plot(pure.women, col = "red", add = T)
plot(pure.women.norm$breaks[-1], 
     log(pure.men.norm$density) - log(pure.women.norm$density))
abline(a = 0, b = 0.4, col = "black")

mu = 0.2; p = 0.85; q = 0.1; sigma_2 = 0.95; m = 1e6
mix <- sampleMockData(mu,-mu,p,q,sigma_2,m)
norm.mix <- normalizingData(mix$men,mix$women)
var(c(mix$men,mix$women))
var(mix$men)
var(mix$women)
summary(mix$men)
pure <- sampleMockData(mu,-mu,p,q,sigma_2,m)
var(c(pure$men,pure$women))
var(pure$men)
var(pure$women)
summary(pure$men)
summary(pure$women)

norm.pop <- normalizingData(pure$men,pure$women)
summary(norm.pop$men)
summary(norm.pop$women)
summary(c(norm.pop$women, norm.pop$men))
var(c(norm.pop$men,
      norm.pop$women))
var(norm.pop$men)
var(norm.pop$men)

pure.men.norm <- hist(norm.pop$men
                      ,breaks = seq(-5.5,5.5,length.out = 100), plot = F)
pure.women.norm <- hist(norm.pop$women
                        ,breaks = seq(-5.5,5.5,length.out = 100), plot = F)

pure.men <- hist(pure$men,breaks = seq(-5.5,5.5,length.out = 100), plot = F)
pure.women <- hist(pure$women,breaks = seq(-5.5,5.5,length.out = 100), plot = F)
plot(pure.men, col = "blue")
plot(pure.women, col = "red", add = T)
plot(pure.women.norm$breaks[-1], 
     log(pure.men.norm$density) - log(pure.women.norm$density))
abline(a = 0, b = 0.4, col = "black")

# Mix ---------------------------------------------------------------------


mu = 0.2; p = 0.75; q = 0.15; sigma_2 = 0.95; m = 1e2
mix <- sampleMockData(mu,-mu,p,q,sigma_2,m)
norm.mix <- normalizingData(mix$men,mix$women)
var(c(mix$men,mix$women))
var(mix$men)
var(mix$women)
summary(mix$men)
summary(mix$women)

mix.men <- hist(mix$men,breaks = seq(-5.5,5.5,length.out = 100), plot = F)
mix.women <- hist(mix$women,breaks = seq(-5.5,5.5,length.out = 100), plot = F)

mix.norm.men <- hist(norm.mix$men,breaks = seq(-5.5,5.5,length.out = 100), plot = F)
mix.norm.women <- hist(norm.mix$women,breaks = seq(-5.5,5.5,length.out = 100), plot = F)
plot(mix.men, col = "blue")
plot(mix.women, col = "red", add = T)
plot(mix.norm.women$breaks[-1], log(mix.norm.men$density) - log(mix.norm.women$density))
abline(a = 0, b = 0.4, col = "black")

plot(pure.women.norm$breaks[-1], 
     log(pure.men.norm$density) - log(pure.women.norm$density), col = "red")
points(mix.norm.women$breaks[-1], log(mix.norm.men$density) - log(mix.norm.women$density)
       ,col = "green")


summary(mix$women)

mix.men <- hist(mix$men,breaks = seq(-5.5,5.5,length.out = 100), plot = F)
mix.women <- hist(mix$women,breaks = seq(-5.5,5.5,length.out = 100), plot = F)

mix.norm.men <- hist(norm.mix$men,breaks = seq(-5.5,5.5,length.out = 100), plot = F)
mix.norm.women <- hist(norm.mix$women,breaks = seq(-5.5,5.5,length.out = 100), plot = F)
plot(mix.men, col = "blue")
plot(mix.women, col = "red", add = T)
plot(mix.norm.women$breaks[-1], log(mix.norm.men$density) - log(mix.norm.women$density))
abline(a = 0, b = 0.4, col = "black")

plot(pure.women.norm$breaks[-1], 
     log(pure.men.norm$density) - log(pure.women.norm$density), col = "red")
points(mix.norm.women$breaks[-1], log(mix.norm.men$density) - log(mix.norm.women$density)
       ,col = "green")


# Validating the sampling -------------------------------------------------

library(ggplot2)
mu = 0.2; p = 0.75; q = 0.15; sigma_2 = 0.95; m = 1e4
mix <- sampleMockData(mu,-mu,p,q,sigma_2,m, T)
ggplot(mix$men,aes(men)) + geom_histogram(fill = "blue", alpha = 0.2
                                          ,bins = 70) + 
  geom_histogram(data=mix$men, aes(pure_men), fill = "red", alpha = 0.2
                 ,bins = 70) 

ggplot(mix$men,aes(men)) + geom_histogram(fill = "blue", alpha = 0.2
                                          ,bins = 70) +
  geom_histogram(data=mix$men, aes(pure_feminine), fill = "green", alpha = 0.2
                 ,bins = 70)

ggplot(mix$women,aes(women)) + geom_histogram(fill = "blue", alpha = 0.2
                                          ,bins = 70) + 
  geom_histogram(data=mix$women, aes(pure_men), fill = "red", alpha = 0.2
                 ,bins = 70)

ggplot(mix$women,aes(women)) + geom_histogram(fill = "blue", alpha = 0.2
                                              ,bins = 70) + 
  geom_histogram(data=mix$women, aes(pure_feminine), fill = "green", alpha = 0.2
                 ,bins = 70)


##### Compute MLE's:
mu = 0.2; p = 0.75; q = 0.15; sigma_2 = 0.95; m = 1e4
mix <- sampleMockData(mu,-mu,p,q,sigma_2,m, T)
population <- data.frame(men = mix$men$men, women = mix$women$women)
MLE <- computeMLEOverGridMockData(population)
debug(computeLlkNullHypothesis)
debug(computeLogLikelihoodForFeature)
debug(evaluateLogLikelihoodOFMixtureModel)
H0_llk <- computeLlkNullHypothesis(mean(population$men), 
                                   population$men,population$women)
