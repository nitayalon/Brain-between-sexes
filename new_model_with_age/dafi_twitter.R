library(ggplot2)
library(dplyr)
library(tidyverse)

plot_mix_comps <- function(x, mu, sigma, lam) {
  lam * dnorm(x, mu, sigma)
}

sample_size = 10000
masculine <- rnorm(sample_size, 0.5, 0.5)
feminine <- rnorm(sample_size, -0.5, 0.5)

men <- ifelse(runif(sample_size) > 0.8, masculine, feminine)
women <- ifelse(runif(sample_size) > 0.6, feminine, masculine)

hist(men, col = rgb(1,0,1,alpha = 0.2))
hist(women, add = T, col = rgb(1,0,0,alpha = 0.2))

data = data.frame(sex = factor(c(rep(0,sample_size),rep(1,sample_size))), 
                  size = c(men,women))
data %>% 
  filter(sex == 0) %>% 
  ggplot(aes(x = size, fill =sex, colour = sex)) + 
  geom_histogram(aes(y =..density..),bins = 100)

data %>% 
  ggplot(aes(x = size, fill = factor(sex))) + 
  geom_histogram(aes(y =..density..),bins = 50, position = "identity")
  

theoretical_curve_unimodel <- data %>% 
  ggplot(aes(x = size, fill =factor(sex))) + 
  geom_histogram(aes(y =..density..),bins = 50, position = "identity",
                 alpha=.8)+
  scale_fill_manual(values = alpha(c('dodgerblue','tomato'),.1)) + 
  stat_function(fun = dnorm, args = list(mean = mean(c(men,women)), 
                                         sd = sd(c(men,women))),size=1.5) + 
  xlim(c(-4,4)) +
  ylim(c(0,0.7)) +
  theme(legend.position="none")

ggsave("theoretical_model_single_model_for_all_people.png", theoretical_curve_unimodel)

theoretical_curve_prue_type <- data %>% 
  ggplot(aes(x = size, fill =factor(sex))) + 
  geom_histogram(aes(y =..density..),bins = 50, position = "identity",
                 alpha=.8)+
  scale_fill_manual(values = alpha(c('dodgerblue','tomato'),.1)) + 
  stat_function(fun = dnorm, args = list(mean = mean(c(men)), 
                                         sd = sd(c(men))),size=1.5,
                linetype = "solid", colour="blue") + 
  stat_function(fun = dnorm, args = list(mean = mean(c(women)), 
                                         sd = sd(c(women))),size=1.5,
                linetype = "dashed", colour="red") + 
  xlim(c(-4,4)) +
  ylim(c(0,0.7))+
  theme(legend.position="none")
ggsave("theoretical_model_pure_model_.png", theoretical_curve_prue_type)

theretical_model_mosaic <- data %>% 
  ggplot(aes(x = size, fill =factor(sex))) + 
  geom_histogram(aes(y =..density..),bins = 50, position = "identity",
                 alpha=0.8)+
  scale_fill_manual(values = alpha(c('dodgerblue', 'tomato'),.1)) + 
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(0.5, 0.5, lam = 0.6),
                linetype = "solid", colour="red", lwd=1.5) +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(-0.5, 0.5, lam = 0.4),
                linetype = "dashed", colour="red", lwd=1.5) +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(0.5, 0.5, lam = 0.2),
                linetype = "solid", colour="blue", lwd=1.5) +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(-0.5, 0.5, lam = 0.8),
                linetype = "dashed", colour="blue", lwd=1.5) +
  xlim(c(-4,4)) +
  ylim(c(0,0.7))+
  theme(legend.position="none")
ggsave("theoretical_mosaic_mixture_model.png", theretical_model_mosaic)
