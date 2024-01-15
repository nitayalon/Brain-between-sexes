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

data = data.frame(sex = factor(c(rep(0,sample_size),rep(1,sample_size))), 
                  size = c(men,women))

data %>% 
  ggplot(aes(x = size, fill =factor(sex))) + 
  geom_histogram(aes(y =..density..),bins = 100, position = "identity",
                 alpha=0.6)+
  scale_fill_manual(values = c("royalblue", "tomato1")) + 
  theme_bw()

data %>% 
  ggplot(aes(x = size, fill =factor(sex))) + 
  geom_histogram(aes(y =..density..),bins = 100, position = "identity",
                 alpha=0.6)+
  scale_fill_manual(values = c("royalblue", "tomato1")) + 
  stat_function(fun = dnorm, args = list(mean = mean(c(men,women)), 
                                         sd = sd(c(men,women))),size=1.5) + 
  theme_bw()

data %>% 
  ggplot(aes(x = size, fill =factor(sex))) + 
  geom_histogram(aes(y =..density..),bins = 100, position = "identity",
                 alpha=0.6)+
  scale_fill_manual(values = c("royalblue", "tomato1")) + 
  stat_function(fun = dnorm, args = list(mean = mean(c(men)), 
                                         sd = sd(c(men))),size=1.5,
                linetype = "solid", colour="blue") + 
  stat_function(fun = dnorm, args = list(mean = mean(c(women)), 
                                         sd = sd(c(women))),size=1.5,
                linetype = "dashed", colour="red") + 
  theme_bw()

data %>% 
  ggplot(aes(x = size, fill =factor(sex))) + 
  geom_histogram(aes(y =..density..),bins = 100, position = "identity",
                 alpha=0.6)+
  scale_fill_manual(values = c("royalblue", "tomato1")) + 
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
  theme_bw()

### Tail distribution
men <- ifelse(runif(sample_size) > 0.7, masculine, feminine)
women <- ifelse(runif(sample_size) > 0.2, feminine, masculine)

data = data.frame(sex = factor(c(rep(0,sample_size),rep(1,sample_size))), 
                  size = c(men,women))
data %>% 
  ggplot(aes(x = size, fill =factor(sex))) + 
  geom_histogram(aes(y =..density..),bins = 100, position = "identity",
                 alpha=0.6)+
  scale_fill_manual(values = c("royalblue", "tomato1")) + 
  theme_bw() + 
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(0.5, 0.5, lam = 0.2),
                linetype = "solid", colour="red", lwd=1.5) +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(-0.5, 0.5, lam = 0.8),
                linetype = "dashed", colour="red", lwd=1.5) +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(0.5, 0.5, lam = 0.3),
                linetype = "solid", colour="blue", lwd=1.5) +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(-0.5, 0.5, lam = 0.7),
                linetype = "dashed", colour="blue", lwd=1.5)
