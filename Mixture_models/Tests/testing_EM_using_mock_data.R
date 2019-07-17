# Sample the data ---------------------------------------------------------
masculine.prob <- 0.8
feminine.prob <- 0.8
masculine.theta <- 0.3
feminine.theta <- -masculine.theta

m <- 10000
w <- 10000
z <- rnorm(m)
plot(density(rnorm(m, masculine.theta, 1)), main = "Masculine distribution", col = 1)
lines(density(rnorm(m, feminine.theta, 1)), col = 2)

men.ind <- sample(c(0,1),m,prob = c(1 - masculine.prob, masculine.prob), replace = T)
women.ind <- sample(c(0,1),w,prob = c(1 - feminine.prob, feminine.prob), replace = T)

table(men.ind)
table(women.ind)

men <- men.ind * (z + masculine.theta) +  (1-men.ind) * (z + feminine.theta)
women <- (1 - women.ind) * (z + masculine.theta) + (women.ind) * (z + feminine.theta) 
m1 <- mean(men)
m2 <- mean(women)
pop <- c(men,women)
summary(pop)

plot(density(pop), col = 3)
lines(density(men), col = 4)
lines(density(women), col = 5)

res <- EM(0.5, 0.5, 1, men, women, m1, m2, 1000, 0.0001)
llk <- computeLogLikelihood(men, women, theta_mas = res$mu_1, theta_fem = res$mu_2, 
                            p = res$p, 
                            q = res$q, sigma_2 = res$sigma_2)
