###QA script

GSP.VBM <- read.csv("~/MastersDegree/Thesis/DAPHNA_JOEL/Data/data_GSP_VBM.csv")
GSP.VBM.mle.features <- dat[dat$file_name == "data_GSP_VBM.csv",]

X.19 <- GSP.VBM.mle.features[GSP.VBM.mle.features$feature_list == "X.19",]

X.19.pop <- GSP.VBM$X.19
X.19.men <- GSP.VBM$X.19[GSP.VBM$Gender == 1]
X.19.women <- GSP.VBM$X.19[GSP.VBM$Gender == 2]
H0 <- computeLlkNullHypothesis(X.19$x_bar, X.19.men, X.19.women)
H1 <- computeLogLikelihoodForFeature(X.19.men, X.19.women,
                                     theta_2 = X.19$Theta2, theta_1 = X.19$Theta1,
                                     p = X.19$p, q = X.19$q, sigma_2 = X.19$Sigma2 
                                      )

#### Visual inspection
n <- 1e5
masculine <- rnorm(n, X.19$Theta2, X.19$Sigma2)
feminine <- rnorm(n, X.19$Theta1, X.19$Sigma2)
men <- rnorm(n, X.19$x_bar, 1 + X.19$x_bar * X.19$y_bar)
women <- rnorm(n, X.19$y_bar, 1 + X.19$x_bar * X.19$y_bar)

plot(density(masculine), col = 1)
lines(density(feminine), col = 2)
lines(density(X.19.men), col = 3)
lines(density(X.19.women), col = 4)

plot(density(men), col = 1)
lines(density(women), col = 2)
lines(density(X.19.men), col = 3)
lines(density(X.19.women), col = 4)


H0.mixture.model <- 1*men + 0 * women
H1.mixture.model <- X.19$p*masculine + (1 - X.19$p) * feminine
plot(density(H1.mixture.model), col = 1)
lines(density(H0.mixture.model), col = 2)
lines(density(X.19.men), col = 3)


plot(density(X.19$p*masculine), col = 1)
lines(density((1 - X.19$p) * feminine), col = 2)
lines(density(men), col = 3)
lines(density(X.19.men), col = 4)

men.prob.H0 <- dnorm(X.19.men, X.19$x_bar, 1 + X.19$x_bar * X.19$y_bar,log = F)
men.prob.H1.Fem <- dnorm(X.19.men, X.19$Theta1, X.19$Sigma2,log = F)
men.prob.H1.Mas <- dnorm(X.19.men, X.19$Theta2, X.19$Sigma2,log = F)

probs <- cbind(X.19.men, men.prob.H0, men.prob.H1.Mas, men.prob.H1.Fem)
plot(X.19.men, men.prob.H0, col = 1, ylim = c(0,0.6))
points(X.19.men, men.prob.H1.Mas, col = 2)
points(X.19.men, men.prob.H1.Fem, col = 3)


# LDA ---------------------------------------------------------------------
lda.men <- dnorm(X.19.men, X.19$Theta1, X.19$Sigma2,log = F) * X.19$p / 
  (dnorm(X.19.men, X.19$Theta1, X.19$Sigma2,log = F) * X.19$p + 
     dnorm(X.19.men, X.19$Theta2, X.19$Sigma2,log = F) * (1 -  X.19$p))
plot(X.19.men, lda.men, col = ifelse(lda.men > 0.5, 1, 2))

lda.women <- dnorm(X.19.women, X.19$Theta2, X.19$Sigma2,log = F) * X.19$q / 
  (dnorm(X.19.women, X.19$Theta1, X.19$Sigma2,log = F) * X.19$q + 
     dnorm(X.19.women, X.19$Theta2, X.19$Sigma2,log = F) * (1 -  X.19$q))
plot(X.19.women, lda.women, col = ifelse(lda.women > 0.5, 1, 2))

