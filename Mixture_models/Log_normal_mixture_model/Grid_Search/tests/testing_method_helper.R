men_theta <- seq(0,10,length.out = 100)
women_theta <- seq(0,10,length.out = 100)
outer_parameter_grid <- expand.grid(men_theta,women_theta) 

i <- sample(1:10000,1)
p <- runif(1)
q <- runif(1)
theta.mas <- outer_parameter_grid[i,]$Var1
theta.fem <- outer_parameter_grid[i,]$Var2
men.avg <-  theta.mas * p + (1-p) * theta.fem
women.avg <- theta.mas * q + (1-q) * theta.fem

# Testing computeMixtureProbabilityForPair:
round(c(p,q),3) == round(computeMixtureProbabilityForPair(theta.mas,theta.fem,
                                 men.avg,women.avg),3)
# Testing computeGroupVariance:
computeGroupVariance(theta.mas,
                     theta.fem)
 var(c(theta.mas,
    theta.fem))
