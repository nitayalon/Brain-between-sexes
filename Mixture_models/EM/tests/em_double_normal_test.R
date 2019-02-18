# 50-50
m = n = 500
dist_1 <- rnorm(m, 2, 2.5)
dist_2 <- rnorm(m, -1, 2.5)

group_a_ind <- sample(m,250,replace = F)
group_a <- c(dist_1[group_a_ind], dist_2[group_a_ind])
group_b <- c(dist_1[-group_a_ind], dist_2[-group_a_ind])

obs <- list(men = group_a, women = group_b)
em <- doubleDoubleEM(obs)

# 90-10
m = n = 500
dist_1 <- rnorm(m, 2, 2.5)
dist_2 <- rnorm(m, -1, 2.5)

group_a_ind_men <- sample(m,450,replace = F)
group_a_ind_women <- sample(n,50,replace = F)

group_a <- c(dist_1[group_a_ind_men], dist_2[group_a_ind_women])
group_b <- c(dist_1[-group_a_ind_men], dist_2[-group_a_ind_women])

hist(group_a, col = rgb(1,0,1,alpha = 0.2), breaks = 50)
hist(group_b, col = rgb(0,1,1,alpha = 0.2), breaks = 50, add = T)

obs <- list(men = group_a, women = group_b)
em <- doubleDoubleEM(obs)
plot(em$llk)
