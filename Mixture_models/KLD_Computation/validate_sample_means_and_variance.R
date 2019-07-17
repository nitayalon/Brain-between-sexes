validateMarginalDistributions <- function(samples){
  stopifnot(names(samples) == c("men","women"))
  pop_mean <- mean(c(samples$men,samples$women))
  pop_var <- var(c(samples$men,samples$women))
  return(c(pop_mean = pop_mean, pop_var = pop_var))
}