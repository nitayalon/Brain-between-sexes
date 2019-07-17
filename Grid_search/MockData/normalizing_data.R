normalizingData <- function(men, women){
  pop <- c(men,women)
  pop_mean <- mean(pop)
  pop_size <- length(pop)
  pop_sd <- sd(pop)
  normalized_men <- (men-pop_mean) / (pop_sd)
  normalized_women <- (women-pop_mean) / (pop_sd )
  return(list(
    men = normalized_men,
    women = normalized_women
  ))
}