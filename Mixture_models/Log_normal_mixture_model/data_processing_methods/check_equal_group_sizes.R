checkEqualSizes <- function(men_ind, women_ind, number_of_people){
  stopifnot(is.logical(men_ind) && is.logical(women_ind))
  m = sum(men_ind)
  n = sum(women_ind) 
  stopifnot(is.numeric(number_of_people) & number_of_people == m + n)
  if(m != n){
    cat("Imbalanced gender data", "\n")
    # More men than womem
    if(m > n)
    {
      men_ind <- sample((1:number_of_people)[men_ind], n, replace = F)
      women_ind <- (1:number_of_people)[women_ind]
    }
    # More women than men
    if(m < n)
    {
      women_ind <- sample((1:number_of_people)[women_ind], m, replace = F)
      men_ind <- (1:number_of_people)[men_ind]
    }
  }
  return(list(
    men_ind = men_ind,
    women_ind = women_ind
  ))
}