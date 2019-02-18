### This method validates the constraints of the grid search:
### theta1 <= -mu, theta2 >= mu, abs(theta1) * theta2 <= 1
validateConstraints <- function(llk_results ,mu){
  res <- apply(llk_results, 1, function(x){rowConstraints(x,mu)})
  all(res)
}

rowConstraints <- function(row, mu){
  return(row[2] <= mu & row[1] >= mu & abs(row[2]) * row[1] <= 1)
}