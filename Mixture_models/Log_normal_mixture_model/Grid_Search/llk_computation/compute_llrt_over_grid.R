# Compute the llr between each grid set and the null hypothesis
computeLlrtOverGrid <- function(llk_over_grid,null_llk)
{
  llr <- apply(llk_over_grid, 1, function(x){llr(x[6],null_llk)})
  llk_over_grid$llr <- llr
  return(llk_over_grid)
}

llr <- function(alternative_llk, null_llk)
{
  return(alternative_llk - null_llk)
}