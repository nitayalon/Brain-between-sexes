
logNormalGridSearch <- function(feature_data,...)
{
  # Step 1 - compute the -llk over feasible grid
  llk_over_grid <- logNormalGridLlkComputation(feature_data)
  # Step 2 - compute the -llk for the null hypothesis
  null_llk <- logNormalNullLlkComputation(feature_data)
  # Step 3 - compute the llrt over the grid
  llrt_over_grid <- computeLlrtOverGrid(llk_over_grid,null_llk)
  # Step 4 - return the results
  return(list(
    null_llk = null_llk,
    llrt_over_grid = llrt_over_grid))
}