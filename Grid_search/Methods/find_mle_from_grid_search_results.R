findMleForFeatureFromGridSearch <- function(llk_results){
  mle_line <- llk_results[which.min(-llk_results$llk),]
  return(mle_line)
}