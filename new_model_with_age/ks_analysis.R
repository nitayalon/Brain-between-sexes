compute_cdf_for_ks_analysis <- function(residuals, mle_parameters)
{
  range = seq(min(residuals$residuals), max(residuals$residuals), 
              length.out=length(residuals$residuals))
  cdf = mle_parameters$p * pnorm(range, mle_parameters$mu_1,
                                       sqrt(mle_parameters$sigma_2_men)) +
    (1 - mle_parameters$p) * pnorm(range, mle_parameters$mu_2,
                                  sqrt(mle_parameters$sigma_2_women)) 
  return(list(range=range, cdf=cdf))
}