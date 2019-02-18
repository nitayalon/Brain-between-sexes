config <- config::get(file = "config.yml")

doubleDoubleEM <- function(observations
                           ,bio_bank_data = T
                           ,max_iter = config$em_iteration
                           ,init_parameters = NULL, ...)
{
  if(!setequal(names(observations),c("men","women")))  
  {
    if(bio_bank_data)
    {
      observations <- prepareDataBioBank(observations)
    }
    else
    {
      observations <- prepareData(observations)
    }
  }
  
  if(is.null(init_parameters) || !validateInitEMParameters(init_parameters))
  {
    p <- 0.51
    q <- 0.49
    men_bar <- mean(observations$men)
    women_bar <- mean(observations$women)
    distributions_parameters <- computeInitParameters(p,q,men_bar,women_bar)
    m_parameters <- list(p = p, q = q, mu_1 = distributions_parameters$mu_1,
                         mu_2 = distributions_parameters$mu_2, 
                         sigma_2 = distributions_parameters$sigma_2)
  }
  
  llk <- NULL
  stopping_condition <- F
  
  for(i in 1:max_iter)
  {
    if(stopping_condition)
    {
      break
    }
    # Estep
    e_parameters <- EStep(m_parameters,observations)
    # Mstep
    m_parameters <- 
      tryCatch(
        {
          MStep(e_parameters,observations,do_not_ignore_NA = F)
        },
        error=function(cond)
        {
          MStep(e_parameters,observations,do_not_ignore_NA = F)
          stopping_condition <- T
        }
        )
    # llk
    llk[i] <- computeLogLikelihoodFull(observations,m_parameters)
  }
  em_results <- list(e_parameters = e_parameters,
                     m_parameters = m_parameters,
                     llk = llk)
  return(em_results)
}