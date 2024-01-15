# config <- config::get(file = "~/Human_brain_research/Mixture_models/EM/config.yml")

doubleDoubleEM <- function(feature_data
                           ,all_data
                           ,bio_bank_data = T
                           ,max_iter = 3000
                           ,init_parameters = NULL, ...)
{
  if(!setequal(names(feature_data),c("men","women")))  
  {
    if(bio_bank_data)
    {
      observations <- prepareDataBioBank(feature_data)
      full_data <- prepareDataBioBank(all_data)
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
    m_parameters <- list(p = p, 
                         q = q, 
                         mu_1 = distributions_parameters$mu_1,
                         mu_2 = distributions_parameters$mu_2, 
                         sigma_2_men = distributions_parameters$sigma_2,
                         sigma_2_women = distributions_parameters$sigma_2)
  }
  sample_list <- c(1000 + 1:5,2000 + 1:5 ,3000 + 1:5)
  llk <- NULL
  stopping_condition <- F
  m_samples <- c()
  for(i in 1:max_iter)
  {
    # Estep
    e_parameters <- EStep(m_parameters,observations)
    # Mstep
    m_parameters <- 
      tryCatch(
        {
          #This was
          # MStep(e_parameters,observations,do_not_ignore_NA = F)
          MStep(e_parameters,observations,do_not_ignore_NA = T)
        },
        error=function(cond)
        {
          #This was
          # MStep(e_parameters,observations,do_not_ignore_NA = F)
          # stopping_condition <- T
          browser()
        }
        )
    if(i %in% sample_list)
    {
      m_samples <- rbind(m_samples,unlist(m_parameters))
    }
    # llk
    llk[i] <- computeLogLikelihoodFull(observations, m_parameters)
  }
  final_e_parameters <- EStep(m_parameters, full_data)
  
  men_responsebilities <- tibble(eid = all_data[all_data$sex == 1,]$eid,
                                  responsebility = final_e_parameters$I)
  women_responsebilities <- tibble(eid = all_data[all_data$sex == 0,]$eid,
                                  responsebility = final_e_parameters$J)
  
  em_results <- list(e_parameters = e_parameters,
                     m_parameters = m_parameters,
                     m_samples = m_samples,
                     all.llk = llk,
                     llk = tail(llk,1),
                     men_responsebilities = men_responsebilities,
                     women_responsebilities = women_responsebilities)
  return(em_results)
}