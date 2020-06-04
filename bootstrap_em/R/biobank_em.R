biobank_bootstrap_em <- function(feature_data
                                 ,init_parameters = NULL      
                           ,max_iter = 3000
                           ,number_of_bootstrap_iteration = 200
                           ,...)
{
  observations = list(men = feature_data$value[feature_data$sex == 1],
                     women = feature_data$value[feature_data$sex == 0]) 
  bootstrap_results = matrix(nrow = 200, ncol = 6)
  iteration_progress = list()
  if(is.null(init_parameters))
  {
    p <- 0.51
    q <- 0.49
    men_bar <- mean(observations$men)
    women_bar <- mean(observations$women)
    mu_1 = ((1-q) * men_bar - (1-p) * women_bar) / (p*(1-q) - q*(1-p)) 
    mu_2 = (q * men_bar - p * women_bar) / (q*(1-p) - p*(1-q))
    sigma_2_men = sigma_2_women = max((mu_1 - mu_2)^2*(p+q)/2*(2-p-q)/2,2)
  }
  else{
    p <- init_parameters$p
    q <- init_parameters$q
    mu_1 = init_parameters$mu_1
    mu_2 = init_parameters$mu_2
    sigma_2_men = init_parameters$sigma_2_men
    sigma_2_women = init_parameters$sigma_2_women
  }
  start_time = Sys.time()
  for(i in 1:number_of_bootstrap_iteration)
  {
    population_data = bootstrap_data_sample(observations)
    em_results = EM(population_data$men, population_data$women, 3000, p, q, mu_1, mu_2, sigma_2_men, sigma_2_women) 
    bootstrap_results[i,] = unlist(em_results[1:6])  
    
    p_progress = diff(em_results[[9]])
    q_progress = diff(em_results[[10]])
    mu_1_progress = diff(em_results[[11]])
    mu_2_progress = diff(em_results[[11]])
    
    progress = data.frame(p_progress = p_progress,
                          q_progress = q_progress,
                          mu_1_progress = mu_1_progress,
                          mu_2_progress = mu_2_progress)
    iteration_progress[[i]] = progress
    end_time = Sys.time()
    if(i %% 10 == 0){
      cat(sprintf("Progress %s percent",i / number_of_bootstrap_iteration), "\n")
      cat(sprintf("Time took %s seconds", round(end_time - start_time), 3), "\n")
      start_time = Sys.time()
      }
  }
  # men_responsebilities <- tibble(eid = full_data[full_data$sex == 1,]$eid,
  #                                 responsebility = e_parameters$I)
  # women_responsebilities <- tibble(eid = full_data[full_data$sex == 0,]$eid,
  #                                 responsebility = e_parameters$J)
  # em_results <- list(e_parameters = e_parameters,
  #                    m_parameters = m_parameters,
  #                    m_samples = m_samples,
  #                    all.llk = llk,
  #                    llk = tail(llk,1),
  #                    men_responsebilities = men_responsebilities,
  #                    women_responsebilities = women_responsebilities)
  em_results = list(bootstrap_results = bootstrap_results, 
                    iteration_progress = iteration_progress)
  return(em_results)
}