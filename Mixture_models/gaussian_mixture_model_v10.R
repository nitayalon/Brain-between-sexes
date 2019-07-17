# EM for each gender ------------------------------------------------------

EStage <- function(p,q,sigma_2,men,women,mu_1,mu_2){
  
  # p = probabiity that a men is masculine
  # q = probabiity that a women is masculine
  normal_dist_ratio_men <- exp(-1 / (sigma_2) * (mu_1 - mu_2) * (men - (mu_1 + mu_2)/2))
  normal_dist_ratio_women <- exp(-1 / (sigma_2) * (mu_1 - mu_2) * (women - (mu_1 + mu_2)/2))
  
  mi <- 1 / ( 1 + (1-p) / p * normal_dist_ratio_men)  
  
  mj <- 1 / ( 1 + (1-q) / q * normal_dist_ratio_women)
  
  return(list( mi = mi, mj = mj))
}

MStage <- function(men, women, mi, mj){
  
  m1 <- mean(men)
  m2 <- mean(women)
  mm1 <- mean(men ^ 2)
  mm2 <- mean(women ^ 2)
  
  A <- mean(men * mi) 
  B <- mean(women * mj)
  p <- mean(mi)
  q <- mean(mj)
  
  new_mu_1 <- (A + B)/(p + q)
  new_mu_2 <- (m1 + m2 - A - B)/(2 - p - q)
  
  sigma_2 <- (mm2 + mm1)/2 - new_mu_2^2 + ((p + q)/2)*(new_mu_2^2 - new_mu_1^2)
  
  return(list(A = A, B = B, p = p, q = q, new_mu_1 = new_mu_1, 
              new_mu_2 = new_mu_2, sigma_2 = sigma_2))
}

EM <- function(p,q,sigma_2,men,women,mu_1,mu_2, max_iter, converge_cond){
  place_holder <- rep(NA,max_iter)  
  track_changes <- data.frame(p = place_holder,
                              q = place_holder,
                              mu_1 = place_holder,
                              mu_2 = place_holder,
                              sigma_2 = place_holder)
  for(i in 1:max_iter){
    
    E_res <- EStage(p,q,sigma_2,men,women,mu_1,mu_2)
    M_res <- MStage(men,women, E_res$mi, E_res$mj)
    
    ### For debug only
    if(M_res$sigma_2 <= 0){browser()}
    
    # if(sum(c(abs(p - M_res$p),
    #          abs(q - M_res$q),
    #          abs(mu_1 - M_res$new_mu_1),
    #          abs(mu_2 - M_res$new_mu_2),
    #          abs(sigma_2 - M_res$sigma_2)
    # )) < converge_cond
    # ){break}
    
    p <- M_res$p
    q <- M_res$q
    mu_1 <- M_res$new_mu_1
    mu_2 <- M_res$new_mu_2
    sigma_2 <- M_res$sigma_2
    track_changes[i,]$p = p
    track_changes[i,]$q = q
    track_changes[i,]$mu_1 = mu_1
    track_changes[i,]$mu_2 = mu_2
    track_changes[i,]$sigma_2 = sigma_2
  }  
  
  return(list(
    p = p,
    q = q,
    mu_1 = mu_1,
    mu_2 = mu_2,
    sigma_2 = sigma_2
  ))
}