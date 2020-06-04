#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List EM(NumericVector men_data, NumericVector women_data, int number_of_iterations,
        double p, double q, double mu_1, double mu_2, double sigma_2_men, double sigma_2_women) {
  NumericVector p_path(number_of_iterations);  
  NumericVector q_path(number_of_iterations);  
  NumericVector mu_1_path(number_of_iterations);  
  NumericVector mu_2_path(number_of_iterations);  
  List results(12);
  //Init values
  int m = men_data.size(); 
  int n = women_data.size();
  NumericVector I(m);
  NumericVector J(n);
  for(int i = 0; i < number_of_iterations; i++)
  {
    //EStep
    double sigma_men = std::sqrt(sigma_2_men);
    double sigma_women = std::sqrt(sigma_2_women);
    I = p * dnorm(men_data,mu_1,sigma_men) / (p * dnorm(men_data,mu_1,sigma_men) + (1-p)*dnorm(men_data,mu_2,sigma_women));
    J = q * dnorm(women_data,mu_1,sigma_men) / (q * dnorm(women_data,mu_1,sigma_men) + (1-q)*dnorm(women_data,mu_2,sigma_women));
    //MStep
    p = mean(I);
    q = mean(J);
    double i_temp = 0;
    double j_temp = 0;
    
    for(int j = 0; j < I.size(); j ++){
      i_temp += I[j] * men_data[j];
    }
    for(int j = 0; j < J.size(); j ++){
      j_temp += J[j] * women_data[j];
    }
    
    mu_1 = (i_temp + j_temp) / (sum(I) + sum(J));
    mu_2 = (sum(men_data) + sum(women_data) - (i_temp + j_temp)) / (m + n - (sum(I) + sum(J)));
    
    double weighted_men_distance_from_mu_1 = 0;
    double weighted_women_distance_from_mu_1 = 0;
    double weighted_men_distance_from_mu_2 = 0;
    double weighted_women_distance_from_mu_2 = 0;
    
    for(int j = 0; j < I.size(); j ++){
      weighted_men_distance_from_mu_1 += I[j] * std::pow(men_data[j] - mu_1, 2);
      weighted_men_distance_from_mu_2 += (1-I[j]) * std::pow(men_data[j] - mu_2, 2);
    }
    
    for(int j = 0; j < J.size(); j ++){
      weighted_women_distance_from_mu_1 += J[j] * std::pow(women_data[j] - mu_1, 2);
      weighted_women_distance_from_mu_2 += (1-J[j]) * std::pow(women_data[j] - mu_2, 2);
    }
    
    sigma_2_men = (weighted_men_distance_from_mu_1  + weighted_women_distance_from_mu_1) / (sum(I) + sum(J));
    sigma_2_women = (weighted_men_distance_from_mu_2 +  weighted_women_distance_from_mu_2) / (m + n - (sum(I) + sum(J)));
    // Limit the variance ratio:
    double varince_ratio = sigma_2_men / sigma_2_women;
    if(varince_ratio < 1/3){
      sigma_2_women = sigma_2_men * 3;
    }
    if(varince_ratio > 3){
      sigma_2_women = sigma_2_men / 3;
    }
    p_path(i) = p;
    q_path(i) = q;
    mu_1_path(i) = mu_1;
    mu_2_path(i) = mu_2;
  }
  results[0] = p;
  results[1] = q;
  results[2] = mu_1;
  results[3] = mu_2;
  results[4] = sigma_2_men;
  results[5] = sigma_2_women;
  results[6] = I;
  results[7] = J;
  results[8] = p_path;
  results[9] = q_path;
  results[10] = mu_1_path;
  results[11] = mu_2_path;
  return(results);
}

