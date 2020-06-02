#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector EM(NumericVector men_data, NumericVector women_data, int number_of_iterations) {
  //Init values
  int m = men_data.size(); 
  int n = women_data.size();
  double p = 0.51;
  double q = 0.49;
  double men_bar = mean(men_data);
  double women_bar = mean(women_data);
  double mu_1 = ((1-q) * men_bar - (1-p) * women_bar) / (p*(1-q) - q*(1-p)); 
  double mu_2 = (q * men_bar - p * women_bar) / (q*(1-p) - p*(1-q));
  double sigma_2_men = max(NumericVector::create(std::pow(mu_1 - mu_2,2)*(p+q)/2*(2-p-q)/2,2));
  double sigma_2_women = max(NumericVector::create(std::pow(mu_1 - mu_2,2)*(p+q)/2*(2-p-q)/2,2));
  NumericMatrix track_parameters(number_of_iterations); 
  for(int i = 0; i < number_of_iterations; i++)
  {
    //EStep
    double sigma_men = std::sqrt(sigma_2_men);
    double sigma_women = std::sqrt(sigma_2_women);
    NumericVector I = p * dnorm(men_data,mu_1,sigma_men) / (p * dnorm(men_data,mu_1,sigma_men) + (1-p)*dnorm(men_data,mu_2,sigma_women));
    NumericVector J = q * dnorm(women_data,mu_1,sigma_men) / (q * dnorm(women_data,mu_1,sigma_men) + (1-q)*dnorm(women_data,mu_2,sigma_women));
    //MStep
    p = mean(I);
    q = mean(J);
    double i_temp = 0;
    double j_temp = 0;
    
    for(int j = 0; j < I.size(); j ++){
      i_temp += I(j) * men_data(j);
    }
    for(int j = 0; j < I.size(); j ++){
      j_temp += J(j) * women_data(j);
    }
    
    mu_1 = (i_temp + j_temp) / (sum(I) + sum(J));
    mu_2 = (sum(men_data) + sum(women_data) - (i_temp + j_temp)) / (m + n - (sum(I) + sum(J)));
    sigma_2_men = (I %*% (men - mu_1)^2  + J %*% (women - mu_1)^2) / (sum(I) + sum(J))
    sigma_2_women = (1-I) %*% (men - mu_2)^2 +  (1-J) %*% (women - mu_2)^2) / (m + n - (sum(I) + sum(J))
    )  
  }
}

