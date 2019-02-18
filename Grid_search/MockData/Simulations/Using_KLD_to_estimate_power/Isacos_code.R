## This script is a translation of Isaco's script to compute the probability
## of rejecting the null hypothesis using KLD
N=1000000;mu=.05;th=.15;mu1=th-mu;mu0=-th-mu;

p=-mu0/(mu1-mu0);

Z=rnorm(N)+1*(mu0+(mu1-mu0)*(runif(N) < -mu0/(mu1-mu0)));

LLR=log(
  (mu1*exp(mu0*(Z-mu0/2))-mu0*exp(mu1*(Z-mu1/2)))
  /(mu1-mu0));

p

c(1000*mean(LLR), sqrt(1000)*sd(LLR))
