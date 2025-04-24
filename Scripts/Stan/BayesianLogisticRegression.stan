# This script defines the base of the Bayesian Logistic Regression that attempts 
# to predict salt at the mouth of the Susquehanna River, at Havre de Grace, MD. 

data {
   
}

parameters {
   
}

model {
 # Model Priors (initial spreads of the parameters)
 alpha ~ normal(0, 2);
 sigma_travel ~ normal(0, 1); 
 sigma_flush ~ normal(0, 1);
 sigma_tide ~ normal(0, 1);
 
 beta_travel ~ normal(0, sigma_travel);
 beta_flush ~ normal(0, sigma_flush);
 beta_tide ~ normal(0, sigma_tide);
 
 # Computing the Likelihood
 for (i in 1 : N) {
    real mu = alpha + 
    
 }
   
}