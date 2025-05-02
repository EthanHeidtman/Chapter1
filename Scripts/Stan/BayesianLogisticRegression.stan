# This script defines the base of the Bayesian Logistic Regression that attempts 
# to predict salt at the mouth of the Susquehanna River, at Havre de Grace, MD. 

data {
   int <lower=1> N;                     // number of observations (minimum 1)
   int <lower=0,upper=1> Exceedance[N]; // binary outcome (exceeds the threshold or it does not)
   vector[N] Discharge_norm;            // normalized discharge from the Conowingo USGS gauge
   vector[N] Tide;                      // normalized tide (reconstructed from Chesapeake City tides)
   vector[N] Salinity;                  // normalized salinity (measured by MD DNR)
   vector[N] Discharge_6hr;             // normalized 6-hour discharge from Conowingo USGS gauge
   
   
   // These are un-normalized for the priors
   vector[N] Discharge;  // raw, un-normalized discharge from the Conowingo USGS gauge
   vector[N] Inflows;    // raw, un-normalized flows at Marietta USGS gauge
   vector[N] FERC;       // raw, un-normalized FERC constraint flow
   
   // Hyperparameters for the Priors (fed from R, how much do we trust the priors??)
   real <lower=0> Inflows_prior_sd;
   real FERC_prior_mean
}

parameters {
   // The time-varying coefficients
   vector[N] beta_Discharge;
   vector[N] beta_Tide;
   vector[N] beta_Salinity;
   
   // Latent (missing) flow component coefficient 
   vector[N] missing_flow;
   
   // Intercept
   real alpha;
   
   // Hyperparameters for the time-varying coefficients
   real<lower=0> sigma_beta_Discharge;
   real<lower=0> sigma_beta_Tide;
   real<lower=0> sigma_beta_Salinity;
   
   // Hyperparameter for the latent flow smoothness
   real<lower=0> sigma_missing_flow;
}

model {
   vector[N] p; // Probability of Exceedance
   vector[N] Effective_Discharge; # if the turbine flow alone does not explain the salt
   
   // --------------------------------------------------------------------------
   // Define the Priors (knowledge we have about the system to initialize the model)
   // --------------------------------------------------------------------------
   
   alpha ~ normal(0, 5); // Intercept, weak
   
   // Randomly walk the coefficients
   beta_Discharge[1] ~ normal(0, 1);
   beta_Tide[1] ~ normal(0, 1);
   beta_Salinity[1] ~ normal(0, 1);
   
   for (n in 2 : N) {
      beta_Discharge[n] ~ normal(beta_Discharge[n-1], sigma_beta_Discharge);
      beta_Tide[n] ~ normal(beta_Tide[n - 1], sigma_beta_Tide);
      beta_Salinity[n] ~ normal(beta_Salinity[n - 1], sigma_beta_Salinity);
   }
   
   sigma_beta_Discharge ~ normal(0, 1);
   sigma_beta_Tide ~ normal(0, 1);
   sigma_beta_Salinity ~ normal(0, 1);
   
   // Latent Flow Smoothness
   missing_flow[1] ~ normal(0, 1);
   
   for (n in 2 : N) {
      missing_flow[n] ~ normal(missing_flow[n - 1], sigma_missing_flow);
   }
   
   sigma_missing_flow ~ normal(0, 1);
   
   // --------------------------------------------------------------------------
   // Define the weakly informative priors for Marietta and FERC flows
   // --------------------------------------------------------------------------
   

}











