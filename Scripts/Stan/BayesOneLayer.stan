// This script contains a single-layer Hierarchical Bayesian model to predict salinity at Havre de Grace, MD
// Initial priors are informed by several regression techniques in R.


// Data Block
data {
   int<lower=0> N;                          // number of observations
   vector[N] Norm_Salinity;                 // response variable: salinity (NOT normalized)
   vector[N] Norm_Discharge;                // normalized discharge from dam
   vector[N] Norm_Tide;                     // normalized tide level
   vector[N] Norm_RollingDischarge;         // normalized rolling average discharge
   vector[N] Norm_Inflows;                  // normalized upstream flow at Marietta
   vector[N] Norm_FERC;                     // normalized FERC minimum flow requirement
}

// Parameter Block
parameters {
   real alpha;                              // intercept
   real beta_Norm_Discharge;                // coefficient for discharge
   real beta_Norm_Tide;                     // coefficient for tide
   real beta_Norm_RollingDischarge;         // coefficient for rolling discharge
   real beta_Norm_Inflows;                  // coefficient for Marietta flow
   real beta_Norm_FERC;                     // coefficient for FERC requirement
   real<lower=0> sigma;                     // error scale
}

// Model Block
model {
   // Define the Informed Priors
   alpha ~ normal();
   beta_Norm_Discharge ~ normal();
   beta_Norm_Discharge ~ normal();
   beta_Norm_RollingDischarge ~ normal(); 
   beta_Norm_Inflows ~ normal();
   beta_Norm_FERC ~ normal();
   sigma ~ exponential(1 / PRIOR_SIGMA_MEAN);
   
   // Compute the Likelihood
   vector[N] mu;
   mu = alpha + (beta_Norm_Discharge * Norm_Discharge) + 
                (beta_Norm_Tide * Norm_Tide) + 
                (beta_Norm_RollingDischarge * Norm_RollingDischarge) + 
                (beta_Norm_Inflows * Norm_Inflows) + 
                (beta_Norm_FERC * Norm_FERC);
                
   
}





