// This script contains a single-layer Hierarchical Bayesian model to predict salinity at Havre de Grace, MD
// Initial priors are informed by several regression techniques in R.


// Data Block
data {
   // Actual Data
   int<lower=0> N;                          // number of observations
   vector[N] Salinity;                      // response variable: salinity (NOT normalized)
   vector[N] Norm_Discharge;                // normalized discharge from dam
   vector[N] Norm_Tide;                     // normalized tide level
   vector[N] Norm_RollingDischarge;         // normalized rolling average discharge
   vector[N] Norm_Inflows;                  // normalized upstream flow at Marietta
   vector[N] Norm_FERC;                     // normalized FERC minimum flow requirement
   
   // Initial Parameter Estimates (from regressions in R)
   real PRIOR_ALPHA_MEAN;                   // prior mean for intercept
   real<lower=0> PRIOR_ALPHA_SD;            // prior SD for intercept
   real PRIOR_DISCHARGE_MEAN;               // prior mean for Norm_Discharge
   real<lower=0> PRIOR_DISCHARGE_SD;        // prior SD for Norm_DIscharge
   real PRIOR_TIDE_MEAN;                    // prior mean for Norm_Tide
   real<lower=0> PRIOR_TIDE_SD;             // prior SD for Norm_Tide
   real PRIOR_ROLLDIS_MEAN;                 // prior mean for Norm_RollingDischarge
   real<lower=0> PRIOR_ROLLDIS_SD;          // prior SD for Norm_RollingDischarge
   real PRIOR_INFLOWS_MEAN;                 // prior mean for Norm_Inflows
   real<lower=0> PRIOR_INFLOWS_SD;          // prior SD for Norm_Inflows
   real PRIOR_FERC_MEAN;                    // prior mean for Norm_FERC
   real<lower=0> PRIOR_FERC_SD;             // prior SD for Norm_FERC
   real<lower=0> PRIOR_SIGMA_MEAN;          // prior mean for error scale
   
   // Parameters for Student's t degrees of freedom prior
   real<lower=0> PRIOR_NU_SHAPE;            // shape parameter for gamma prior on nu
   real<lower=0> PRIOR_NU_RATE;             // rate parameter for gamma prior on nu
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
   real<lower=1, upper=100> nu;             // degrees of freedom for Student's t
}

// Model Block
model {
   // Define the Informed Priors
   alpha ~ normal(PRIOR_ALPHA_MEAN, PRIOR_ALPHA_SD);
   beta_Norm_Discharge ~ normal(PRIOR_DISCHARGE_MEAN, PRIOR_DISCHARGE_SD);
   beta_Norm_Tide ~ normal(PRIOR_TIDE_MEAN, PRIOR_TIDE_SD);
   beta_Norm_RollingDischarge ~ normal(PRIOR_ROLLDIS_MEAN, PRIOR_ROLLDIS_SD); 
   beta_Norm_Inflows ~ normal(PRIOR_INFLOWS_MEAN, PRIOR_INFLOWS_SD);
   beta_Norm_FERC ~ normal(PRIOR_FERC_MEAN, PRIOR_FERC_SD);
   sigma ~ exponential(1 / PRIOR_SIGMA_MEAN);
   
   // Compute the prediction
   vector[N] mu;
   mu = alpha + (beta_Norm_Discharge * Norm_Discharge) + 
                (beta_Norm_Tide * Norm_Tide) + 
                (beta_Norm_RollingDischarge * Norm_RollingDischarge) + 
                (beta_Norm_Inflows * Norm_Inflows) + 
                (beta_Norm_FERC * Norm_FERC);
       
   // Compute the likelihood using Student_T distribution, giving preference to extrem salinity values         
   Salinity ~ student_t(nu, mu, sigma);
}

// Generated Quantities Block: For Validation and Posterior Predictions
generated quantities {
   vector[N] log_lik;
   vector[N] y_rep;                         // Posterior predictive replications
   
   for (n in 1:N) {
      // Linear predictor (mean prediction)
      real mu_n = alpha + 
                 (beta_Norm_Discharge * Norm_Discharge[n]) + 
                 (beta_Norm_Tide * Norm_Tide[n]) + 
                 (beta_Norm_RollingDischarge * Norm_RollingDischarge[n]) + 
                 (beta_Norm_Inflows * Norm_Inflows[n]) + 
                 (beta_Norm_FERC * Norm_FERC[n]);
                             
      // Log likelihood for model comparison (WAIC, LOO)
      log_lik[n] = student_t_lpdf(Salinity[n] | nu, mu_n, sigma);
      
      // Generate posterior predictive samples
      y_rep[n] = student_t_rng(nu, mu_n, sigma);
   }
}





