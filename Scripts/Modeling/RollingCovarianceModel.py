# =============================================================================
# Script Name:    RollingCovarianceModel.py
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-08-14
# Last Updated:   2025-08-14
# Description:    Implements rolling window distribution fitting (Phase 2) and
#                 covariance-based risk modeling (Phase 3) for salinity exceedance
#                 prediction and minimum release recommendations.
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
import pandas as pd
import numpy as np
from scipy import stats
from scipy.optimize import minimize_scalar
from datetime import datetime, timedelta
from sklearn.covariance import LedoitWolf
import json
import warnings
from types import SimpleNamespace
warnings.filterwarnings('ignore')

# Load your custom distribution classes
from Distributions import *


def dict_to_namespace(d):
    """Convert nested dictionary to SimpleNamespace for easy attribute access"""
    if not isinstance(d, dict):
        return d
    return SimpleNamespace(**{k: dict_to_namespace(v) for k, v in d.items()})


class RollingCovarianceModel:
    """
    Rolling window distribution fitting and window-based risk modeling
    """
    
    def __init__(self, config):
        self.config = config
        
        # Initialize distribution mappings from Distributions.py file
        self._initialize_distributions()
        
        # Validate distribution family
        if config.distribution_family not in self.available_distributions:
            available = list(self.available_distributions.keys())
            raise ValueError(f"Unsupported distribution: {config.distribution_family}. "
                           f"Available: {available}")
        
        self.dist_family = config.distribution_family
        self.dist_fitter = self.available_distributions[self.dist_family]
        
        # Storage for model components
        self.data = None
        self.predictors = None
        self.rolling_distributions = None
        self.predictor_data = None
        self.window_model = None
        
    def _initialize_distributions(self):
        """Initialize available distributions from Distributions.py file"""
        self.available_distributions = {
            'burr': Burr(),
            'gpd': GPD(),
            'gengamma': GenGamma(), 
            'lognormal': Lognormal(),
            'loglogistic': Loglogistic(),
            'gamma': Gamma()
        }
        
    def load_and_prepare_data(self):
        """Load CSV data and automatically identify predictors"""
        print(f"Loading data from: {self.config.data_csv}")
        
        try:
            # Load raw data
            self.data = pd.read_csv(self.config.data_csv)
            print(f"Loaded {len(self.data)} rows, {len(self.data.columns)} columns")
            
            # Parse datetime
            self._parse_datetime()
            
            # Identify and prepare predictors
            self._identify_predictors()
            
            # Handle cyclical variables
            self._handle_cyclical_variables()
            
            # Clean the data
            self._clean_data()
            
            print(f"Final dataset: {len(self.data)} rows, {len(self.predictors)} predictors")
            print(f"Predictors: {self.predictors}")
            
            return self.data, self.predictors
            
        except Exception as e:
            print(f"Error loading data: {e}")
            raise
    
    def _parse_datetime(self):
        """Parse datetime column with flexible formatting"""
        if 'DateTime' in self.data.columns:
            # Handle various datetime formats
            self.data['DateTime'] = self.data['DateTime'].astype(str).str.strip()
            self.data['DateTime'] = self.data['DateTime'].apply(
                lambda x: x if ':' in x else x + ' 00:00:00'
            )
            self.data['DateTime'] = pd.to_datetime(self.data['DateTime'], errors='coerce')
            
            # Remove invalid dates and sort
            initial_len = len(self.data)
            self.data = self.data.dropna(subset=['DateTime'])
            if len(self.data) < initial_len:
                print(f"Warning: Removed {initial_len - len(self.data)} rows with invalid dates")
                
            self.data = self.data.sort_values('DateTime').reset_index(drop=True)
        else:
            raise ValueError("No 'DateTime' column found in data")
    
    def _identify_predictors(self):
        """Use pre-selected predictors from data"""
        # Get all numeric columns except DateTime and salinity
        numeric_cols = self.data.select_dtypes(include=[np.number]).columns.tolist()
        exclude_cols = ['DateTime', 'Year', 'Month', 'Day', self.config.salinity_col]
        
        # Since predictors are pre-selected, use all available numeric columns
        self.predictors = [col for col in numeric_cols if col not in exclude_cols]
        
        if not self.predictors:
            raise ValueError("No valid predictor variables found in data")
            
        print(f"Using {len(self.predictors)} pre-selected predictors")
    
    def _handle_cyclical_variables(self):
        """Transform DayOfYear if present (since it's the only cyclical variable)"""
        if 'DayOfYear' in self.data.columns and 'DayOfYear' in self.predictors:
            print("Transforming DayOfYear as cyclical variable")
            angle = 2 * np.pi * self.data['DayOfYear'] / 365.25
            self.data['DayOfYear_sin'] = np.sin(angle)
            self.data['DayOfYear_cos'] = np.cos(angle)
            
            # Replace DayOfYear with sin/cos components
            self.predictors.remove('DayOfYear')
            self.predictors.extend(['DayOfYear_sin', 'DayOfYear_cos'])
    
    def _clean_data(self):
        """Clean and validate pre-processed data"""
        initial_len = len(self.data)
        
        # Remove rows with all missing values
        self.data = self.data.dropna(how = 'all')
        
        # Convert predictors and salinity to numeric
        for col in self.predictors + [self.config.salinity_col]:
            if col in self.data.columns:
                self.data[col] = pd.to_numeric(self.data[col], errors = 'coerce')
        
        # Remove rows where salinity is missing
        self.data = self.data.dropna(subset = [self.config.salinity_col])
        
        final_len = len(self.data)
        if final_len < initial_len:
            print(f"Removed {initial_len - final_len} rows during cleaning")
            
        if final_len < 100:
            print("Warning: Very few observations remaining after cleaning")

    def fit_rolling_distributions(self):
        """Phase 2: Fit parametric distributions to salinity within rolling windows"""
        print(f"Phase 2: Fitting {self.dist_family} distributions on rolling windows...")
        
        window_days = int(self.config.window_length)
        threshold = float(self.config.salinity_threshold)
        min_obs = getattr(self.config, 'min_observations_per_window', 10)
        
        results = []
        
        for i, row in self.data.iterrows():
            end_date = row['DateTime']
            start_date = end_date - pd.Timedelta(days=window_days)
            
            # Extract window data
            window_mask = (self.data['DateTime'] >= start_date) & (self.data['DateTime'] <= end_date)
            window_salinity = self.data.loc[window_mask, self.config.salinity_col].dropna()
            
            if len(window_salinity) < min_obs:
                continue
                
            try:
                # Fit distribution 
                param_dict = self.dist_fitter.fit_params(window_salinity.values)
                
                # Calculate exceedance probability P(Salinity > threshold)
                exceedance_prob = 1 - self.dist_fitter.cdf(threshold, param_dict)
                
                # Store results
                result = {
                    'timestamp': end_date,
                    'n_observations': len(window_salinity),
                    'exceedance_probability': float(exceedance_prob),
                    'distribution_family': self.dist_family
                }
                
                # Store parameters using their actual names
                for param_name, param_value in param_dict.items():
                    result[param_name] = float(param_value)
                
                results.append(result)
                
            except Exception as e:
                # Skip failed fi ants silently unless in debug mode
                if getattr(self.config, 'debug', False):
                    print(f"Distribution fit failed for {end_date}: {e}")
                continue
        
        if not results:
            raise ValueError("No successful distribution fits! Check data quality and parameters.")
            
        self.rolling_distributions = pd.DataFrame(results)
        print(f"Successfully fitted {len(self.rolling_distributions)} distributions")
        
        return self.rolling_distributions
    
    def prepare_predictor_data(self):
        """Extract window-averaged predictors for window modeling"""
        print("Preparing window-averaged predictor data...")
        
        window_days = int(self.config.window_length)
        min_obs = getattr(self.config, 'min_observations_per_window', 10)
        
        predictor_results = []
        
        for i, row in self.data.iterrows():
            end_date = row['DateTime']
            start_date = end_date - pd.Timedelta(days=window_days)
            
            # Extract window data
            window_mask = (self.data['DateTime'] >= start_date) & (self.data['DateTime'] <= end_date)
            window_data = self.data.loc[window_mask]
            
            if len(window_data) < min_obs:
                continue
                
            # Calculate window means for each predictor
            result = {'timestamp': end_date}
            
            for predictor in self.predictors:
                pred_values = window_data[predictor].dropna()
                if len(pred_values) >= 3:  # Minimum for meaningful average
                    result[predictor] = float(pred_values.mean())
                else:
                    result[predictor] = np.nan
            
            predictor_results.append(result)
        
        self.predictor_data = pd.DataFrame(predictor_results)
        self.predictor_data['timestamp'] = pd.to_datetime(self.predictor_data['timestamp'])
        self.predictor_data = self.predictor_data.set_index('timestamp').sort_index()
        
        # Remove rows with too many missing predictors
        max_missing = len(self.predictors) // 2  # Allow up to half missing
        self.predictor_data = self.predictor_data.dropna(thresh=len(self.predictors) - max_missing)
        
        print(f"Prepared predictor data for {len(self.predictor_data)} time periods")
        return self.predictor_data

    def logit_transform(self, probabilities):
        """Transform probabilities to logit space for linear modeling"""
        epsilon = getattr(self.config, 'logit_epsilon', 1e-6)
        p_clipped = np.clip(probabilities, epsilon, 1 - epsilon)
        return np.log(p_clipped / (1 - p_clipped))

    def inv_logit_transform(self, logit_values):
        """Transform from logit space back to probabilities"""
        # Clip to prevent overflow
        logit_clipped = np.clip(logit_values, -500, 500)
        return 1 / (1 + np.exp(-logit_clipped))

    def fit_covariance_model(self):
       """
       Phase 3: Fit covariance-based model using distribution parameter regression.
       
       Model: 
       θ₁ = α₀ + α₁×trace(Σ) + α₂×log_det(Σ) + α₃×condition(Σ) + α₄×λₘₐₓ(Σ)
       θ₂ = β₀ + β₁×trace(Σ) + β₂×log_det(Σ) + β₃×condition(Σ) + β₄×λₘₐₓ(Σ)
       
       Where θ₁, θ₂ are distribution parameters and Σ is the covariance matrix.
       """
       print("Phase 3: Fitting covariance-based distribution parameter model...")
       
       # Align distribution results with predictor data
       dist_times = pd.to_datetime(self.rolling_distributions['timestamp'])
       dist_times_index = pd.Index(dist_times)
       pred_times = self.predictor_data.index
       common_times = dist_times_index.intersection(pred_times)
       
       if len(common_times) < 20:
           raise ValueError("Insufficient overlapping time periods for covariance modeling")
       
       # Prepare aligned data
       dist_aligned = self.rolling_distributions.set_index('timestamp').loc[common_times]
       
       # Extract distribution parameters
       param_names = [col for col in dist_aligned.columns if col not in 
                      ['timestamp', 'n_observations', 'exceedance_probability', 'distribution_family']]
       
       if len(param_names) < 1:
           raise ValueError("No distribution parameters found in rolling_distributions")
       
       # Calculate covariance structure metrics for each window
       covariance_results = []
       window_days = int(self.config.window_length)
       min_obs = getattr(self.config, 'min_observations_per_window', 10)
       use_shrinkage = getattr(self.config, 'use_shrinkage', True)
       
       for timestamp in common_times:
           try:
               # Get rolling window of predictor data ending at this timestamp
               end_date = pd.to_datetime(timestamp)
               start_date = end_date - pd.Timedelta(days=window_days)
               
               window_mask = (self.data['DateTime'] >= start_date) & (self.data['DateTime'] <= end_date)
               window_predictors = self.data.loc[window_mask, self.predictors].dropna()
               
               if len(window_predictors) < min_obs:
                   continue
                   
               # Calculate covariance matrix with optional shrinkage
               X_window = window_predictors.values
               
               if use_shrinkage and len(X_window) > len(self.predictors) + 5:
                   # Use Ledoit-Wolf shrinkage for robust covariance estimation
                   lw = LedoitWolf()
                   cov_matrix = lw.fit(X_window).covariance_
                   shrinkage_intensity = lw.shrinkage_
               else:
                   # Sample covariance matrix
                   cov_matrix = np.cov(X_window.T, bias=False)
                   shrinkage_intensity = 0.0
                   
                   # Add ridge regularization if poorly conditioned
                   condition_num = np.linalg.cond(cov_matrix)
                   if condition_num > 1e12:
                       ridge_penalty = 1e-6 * np.trace(cov_matrix) / len(self.predictors)
                       cov_matrix += ridge_penalty * np.eye(len(self.predictors))
               
               # Extract covariance structure metrics
               trace_cov = np.trace(cov_matrix)
               det_cov = np.linalg.det(cov_matrix)
               condition_num = np.linalg.cond(cov_matrix)
               eigenvalues = np.linalg.eigvals(cov_matrix)
               largest_eigenval = np.max(eigenvalues)
               
               # Store results
               result = {
                   'timestamp': timestamp,
                   'trace_covariance': float(trace_cov),
                   'log_det_covariance': float(np.log(max(det_cov, 1e-12))),  # Avoid log(0)
                   'condition_number': float(min(condition_num, 1e12)),        # Cap extreme values
                   'largest_eigenvalue': float(largest_eigenval),
                   'shrinkage_intensity': float(shrinkage_intensity),
                   'n_observations': len(window_predictors)
               }
               
               # Add distribution parameters for this timestamp
               for param_name in param_names:
                   result[param_name] = dist_aligned.loc[timestamp, param_name]
               
               covariance_results.append(result)
               
           except Exception as e:
               if getattr(self.config, 'debug', False):
                   print(f"Covariance calculation failed for {timestamp}: {e}")
               continue
       
       if not covariance_results:
           raise ValueError("No successful covariance calculations!")
       
       # Convert to DataFrame
       cov_df = pd.DataFrame(covariance_results)
       print(f"Computed covariance metrics for {len(cov_df)} windows")
       
       # Prepare predictor matrix (covariance structure metrics)
       X_cols = ['trace_covariance', 'log_det_covariance', 'condition_number', 'largest_eigenvalue']
       X = cov_df[X_cols].values
       
       # Remove rows with invalid covariance metrics
       valid_mask = np.all(np.isfinite(X), axis=1)
       X_valid = X[valid_mask]
       cov_df_valid = cov_df[valid_mask].reset_index(drop=True)
       
       if len(X_valid) < 10:
           raise ValueError("Insufficient valid covariance observations for modeling")
       
       # Standardize predictors for numerical stability
       X_mean = np.mean(X_valid, axis=0)
       X_std = np.std(X_valid, axis=0)
       X_std = np.where(X_std < 1e-10, 1.0, X_std)  # Avoid division by zero
       X_scaled = (X_valid - X_mean) / X_std
       
       # Add intercept
       X_reg = np.column_stack([np.ones(len(X_scaled)), X_scaled])
       
       # Fit separate regressions for each distribution parameter
       models = {}
       
       for param_name in param_names:
           try:
               # Extract parameter values
               y_param = cov_df_valid[param_name].values
               
               # Remove invalid parameter values
               param_valid_mask = np.isfinite(y_param)
               if np.sum(param_valid_mask) < 10:
                   print(f"Warning: Insufficient valid values for parameter {param_name}, skipping")
                   continue
                   
               y_param_valid = y_param[param_valid_mask]
               X_reg_valid = X_reg[param_valid_mask]
               
               # Fit regression: param = β₀ + β₁×trace + β₂×log_det + β₃×condition + β₄×λₘₐₓ
               try:
                   # Regularized least squares for numerical stability
                   XTX = X_reg_valid.T @ X_reg_valid
                   XTy = X_reg_valid.T @ y_param_valid
                   ridge_lambda = 1e-6
                   XTX_reg = XTX + ridge_lambda * np.eye(XTX.shape[0])
                   beta = np.linalg.solve(XTX_reg, XTy)
               except np.linalg.LinAlgError:
                   beta = np.linalg.lstsq(X_reg_valid, y_param_valid, rcond=None)[0]
               
               # Calculate model diagnostics
               y_pred = X_reg_valid @ beta
               residuals = y_param_valid - y_pred
               residual_var = np.var(residuals, ddof=len(beta))
               r_squared = 1 - np.var(residuals) / np.var(y_param_valid) if np.var(y_param_valid) > 0 else 0
               
               # Store model for this parameter
               models[param_name] = {
                   'intercept': float(beta[0]),
                   'beta_trace': float(beta[1]),
                   'beta_log_det': float(beta[2]),
                   'beta_condition': float(beta[3]),
                   'beta_eigenvalue': float(beta[4]),
                   'residual_variance': float(residual_var),
                   'r_squared': float(r_squared),
                   'n_observations': len(y_param_valid)
               }
               
               print(f"Model for {param_name}:")
               print(f"  R²: {r_squared:.4f}")
               print(f"  Trace coef: {beta[1]:.4f}, Log-det coef: {beta[2]:.4f}")
               print(f"  Condition coef: {beta[3]:.4f}, Eigenvalue coef: {beta[4]:.4f}")
               
           except Exception as e:
               print(f"Failed to fit model for parameter {param_name}: {e}")
               continue
       
       if not models:
           raise ValueError("No successful parameter models fitted!")
       
       # Store the complete covariance model
       self.covariance_model = pd.DataFrame([{
           'model_type': 'distribution_parameter_regression',
           'distribution_family': self.dist_family,
           'n_parameters': len(models),
           'parameter_names': list(models.keys()),
           'n_windows': len(cov_df_valid),
           # Store standardization parameters for future predictions
           'X_mean_trace': float(X_mean[0]),
           'X_std_trace': float(X_std[0]),
           'X_mean_log_det': float(X_mean[1]),
           'X_std_log_det': float(X_std[1]),
           'X_mean_condition': float(X_mean[2]),
           'X_std_condition': float(X_std[2]),
           'X_mean_eigenvalue': float(X_mean[3]),
           'X_std_eigenvalue': float(X_std[3])
       }])
       
       # Store individual parameter models
       self.parameter_models = models
       
       print(f"\nCovariance model fitting complete:")
       print(f"  Distribution: {self.dist_family}")
       print(f"  Parameters modeled: {list(models.keys())}")
       print(f"  Windows used: {len(cov_df_valid)}")
       
       return self.covariance_model
    
    def _fit_linear_model(self, y, X, use_shrinkage=False):
        """Fit linear model with optional shrinkage"""
        # Add intercept term
        X_with_intercept = np.column_stack([np.ones(len(y)), X])
        
        if use_shrinkage and len(y) > len(self.predictors) + 5:
            # Apply Ledoit-Wolf shrinkage to covariance matrix
            lw = LedoitWolf()
            X_centered = X - np.mean(X, axis=0)
            cov_shrunk, _ = lw.fit(X_centered).covariance_, lw.shrinkage_
            
            # Regularized least squares solution
            XTX = X_with_intercept.T @ X_with_intercept
            XTy = X_with_intercept.T @ y
            
            # Add small ridge regularization
            ridge_lambda = 1e-4
            XTX_reg = XTX + ridge_lambda * np.eye(XTX.shape[0])
            beta = np.linalg.solve(XTX_reg, XTy)
        else:
            # Standard OLS
            beta = np.linalg.lstsq(X_with_intercept, y, rcond=None)[0]
        
        # Calculate residual variance
        y_pred = X_with_intercept @ beta
        residuals = y - y_pred
        residual_var = np.var(residuals, ddof=len(beta))
        
        return beta, residual_var

    def rolling_distributions_with_ci(self, confidence_levels=[0.50, 0.75, 0.90, 0.95]):
       """
       Generate rolling distributions with confidence intervals using covariance-based parameter prediction.
       """
       if self.rolling_distributions is None or not hasattr(self, 'parameter_models'):
           raise ValueError("Must fit rolling distributions and covariance model first!")
       
       # Get standardization parameters
       model_info = self.covariance_model.iloc[0]
       X_means = np.array([model_info['X_mean_trace'], model_info['X_mean_log_det'], 
                          model_info['X_mean_condition'], model_info['X_mean_eigenvalue']])
       X_stds = np.array([model_info['X_std_trace'], model_info['X_std_log_det'],
                         model_info['X_std_condition'], model_info['X_std_eigenvalue']])
       
       results = []
       window_days = int(self.config.window_length)
       
       for _, row in self.rolling_distributions.iterrows():
           try:
               timestamp = pd.to_datetime(row['timestamp'])
               
               # Calculate covariance structure for this window
               end_date = timestamp
               start_date = end_date - pd.Timedelta(days=window_days)
               
               window_mask = (self.data['DateTime'] >= start_date) & (self.data['DateTime'] <= end_date)
               window_predictors = self.data.loc[window_mask, self.predictors].dropna()
               
               if len(window_predictors) < 10:
                   # Use original values if covariance calculation fails
                   row_dict = row.to_dict()
                   row_dict['predicted_exceedance_probability'] = row['exceedance_probability']
                   for conf in confidence_levels:
                       row_dict[f'ci_lower_{int(conf*100)}'] = np.nan
                       row_dict[f'ci_upper_{int(conf*100)}'] = np.nan
                   results.append(row_dict)
                   continue
               
               # Calculate covariance metrics
               cov_matrix = np.cov(window_predictors.values.T, bias=False)
               trace_cov = np.trace(cov_matrix)
               det_cov = np.linalg.det(cov_matrix)
               condition_num = np.linalg.cond(cov_matrix)
               largest_eigenval = np.max(np.linalg.eigvals(cov_matrix))
               
               # Standardize predictors
               X_raw = np.array([trace_cov, np.log(max(det_cov, 1e-12)), 
                                min(condition_num, 1e12), largest_eigenval])
               X_scaled = (X_raw - X_means) / X_stds
               X_reg = np.concatenate([[1.0], X_scaled])  # Add intercept
               
               # Predict distribution parameters
               predicted_params = {}
               param_uncertainties = {}
               
               for param_name, model in self.parameter_models.items():
                   # Point prediction
                   coeffs = np.array([model['intercept'], model['beta_trace'], model['beta_log_det'],
                                    model['beta_condition'], model['beta_eigenvalue']])
                   predicted_param = X_reg @ coeffs
                   predicted_params[param_name] = predicted_param
                   
                   # Prediction uncertainty
                   param_uncertainties[param_name] = np.sqrt(model['residual_variance'])
               
               # Calculate exceedance probability from predicted parameters
               try:
                   predicted_prob = 1 - self.dist_fitter.cdf(self.config.salinity_threshold, predicted_params)
               except:
                   predicted_prob = row['exceedance_probability']  # Fallback
               
               # Store results
               row_dict = row.to_dict()
               row_dict['predicted_exceedance_probability'] = float(predicted_prob)
               
               # Add covariance metrics for diagnostics
               row_dict['trace_covariance'] = float(trace_cov)
               row_dict['log_det_covariance'] = float(np.log(max(det_cov, 1e-12)))
               row_dict['condition_number'] = float(min(condition_num, 1e12))
               row_dict['largest_eigenvalue'] = float(largest_eigenval)
               
               # Calculate confidence intervals using parameter uncertainty
               # Simple approach: use residual variance to estimate prediction intervals
               avg_residual_var = np.mean([model['residual_variance'] for model in self.parameter_models.values()])
               prediction_se = np.sqrt(avg_residual_var)
               
               for conf in confidence_levels:
                   z = stats.norm.ppf((1 + conf) / 2)
                   
                   # This is a simplified CI calculation - in practice you'd want to 
                   # propagate parameter uncertainties through the distribution properly
                   ci_factor = z * prediction_se
                   
                   # Use logit space for CI calculation to ensure [0,1] bounds
                   logit_prob = np.log(max(predicted_prob, 1e-6) / max(1 - predicted_prob, 1e-6))
                   ci_lower_logit = logit_prob - ci_factor
                   ci_upper_logit = logit_prob + ci_factor
                   
                   ci_lower = 1 / (1 + np.exp(-ci_lower_logit))
                   ci_upper = 1 / (1 + np.exp(-ci_upper_logit))
                   
                   row_dict[f'ci_lower_{int(conf*100)}'] = float(ci_lower)
                   row_dict[f'ci_upper_{int(conf*100)}'] = float(ci_upper)
               
               results.append(row_dict)
               
           except Exception as e:
               if getattr(self.config, 'debug', False):
                   print(f"Prediction failed for {row['timestamp']}: {e}")
               # Fallback to original values
               row_dict = row.to_dict()
               row_dict['predicted_exceedance_probability'] = row['exceedance_probability']
               for conf in confidence_levels:
                   row_dict[f'ci_lower_{int(conf*100)}'] = np.nan
                   row_dict[f'ci_upper_{int(conf*100)}'] = np.nan
               results.append(row_dict)
       
       return pd.DataFrame(results)
    
    def predict_future_exceedance(self, future_data, confidence_levels=[0.50, 0.75, 0.90, 0.95]):
       """
       Predict exceedance probability for future data using covariance-based parameter models.
       
       Parameters
       ----------
       future_data : pd.DataFrame
           Predictor values at future timestamps. Must include all predictors used in training.
       """
       if not hasattr(self, 'parameter_models'):
           raise ValueError("Parameter models must be fitted first.")
       
       # Get standardization parameters
       model_info = self.covariance_model.iloc[0]
       X_means = np.array([model_info['X_mean_trace'], model_info['X_mean_log_det'], 
                          model_info['X_mean_condition'], model_info['X_mean_eigenvalue']])
       X_stds = np.array([model_info['X_std_trace'], model_info['X_std_log_det'],
                         model_info['X_std_condition'], model_info['X_std_eigenvalue']])
       
       results = []
       window_days = int(self.config.window_length)
       
       for idx in future_data.index:
           try:
               # Get rolling window of future data ending at this timestamp
               end_date = pd.to_datetime(idx)
               start_date = end_date - pd.Timedelta(days=window_days)
               
               # Extract window from future_data
               window_mask = (future_data.index >= start_date) & (future_data.index <= end_date)
               window_data = future_data.loc[window_mask, self.predictors].dropna()
               
               if len(window_data) < 10:
                   row_dict = {'timestamp': idx, 'predicted_probability': np.nan}
                   for conf in confidence_levels:
                       row_dict[f'ci_lower_{int(conf*100)}'] = np.nan
                       row_dict[f'ci_upper_{int(conf*100)}'] = np.nan
                   results.append(row_dict)
                   continue
               
               # Calculate covariance metrics
               cov_matrix = np.cov(window_data.values.T, bias=False)
               trace_cov = np.trace(cov_matrix)
               det_cov = np.linalg.det(cov_matrix)
               condition_num = np.linalg.cond(cov_matrix)
               largest_eigenval = np.max(np.linalg.eigvals(cov_matrix))
               
               # Standardize and predict
               X_raw = np.array([trace_cov, np.log(max(det_cov, 1e-12)), 
                                min(condition_num, 1e12), largest_eigenval])
               X_scaled = (X_raw - X_means) / X_stds
               X_reg = np.concatenate([[1.0], X_scaled])
               
               # Predict distribution parameters
               predicted_params = {}
               for param_name, model in self.parameter_models.items():
                   coeffs = np.array([model['intercept'], model['beta_trace'], model['beta_log_det'],
                                    model['beta_condition'], model['beta_eigenvalue']])
                   predicted_params[param_name] = X_reg @ coeffs
               
               # Calculate exceedance probability
               predicted_prob = 1 - self.dist_fitter.cdf(self.config.salinity_threshold, predicted_params)
               
               # Build result
               row_dict = {
                   'timestamp': idx, 
                   'predicted_probability': float(predicted_prob),
                   'trace_covariance': float(trace_cov),
                   'condition_number': float(min(condition_num, 1e12))
               }
               
               # Add confidence intervals (simplified approach)
               avg_residual_var = np.mean([model['residual_variance'] for model in self.parameter_models.values()])
               prediction_se = np.sqrt(avg_residual_var)
               
               logit_prob = np.log(max(predicted_prob, 1e-6) / max(1 - predicted_prob, 1e-6))
               
               for conf in confidence_levels:
                   z = stats.norm.ppf((1 + conf) / 2)
                   ci_factor = z * prediction_se
                   
                   ci_lower = 1 / (1 + np.exp(-(logit_prob - ci_factor)))
                   ci_upper = 1 / (1 + np.exp(-(logit_prob + ci_factor)))
                   
                   row_dict[f'ci_lower_{int(conf*100)}'] = float(ci_lower)
                   row_dict[f'ci_upper_{int(conf*100)}'] = float(ci_upper)
               
               results.append(row_dict)
               
           except Exception as e:
               if getattr(self.config, 'debug', False):
                   print(f"Future prediction failed for {idx}: {e}")
               row_dict = {'timestamp': idx, 'predicted_probability': np.nan}
               for conf in confidence_levels:
                   row_dict[f'ci_lower_{int(conf*100)}'] = np.nan
                   row_dict[f'ci_upper_{int(conf*100)}'] = np.nan
               results.append(row_dict)
       
       return pd.DataFrame(results)

    def fit_complete_model(self):
        """Execute complete modeling pipeline"""
        print("=== ROLLING COVARIANCE MODEL FOR SALINITY MANAGEMENT ===")
        print(f"Configuration:")
        print(f"  - Distribution: {self.config.distribution_family}")
        print(f"  - Window length: {self.config.window_length} days")
        print(f"  - Salinity threshold: {self.config.salinity_threshold}")
        print(f"  - Data file: {self.config.data_csv}")
        
        # Step 1: Load and prepare data
        self.load_and_prepare_data()
        print(f"After loading data: {len(self.data)} rows")
        
        # Step 2: Fit rolling distributions (Phase 2)
        self.fit_rolling_distributions()
        print(f"After rolling distributions: {len(self.rolling_distributions)} rows, NaN count: {self.rolling_distributions.isna().sum().sum()}")
        
        # Step 3: Prepare predictor data
        self.prepare_predictor_data()
        print(f"After predictor prep: {len(self.predictor_data)} rows, NaN count: {self.predictor_data.isna().sum().sum()}")
        
        # Step 4: Fit covariance model (Phase 3)
        self.fit_covariance_model()
        print(f"After covariance model: {len(self.window_model)} rows, NaN count: {self.window_model.isna().sum().sum()}")
        
        print("\n=== MODEL FITTING COMPLETE ===")
        print(f"Summary:")
        print(f"  - Data points: {len(self.data)}")
        print(f"  - Predictors: {len(self.predictors)} ({self.predictors})")
        print(f"  - Distribution fits: {len(self.rolling_distributions)}")
        print(f"  - Covariance model periods: {len(self.window_model)}")
        
        return self


def run_rolling_model(config, confidence_levels=[0.50, 0.75, 0.90, 0.95]):
    """Main execution function with comprehensive error handling and multi-level CIs"""
    
    # Convert config to namespace if needed
    if isinstance(config, dict):
        config = dict_to_namespace(config)
    
    # Validate required parameters
    required = ['data_csv', 'salinity_col', 'salinity_threshold', 'distribution_family', 'window_length']
    missing = [p for p in required if not hasattr(config, p)]
    if missing:
        raise ValueError(f"Missing required configuration parameters: {missing}")
    
    try:
        # Initialize and fit model
        model = RollingCovarianceModel(config)
        model.fit_complete_model()
        
        # Generate rolling distributions with multiple confidence intervals
        rolling_with_ci = model.rolling_distributions_with_ci(confidence_levels=confidence_levels)
        
        # Optionally, generate future predictions if needed
        # future_predictions = model.predict_future_exceedance(future_data, confidence_levels=confidence_levels)
        
        # Calculate basic statistics
        exceedance_rate = (model.data[config.salinity_col] > config.salinity_threshold).mean()
        
        # Compile results for JSON export
        results = {
            'model_info': {
                'distribution_family': config.distribution_family,
                'window_length': config.window_length,
                'salinity_threshold': config.salinity_threshold,
                'n_predictors': len(model.predictors),
                'predictor_names': model.predictors,
                'observed_exceedance_rate': float(exceedance_rate),
                'total_observations': len(model.data)
            },
            'rolling_distributions': rolling_with_ci.to_dict('records'),  # full CI included
            'window_model': model.window_model.to_dict('records'),
            'summary': {
                'distribution_fits': len(rolling_with_ci),
                'covariance_periods': len(model.window_model),
            }
        }
        
        print(f"\n=== EXECUTION COMPLETE ===")
        print(f"Results generated successfully!")
        
        return results
        
    except Exception as e:
        print(f"Error in model execution: {e}")
        raise
