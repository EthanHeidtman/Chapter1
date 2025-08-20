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
    Rolling window distribution fitting and covariance-based risk modeling
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
        self.covariance_model = None
        
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
                # Skip failed fits silently unless in debug mode
                if getattr(self.config, 'debug', False):
                    print(f"Distribution fit failed for {end_date}: {e}")
                continue
        
        if not results:
            raise ValueError("No successful distribution fits! Check data quality and parameters.")
            
        self.rolling_distributions = pd.DataFrame(results)
        print(f"Successfully fitted {len(self.rolling_distributions)} distributions")
        
        return self.rolling_distributions
    
    def prepare_predictor_data(self):
        """Extract window-averaged predictors for covariance modeling"""
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
        """Phase 3: Fit rolling covariance model in logit space"""
        print("Phase 3: Fitting rolling covariance model...")
        
        # Align distribution results with predictor data
        dist_times = pd.to_datetime(self.rolling_distributions['timestamp'])
        dist_times_index = pd.Index(dist_times) # make dist_times an index
        pred_times = self.predictor_data.index
        common_times = dist_times_index.intersection(pred_times)
        
        if len(common_times) < 20:
            raise ValueError("Insufficient overlapping time periods for covariance modeling")
        
        # Prepare aligned data
        dist_aligned = self.rolling_distributions.set_index('timestamp').loc[common_times]
        pred_aligned = self.predictor_data.loc[common_times, self.predictors]
        
        # Transform to logit space
        eps = 1e-6
        y_probs = dist_aligned['exceedance_probability'].values
        y_probs = np.clip(y_probs, eps, 1 - eps)
        y_logit = self.logit_transform(y_probs)
        X = pred_aligned.values
        
        # Remove invalid observations
        valid_mask = np.isfinite(y_logit) & np.all(np.isfinite(X), axis=1)
        y_logit = y_logit[valid_mask]
        X = X[valid_mask]
        valid_times = common_times[valid_mask]
        
        print(f"Using {len(y_logit)} valid observations for covariance modeling")
        
        # Fit rolling covariance models
        covariance_window = getattr(self.config, 'covariance_window', 60)
        use_shrinkage = getattr(self.config, 'use_shrinkage', False)
        
        covariance_results = []
        
        for i in range(covariance_window - 1, len(valid_times)):
            # Define rolling window
            start_idx = i - covariance_window + 1
            end_idx = i + 1
            
            y_window = y_logit[start_idx:end_idx]
            X_window = X[start_idx:end_idx]
            
            try:
                # Fit linear model: y = β₀ + βᵀx + ε
                beta, residual_var = self._fit_linear_model(y_window, X_window, use_shrinkage)
                
                result = {
                    'timestamp': valid_times[i],
                    'intercept': float(beta[0]),
                    'residual_variance': float(residual_var),
                    'n_observations': len(y_window)
                }
                
                # Store coefficients for each predictor
                for j, pred_name in enumerate(self.predictors):
                    result[f'beta_{pred_name}'] = float(beta[j + 1])
                
                covariance_results.append(result)
                
            except np.linalg.LinAlgError as e:
                if getattr(self.config, 'debug', False):
                    print(f"Singular matrix at {valid_times[i]}: {e}")
                continue
        
        if not covariance_results:
            raise ValueError("No successful covariance model fits!")
            
        self.covariance_model = pd.DataFrame(covariance_results)
        print(f"Fitted covariance models for {len(self.covariance_model)} time periods")
        
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
        Generate rolling distributions with multiple confidence intervals for every timestamp.
        
        Parameters
        ----------
        confidence_levels : list of float
            Confidence levels to compute (e.g., [0.5, 0.75, 0.9, 0.95]).
        
        Returns
        -------
        DataFrame
            Original rolling distribution columns plus:
            - exceedance_probability
            - ci_lower_{level}, ci_upper_{level} for each confidence level
        """
        if self.rolling_distributions is None or self.covariance_model is None:
            raise ValueError("Must fit rolling distributions and covariance model first!")
    
        # Merge rolling distributions with covariance model on timestamp
        merged = self.rolling_distributions.merge(
            self.covariance_model,
            on='timestamp',
            how='left',
            suffixes=('', '_cov')
        )
    
        results = []
    
        for _, row in merged.iterrows():
            try:
                # Extract predictors
                X_current = [row[pred] if pred in row else 0 for pred in self.predictors]
    
                # Linear predictor in logit space
                intercept = row['intercept']
                coefficients = [row[f'beta_{pred}'] for pred in self.predictors]
                y_logit = intercept + np.dot(coefficients, X_current)
    
                # Predicted exceedance probability
                p = self.inv_logit_transform(y_logit)
    
                # Confidence intervals for each level
                row_dict = row.to_dict()
                row_dict['exceedance_probability'] = p
    
                residual_var = row['residual_variance']
                y_se = np.sqrt(residual_var)
    
                for conf in confidence_levels:
                    z = stats.norm.ppf((1 + conf) / 2)
                    ci_lower = self.inv_logit_transform(y_logit - z * y_se)
                    ci_upper = self.inv_logit_transform(y_logit + z * y_se)
                    row_dict[f'ci_lower_{int(conf*100)}'] = ci_lower
                    row_dict[f'ci_upper_{int(conf*100)}'] = ci_upper
    
                results.append(row_dict)
    
            except Exception as e:
                # Keep original row but set NA for probabilities if something fails
                row_dict = row.to_dict()
                row_dict['exceedance_probability'] = np.nan
                for conf in confidence_levels:
                    row_dict[f'ci_lower_{int(conf*100)}'] = np.nan
                    row_dict[f'ci_upper_{int(conf*100)}'] = np.nan
                results.append(row_dict)
    
        return pd.DataFrame(results)
    
    def predict_future_exceedance(self, future_data, confidence_levels=[0.50, 0.75, 0.90, 0.95]):
        """
        Predict exceedance probability for new/future predictor data.
        
        Parameters
        ----------
        future_data : pd.DataFrame
            Predictor values at future timestamps. Must include all predictors.
        confidence_levels : list of float
            Confidence levels for CI.
        
        Returns
        -------
        pd.DataFrame
            Each row: timestamp, predicted probability, ci_lower_{level}, ci_upper_{level}, confidence_level
        """
        if self.covariance_model is None:
            raise ValueError("Covariance model must be fitted first.")
    
        results = []
        latest_model = self.covariance_model.iloc[-1]  # Use most recent fitted coefficients
    
        for idx in future_data.index:
            row = future_data.loc[idx]
            X_current = [row[pred] for pred in self.predictors]
    
            intercept = latest_model['intercept']
            coefficients = [latest_model[f'beta_{pred}'] for pred in self.predictors]
            y_logit = intercept + np.dot(coefficients, X_current)
            p = self.inv_logit_transform(y_logit)
    
            residual_var = latest_model['residual_variance']
            y_se = np.sqrt(residual_var)
    
            row_dict = {'timestamp': idx, 'predicted_probability': p}
    
            for conf in confidence_levels:
                z = stats.norm.ppf((1 + conf) / 2)
                row_dict[f'ci_lower_{int(conf*100)}'] = self.inv_logit_transform(y_logit - z * y_se)
                row_dict[f'ci_upper_{int(conf*100)}'] = self.inv_logit_transform(y_logit + z * y_se)
    
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
        print(f"After covariance model: {len(self.covariance_model)} rows, NaN count: {self.covariance_model.isna().sum().sum()}")
        
        print("\n=== MODEL FITTING COMPLETE ===")
        print(f"Summary:")
        print(f"  - Data points: {len(self.data)}")
        print(f"  - Predictors: {len(self.predictors)} ({self.predictors})")
        print(f"  - Distribution fits: {len(self.rolling_distributions)}")
        print(f"  - Covariance model periods: {len(self.covariance_model)}")
        
        return self


def run_rolling_covariance_model(config, confidence_levels=[0.50, 0.75, 0.90, 0.95]):
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
            'covariance_model': model.covariance_model.to_dict('records'),
            'summary': {
                'distribution_fits': len(rolling_with_ci),
                'covariance_periods': len(model.covariance_model),
            }
        }
        
        print(f"\n=== EXECUTION COMPLETE ===")
        print(f"Results generated successfully!")
        
        return results
        
    except Exception as e:
        print(f"Error in model execution: {e}")
        raise
