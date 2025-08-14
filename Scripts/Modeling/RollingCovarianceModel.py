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

# Load functions from other python files
from Distributions import *

def dict_to_namespace(d):
    """Convert nested dictionary to SimpleNamespace for easy attribute access"""
    if not isinstance(d, dict):
        return d
    return SimpleNamespace(**{k: dict_to_namespace(v) for k,v in d.items()})

# Map distribution names to classes
DISTS = {
    'burr': Burr(),
    'gpd': GPD(),
    'gengamma': GenGamma(), 
    'lognormal': Lognormal(),
    'loglogistic': Loglogistic(),
    'gamma': Gamma()
}

class RollingCovarianceModel:
    """
    Main class for rolling window distribution fitting and covariance-based risk modeling
    """
    def __init__(self, config):
        self.config = config
        
        # Validate distribution family
        if config.distribution_family not in DISTS:
            raise ValueError(f"Unsupported distribution: {config.distribution_family}")
        
        self.dist_family = config.distribution_family
        self.dist_fitter = DISTS[self.dist_family]
        
        # Storage for fitted components
        self.rolling_distributions = None    # distribution parameters per window
        self.covariance_model = None         # β coefficients and covariances
        self.predictor_data = None           # window-mean predictors
        
    def load_data(self, config):
        """Load CSV and prepare data with datetime parsing"""
        try:
            data = pd.read_csv(self.config.data_csv)
            
            if 'DateTime' in data.columns:
                data['DateTime'] = data['DateTime'].astype(str).str.strip()
                data['DateTime'] = data['DateTime'].apply(
                    lambda x: x if ':' in x else x + ' 00:00:00'
                )
                data['DateTime'] = pd.to_datetime(data['DateTime'], errors='coerce')
                data = data.dropna(subset=['DateTime'])
                data = data.sort_values('DateTime').reset_index(drop=True)
            
            # Load predictors from config (assuming they're directly specified)
            if hasattr(self.config, 'predictors'):
                predictors = self.config.predictors
            else:
                # Fallback - use all numeric columns except DateTime and Salinity
                numeric_cols = data.select_dtypes(include=[np.number]).columns
                predictors = [col for col in numeric_cols if col != self.config.salinity_col]
            
            available_predictors = [p for p in predictors if p in data.columns]
            if len(available_predictors) != len(predictors):
                missing = set(predictors) - set(available_predictors)
                print(f"Warning: Missing predictors in data: {missing}")
            
            print(f"Data loaded: {data.shape[0]} rows, {len(available_predictors)} predictors available")
            return data, available_predictors
        
        except Exception as e:
            print(f"Error loading data: {e}")
            raise
        
    def clean_data(self, df):
        """Clean data by dropping rows with all missing and ensuring numeric types"""
        df = df.dropna(how='all').copy()
        
        # Convert DateTime column explicitly
        if 'DateTime' in df.columns:
            df['DateTime'] = pd.to_datetime(df['DateTime'], errors='coerce')
        
        # Convert other columns to numeric where possible
        cols_to_convert = [c for c in df.columns if c != 'DateTime']
        df[cols_to_convert] = df[cols_to_convert].apply(pd.to_numeric, errors='coerce')
    
        return df

    def fit_rolling_distributions(self, data):
        """
        Phase 2: Fit parametric distributions to salinity within each rolling window
        """
        print(f"Phase 2: Fitting {self.dist_family} distributions on rolling windows...")
        
        window_size = int(self.config.window_length)
        threshold = float(self.config.salinity_threshold)
        results_list = []
        
        for i in range(len(data)):
            end_date = data.iloc[i]['DateTime']
            start_date = end_date - pd.Timedelta(days=window_size)
            
            # Get salinity data in window
            window_data = data[(data['DateTime'] >= start_date) & (data['DateTime'] <= end_date)]
            salinity_values = window_data[self.config.salinity_col].dropna()
            
            if len(salinity_values) < self.config.min_observations_per_window:
                continue
                
            try:
                # Fit distribution by MLE
                params = self.dist_fitter(salinity_values.values)
                
                # Compute exceedance probability P(S > threshold)
                if self.dist_family == 'lognormal':
                    # params = (s, loc, scale) for lognorm
                    exceedance_prob = 1 - self.dist_obj.cdf(threshold, *params)
                else:
                    # Standard scipy format
                    exceedance_prob = 1 - self.dist_obj.cdf(threshold, *params)
                
                # Store result
                result = {
                    'timestamp': end_date,
                    'n_observations': len(salinity_values),
                    'exceedance_probability': float(exceedance_prob),
                    'distribution_family': self.dist_family
                }
                
                # Store distribution parameters
                param_names = [f'param_{j}' for j in range(len(params))]
                for name, value in zip(param_names, params):
                    result[name] = float(value)
                
                results_list.append(result)
                
            except Exception as e:
                print(f"Warning: Failed to fit distribution for window ending {end_date}: {e}")
                continue
        
        if not results_list:
            raise ValueError("No valid distribution fits found!")
            
        self.rolling_distributions = pd.DataFrame(results_list)
        print(f"  Fitted distributions for {len(self.rolling_distributions)} time periods")
        
        return self.rolling_distributions
    
    def prepare_predictor_data(self, data, predictors):
        """
        Extract window-mean predictors for covariance modeling
        """
        print("Preparing window-mean predictor data...")
        
        window_size = int(self.config.window_length)
        predictor_data_list = []
    
        for i in range(len(data)):
            end_date = data.iloc[i]['DateTime']
            start_date = end_date - pd.Timedelta(days=window_size)
            
            window_data = data[(data['DateTime'] >= start_date) & (data['DateTime'] <= end_date)]
    
            if len(window_data) < self.config.min_observations_per_window:
                continue
    
            row = {'timestamp': end_date}
    
            for predictor in predictors:
                if predictor not in window_data.columns:
                    continue
                
                pred_values = window_data[predictor].dropna()
                if len(pred_values) < 3:
                    row[predictor] = np.nan
                    continue
                
                # Calculate window mean
                row[predictor] = float(np.mean(pred_values.values))
    
            predictor_data_list.append(row)
    
        self.predictor_data = pd.DataFrame(predictor_data_list)
        self.predictor_data['timestamp'] = pd.to_datetime(self.predictor_data['timestamp'])
        self.predictor_data = self.predictor_data.set_index('timestamp').sort_index()
    
        print(f"Prepared predictor data for {len(self.predictor_data)} time periods")
        return self.predictor_data.dropna()

    def logit_transform(self, probabilities):
        """Transform probabilities to logit space"""
        # Clip to avoid infinite values
        p_clipped = np.clip(probabilities, 1e-6, 1 - 1e-6)
        return np.log(p_clipped / (1 - p_clipped))

    def inv_logit_transform(self, logit_values):
        """Inverse logit transform"""
        return 1 / (1 + np.exp(-logit_values))

    def fit_rolling_covariance_model(self, predictors):
        """
        Phase 3: Build rolling covariance model in logit space
        """
        print("Phase 3: Fitting rolling covariance model in logit space...")
        
        # Merge distribution results with predictor data
        common_times = pd.to_datetime(self.rolling_distributions['timestamp']).intersection(
            self.predictor_data.index
        )
        
        dist_data = self.rolling_distributions[
            self.rolling_distributions['timestamp'].isin(common_times)
        ].copy()
        dist_data = dist_data.set_index('timestamp').loc[common_times]
        
        pred_data = self.predictor_data.loc[common_times, predictors]
        
        # Transform exceedance probabilities to logit space
        y_probs = dist_data['exceedance_probability'].values
        y_logit = self.logit_transform(y_probs)
        
        # Prepare predictor matrix
        X = pred_data.values
        
        # Remove any rows with NaN values
        valid_mask = ~(np.isnan(y_logit) | np.any(np.isnan(X), axis=1))
        y_logit = y_logit[valid_mask]
        X = X[valid_mask, :]
        valid_times = common_times[valid_mask]
        
        if len(y_logit) < 10:
            raise ValueError("Insufficient valid data for covariance modeling")
        
        print(f"  Using {len(y_logit)} valid observations for covariance model")
        
        # Fit rolling covariance model
        covariance_window = getattr(self.config, 'covariance_window', 60)
        results_list = []
        
        for i in range(len(valid_times)):
            if i < covariance_window:
                continue
                
            # Rolling window of data
            start_idx = max(0, i - covariance_window + 1)
            end_idx = i + 1
            
            y_window = y_logit[start_idx:end_idx]
            X_window = X[start_idx:end_idx, :]
            
            # Calculate rolling moments
            X_mean = np.mean(X_window, axis=0)
            y_mean = np.mean(y_window)
            
            # Center the data
            X_centered = X_window - X_mean
            y_centered = y_window - y_mean
            
            # Calculate covariances
            Sigma_XX = np.dot(X_centered.T, X_centered) / (len(y_window) - 1)
            Sigma_Xy = np.dot(X_centered.T, y_centered) / (len(y_window) - 1)
            
            # Apply shrinkage if requested
            if getattr(self.config, 'use_shrinkage', False):
                lw = LedoitWolf()
                Sigma_XX, _ = lw.fit(X_centered).covariance_, lw.shrinkage_
            
            try:
                # Analytical solution for coefficients
                beta = np.linalg.solve(Sigma_XX, Sigma_Xy)
                beta_0 = y_mean - np.dot(beta, X_mean)
                
                # Residual variance for confidence intervals
                y_pred = beta_0 + np.dot(X_window, beta)
                residuals = y_window - y_pred
                residual_var = np.var(residuals, ddof=len(beta))
                
                # Store results
                result = {
                    'timestamp': valid_times[i],
                    'beta_0': float(beta_0),
                    'residual_variance': float(residual_var),
                    'n_observations': int(len(y_window))
                }
                
                for j, pred_name in enumerate(predictors):
                    result[f'beta_{pred_name}'] = float(beta[j])
                
                results_list.append(result)
                
            except np.linalg.LinAlgError:
                print(f"Warning: Singular matrix at time {valid_times[i]}")
                continue
        
        if not results_list:
            raise ValueError("No valid covariance model fits!")
        
        self.covariance_model = pd.DataFrame(results_list)
        print(f"  Fitted covariance model for {len(self.covariance_model)} time periods")
        
        return self.covariance_model

    def predict_exceedance_risk(self, current_data, predictors, confidence_level=0.95):
        """
        Predict exceedance risk using the covariance model with confidence intervals
        """
        if self.covariance_model is None:
            raise ValueError("Covariance model not fitted yet!")
            
        predictions = []
        
        for idx in current_data.index:
            try:
                # Get current predictor values
                X_current = np.array([current_data.loc[idx, pred] for pred in predictors])
                
                # Find most recent covariance model parameters
                recent_model = self.covariance_model.iloc[-1]  # Use most recent
                
                # Extract coefficients
                beta_0 = recent_model['beta_0']
                beta = np.array([recent_model[f'beta_{pred}'] for pred in predictors])
                residual_var = recent_model['residual_variance']
                
                # Predict in logit space
                y_logit_pred = beta_0 + np.dot(beta, X_current)
                
                # Convert to probability
                p_pred = self.inv_logit_transform(y_logit_pred)
                
                # Calculate confidence interval (approximate)
                z_score = stats.norm.ppf((1 + confidence_level) / 2)
                y_logit_se = np.sqrt(residual_var)
                
                y_logit_lower = y_logit_pred - z_score * y_logit_se
                y_logit_upper = y_logit_pred + z_score * y_logit_se
                
                p_lower = self.inv_logit_transform(y_logit_lower)
                p_upper = self.inv_logit_transform(y_logit_upper)
                
                predictions.append({
                    'timestamp': idx,
                    'predicted_probability': float(p_pred),
                    'ci_lower': float(p_lower),
                    'ci_upper': float(p_upper)
                })
                
            except Exception as e:
                print(f"Warning: Prediction failed for index {idx}: {e}")
                predictions.append({
                    'timestamp': idx,
                    'predicted_probability': np.nan,
                    'ci_lower': np.nan,
                    'ci_upper': np.nan
                })
        
        return pd.DataFrame(predictions)

    def solve_minimum_release(self, current_predictors, discharge_predictor, 
                            risk_tolerance=0.05, discharge_range=None):
        """
        Solve for minimum discharge that keeps exceedance risk ≤ risk_tolerance
        """
        if self.covariance_model is None:
            raise ValueError("Covariance model not fitted yet!")
            
        if discharge_predictor not in current_predictors.index:
            raise ValueError(f"Discharge predictor '{discharge_predictor}' not found")
            
        # Get recent model parameters
        recent_model = self.covariance_model.iloc[-1]
        beta_0 = recent_model['beta_0']
        beta_discharge = recent_model[f'beta_{discharge_predictor}']
        
        # Set discharge range if not provided
        if discharge_range is None:
            current_discharge = current_predictors[discharge_predictor]
            discharge_range = (current_discharge * 0.5, current_discharge * 3.0)
        
        def risk_function(discharge):
            # Update predictors with test discharge
            test_predictors = current_predictors.copy()
            test_predictors[discharge_predictor] = discharge
            
            # Calculate risk
            beta = np.array([recent_model[f'beta_{pred}'] for pred in current_predictors.index])
            y_logit = beta_0 + np.dot(beta, test_predictors.values)
            risk = self.inv_logit_transform(y_logit)
            
            return risk - risk_tolerance
        
        try:
            # Find minimum discharge where risk ≤ tolerance
            result = minimize_scalar(lambda q: abs(risk_function(q)), 
                                   bounds=discharge_range, method='bounded')
            
            if result.success:
                return float(result.x)
            else:
                return discharge_range[1]  # Return max if optimization fails
                
        except Exception as e:
            print(f"Warning: Minimum release calculation failed: {e}")
            return discharge_range[1]

    def fit_full_model(self, data, predictors):
        """
        Complete model fitting pipeline for Phases 2 & 3
        """
        print("=== ROLLING COVARIANCE MODEL FOR SALINITY MANAGEMENT ===")
        print(f"Distribution family: {self.dist_family}")
        print(f"Window length: {self.config.window_length} days")
        print(f"Predictors: {predictors}")
    
        # Step 1: Clean and prepare data
        clean_df = self.clean_data(data)
    
        # Step 2: Phase 2 - Fit rolling distributions
        self.fit_rolling_distributions(clean_df)
    
        # Step 3: Prepare predictor data
        self.prepare_predictor_data(clean_df, predictors)
    
        # Step 4: Phase 3 - Fit rolling covariance model
        self.fit_rolling_covariance_model(predictors)
    
        print(f"\n=== MODEL FITTING COMPLETE ===")
        print(f"- Distribution fits: {len(self.rolling_distributions)} time periods")
        print(f"- Predictor data: {len(self.predictor_data)} periods")  
        print(f"- Covariance model: {len(self.covariance_model)} periods")
    
        return self

def run_rolling_covariance_model(config):
    """
    Main execution function with error handling and detailed output
    """
    if isinstance(config, dict):
        config = dict_to_namespace(config)

    # Validate required config parameters
    required_params = ['data_csv', 'salinity_col', 'salinity_threshold', 
                      'distribution_family', 'window_length']
    missing_params = [p for p in required_params if not hasattr(config, p)]
    if missing_params:
        raise ValueError(f"Missing required config parameters: {missing_params}")

    # Set defaults for optional parameters
    if not hasattr(config, 'min_observations_per_window'):
        config.min_observations_per_window = 10
    if not hasattr(config, 'covariance_window'):
        config.covariance_window = 60
    if not hasattr(config, 'use_shrinkage'):
        config.use_shrinkage = False
    if not hasattr(config, 'risk_tolerance'):
        config.risk_tolerance = 0.05

    try:
        # Initialize and fit model
        model = RollingCovarianceModel(config)
        
        # Load data
        data, predictors = model.load_data(config)
    
        # Create binary target for evaluation
        data['exceed_threshold'] = (data[config.salinity_col] > config.salinity_threshold).astype(int)
        exceedance_rate = data['exceed_threshold'].mean()
        print(f"Threshold ({config.salinity_threshold}) exceedance rate: {exceedance_rate:.3f}")

        # Fit full model
        model.fit_full_model(data, predictors)

        # Generate predictions
        print("Generating predictions...")
        data_indexed = data.set_index('DateTime')
        predictions_df = model.predict_exceedance_risk(data_indexed, predictors)
        
        # Calculate minimum releases if discharge predictor available
        discharge_predictors = [p for p in predictors 
                              if any(k in p.lower() for k in ['discharge', 'inflow'])]
        
        min_releases = None
        if discharge_predictors:
            discharge_pred = discharge_predictors[0]  # Use first discharge predictor
            print(f"Calculating minimum releases using {discharge_pred}...")
            
            min_releases = []
            for idx in data_indexed.index:
                current_preds = data_indexed.loc[idx, predictors]
                q_min = model.solve_minimum_release(
                    current_preds, discharge_pred, config.risk_tolerance
                )
                min_releases.append({
                    'timestamp': idx,
                    'minimum_release': q_min
                })
            
            min_releases = pd.DataFrame(min_releases)

        # Prepare comprehensive results for R analysis
        results = {
            'model_info': {
                'distribution_family': config.distribution_family,
                'window_length': config.window_length,
                'covariance_window': config.covariance_window,
                'salinity_threshold': config.salinity_threshold,
                'risk_tolerance': config.risk_tolerance,
                'n_predictors': len(predictors),
                'predictor_names': predictors,
                'exceedance_rate': float(exceedance_rate)
            },
            'rolling_distributions': model.rolling_distributions.to_dict('records'),
            'covariance_model': model.covariance_model.to_dict('records'),
            'predictions': predictions_df.to_dict('records'),
            'minimum_releases': min_releases.to_dict('records') if min_releases is not None else None,
            'data_summary': {
                'total_observations': len(data),
                'distribution_fits': len(model.rolling_distributions),
                'covariance_fits': len(model.covariance_model),
                'prediction_periods': len(predictions_df)
            }
        }

        print(f"\n=== RESULTS SUMMARY ===")
        print(f"Distribution fits: {results['data_summary']['distribution_fits']}")
        print(f"Covariance model periods: {results['data_summary']['covariance_fits']}")
        print(f"Predictions generated: {results['data_summary']['prediction_periods']}")
        if min_releases is not None:
            print(f"Minimum releases calculated: {len(min_releases)}")

        return results

    except Exception as e:
        print(f"Error in model execution: {e}")
        raise
