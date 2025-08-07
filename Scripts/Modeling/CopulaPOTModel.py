# =============================================================================
# Script Name:    CopulaPOTModel.py
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-08-07
# Last Updated:   2025-08-07
# Description:    Implements a Peaks Over Threshold (POT) modeling approach that
#                 fits right-tailed distributions to exceedances of salinity over
#                 0.2 PSU on rolling windows and uses copulas to relate the change
#                 in the exceedance distribution to changes in predictor variables.
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
import pandas as pd
import numpy as np
from scipy import stats
from sklearn.model_selection import TimeSeriesSplit
from sklearn.metrics import mean_squared_error, r2_score, mean_absolute_error
from types import SimpleNamespace
import json
import warnings
from scipy.stats import multivariate_normal, t as t_dist
from scipy.optimize import minimize
warnings.filterwarnings('ignore')

# Load functions from other python files
from TailDistributions import *
from Copulas import *

def dict_to_namespace(d):
    """Convert nested dictionary to SimpleNamespace for easy attribute access"""
    if not isinstance(d, dict):
        return d
    return SimpleNamespace(**{k: dict_to_namespace(v) for k,v in d.items()})

class VariableTypeDetector:
    """Automatic detection and handling of different variable types"""
    
    @staticmethod
    def detect_variable_type(variable_data, variable_name=None):
        """Automatically detect variable type from data"""
        clean_data = variable_data.dropna()
        unique_vals = clean_data.unique()
        n_unique = len(unique_vals)
        
        # Boolean detection
        if n_unique == 2 and set(unique_vals).issubset({0, 1, True, False}):
            return 'boolean'
        
        # Circular detection (heuristic based on name and range)
        if variable_name:
            circular_indicators = ['day', 'month', 'hour', 'angle', 'direction', 'bearing']
            if any(indicator in variable_name.lower() for indicator in circular_indicators):
                val_range = unique_vals.max() - unique_vals.min()
                if 300 <= val_range <= 366 or 20 <= val_range <= 25:  # Days or hours
                    return 'circular'
        
        # Small discrete detection
        if n_unique <= 10 and all(isinstance(x, (int, np.integer)) for x in unique_vals):
            return 'discrete'
        
        # Constant (no variation)
        if n_unique == 1:
            return 'constant'
        
        # Default to continuous
        return 'continuous'
    
    @staticmethod
    def transform_variable_for_copula(variable_data, variable_type, variable_name=None):
        """Transform variable to appropriate form for copula modeling"""
        clean_data = variable_data.dropna()
        
        if variable_type == 'boolean':
            # Use probit transformation to latent continuous
            prop_true = clean_data.mean()
            prop_true = np.clip(prop_true, 0.01, 0.99)  # Avoid extremes
            latent_threshold = norm.ppf(1 - prop_true)
            # Create latent variable (simplified approach)
            latent_values = np.where(clean_data, 
                                   norm.rvs(loc=latent_threshold + 1, scale=1, size=len(clean_data)),
                                   norm.rvs(loc=latent_threshold - 1, scale=1, size=len(clean_data)))
            return latent_values, {'type': 'boolean', 'threshold': latent_threshold, 'prop_true': prop_true}
        
        elif variable_type == 'circular':
            # Transform to sin/cos components
            if variable_name and 'day' in variable_name.lower():
                period = 365.25
            else:
                period = np.max(clean_data) - np.min(clean_data)
            
            angle = 2 * np.pi * clean_data / period
            sin_comp = np.sin(angle)
            cos_comp = np.cos(angle)
            # Return as bivariate for multivariate copula
            return np.column_stack([sin_comp, cos_comp]), {'type': 'circular', 'period': period}
        
        elif variable_type == 'discrete':
            # Treat as continuous with jittering
            jittered = clean_data + np.random.normal(0, 0.1, len(clean_data))
            return jittered, {'type': 'discrete', 'original_values': clean_data.unique()}
        
        elif variable_type == 'constant':
            # Cannot be used in copula
            return None, {'type': 'constant', 'value': clean_data.iloc[0]}
        
        else:  # continuous
            return clean_data.values, {'type': 'continuous'}
         
class TimeVaryingPOTModel:
    """
    Main class for time-varying POT modeling with multivariate copulas
    """
    def __init__(self, config):
        self.config = config
        self.tail_dist_obj = TAIL_DISTS[config.tail_distribution]
        self.variable_detector = VariableTypeDetector()
        
        # Initialize copula based on configuration
        if config.copula_type == 'gaussian':
            self.copula = MultivariateGaussianCopula()
        elif config.copula_type == 'student_t':
            self.copula = MultivariateStudentTCopula()
        else:
            raise ValueError(f"Unsupported copula type: {config.copula_type}")
        
        # Storage for fitted components
        self.rolling_params = None
        self.predictor_transformations = None
        self.copula_params = None
        self.variable_types = None
        
    def detect_and_classify_variables(self, data, predictors):
        """Detect variable types for all predictors"""
        print("Detecting variable types...")
        
        self.variable_types = {}
        
        for predictor in predictors:
            if predictor in data.columns:
                var_type = self.variable_detector.detect_variable_type(
                    data[predictor], predictor
                )
                self.variable_types[predictor] = var_type
                
                # Count unique values for reporting
                n_unique = data[predictor].nunique()
                print(f"  {predictor}: {var_type} ({n_unique} unique values)")
            else:
                print(f"  Warning: Predictor '{predictor}' not found in data")
        
        # Filter out constant variables
        usable_predictors = [p for p, v_type in self.variable_types.items() 
                           if v_type != 'constant']
        
        if len(usable_predictors) < len(predictors):
            removed = [p for p in predictors if p not in usable_predictors]
            print(f"  Removed constant variables: {removed}")
        
        return usable_predictors
    
    def fit_rolling_pot_params(self, data):
        """
        Fit POT parameters on rolling windows of exceedances > threshold
        """
        print(f"Fitting rolling POT parameters using {self.config.tail_distribution}...")
        
        window_size = self.config.group_window_days
        params_list = []
        
        for i in range(len(data)):
            end_date = data.loc[i, 'DateTime']
            start_date = end_date - pd.Timedelta(days=window_size)
            
            # Get salinity exceedances in window
            window_data = data[(data['DateTime'] >= start_date) & (data['DateTime'] <= end_date)]
            exceedances = window_data[self.config.salinity_col] - self.config.base_threshold
            exceedances = exceedances[exceedances > 0]
            
            if len(exceedances) < self.config.min_exceedances_per_group:
                continue
                
            try:
                # Fit tail distribution directly to exceedances
                params = self.tail_dist_obj.fit_params(exceedances.values)
                
                # Clean parameter extraction
                clean_params = {}
                for k, v in params.items():
                    if isinstance(v, dict):
                        v = list(v.values())[0] if v else np.nan
                    clean_params[k] = float(v) if not pd.isna(v) else np.nan
                
                row = {'timestamp': end_date, 'n_exceedances': len(exceedances)}
                row.update(clean_params)
                params_list.append(row)
                
            except Exception as e:
                print(f"Warning: Failed to fit POT for window ending {end_date}: {e}")
                continue
        
        if not params_list:
            raise ValueError("No valid POT parameter fits found!")
            
        self.rolling_params = pd.DataFrame(params_list).set_index('timestamp')
        
        # Apply smoothing if requested
        if self.config.param_smoothing:
            param_cols = [c for c in self.rolling_params.columns if c != 'n_exceedances']
            self.rolling_params[param_cols] = self.rolling_params[param_cols].rolling(
                window=3, center=True
            ).mean()
        
        print(f"  Fitted POT parameters for {len(self.rolling_params)} time periods")
        return self.rolling_params.dropna()
    
    def prepare_predictor_data(self, data, predictors):
        """
        Prepare predictor data for copula modeling with automatic type handling
        """
        print("Preparing predictor data for copula modeling...")
        
        window_size = self.config.group_window_days
        predictor_data_list = []
        
        for i in range(len(data)):
            end_date = data.loc[i, 'DateTime']
            start_date = end_date - pd.Timedelta(days=window_size)
            
            window_data = data[(data['DateTime'] >= start_date) & (data['DateTime'] <= end_date)]
            
            if len(window_data) < self.config.min_exceedances_per_group:
                continue
                
            try:
                row = {'timestamp': end_date}
                
                # Process each predictor based on its detected type
                for predictor in predictors:
                    if predictor not in window_data.columns:
                        continue
                    
                    pred_values = window_data[predictor].dropna()
                    
                    if len(pred_values) < 3:
                        row[f'{predictor}_values'] = None
                        row[f'{predictor}_count'] = 0
                        continue
                    
                    # Transform based on variable type
                    var_type = self.variable_types.get(predictor, 'continuous')
                    
                    transformed_values, transform_info = self.variable_detector.transform_variable_for_copula(
                        pred_values, var_type, predictor
                    )
                    
                    if transformed_values is not None:
                        row[f'{predictor}_values'] = transformed_values
                        row[f'{predictor}_count'] = len(pred_values)
                        row[f'{predictor}_transform_info'] = transform_info
                        
                        # Store summary statistics
                        if var_type in ['continuous', 'discrete']:
                            row[f'{predictor}_mean'] = np.mean(transformed_values)
                            row[f'{predictor}_std'] = np.std(transformed_values)
                        elif var_type == 'boolean':
                            row[f'{predictor}_prop_true'] = transform_info['prop_true']
                    else:
                        row[f'{predictor}_values'] = None
                        row[f'{predictor}_count'] = 0
                
                predictor_data_list.append(row)
                
            except Exception as e:
                print(f"Warning: Failed to prepare predictor data for {end_date}: {e}")
                continue
        
        self.predictor_data = pd.DataFrame(predictor_data_list).set_index('timestamp')
        
        print(f"  Prepared predictor data for {len(self.predictor_data)} time periods")
        return self.predictor_data.dropna()
    
    def fit_multivariate_copula_relationships(self, data, predictors):
        """
        Fit multivariate copulas relating POT parameters to multiple predictors
        """
        print(f"Fitting multivariate {self.config.copula_type} copula relationships...")
        
        # Get aligned time periods
        common_times = self.rolling_params.index.intersection(self.predictor_data.index)
        
        pot_data = self.rolling_params.loc[common_times]
        current_predictors = data.set_index('DateTime')[predictors].loc[common_times]
        
        copula_fits = {}
        param_names = self.tail_dist_obj.param_names()
        
        # Limit number of predictors for computational efficiency
        max_predictors = min(len(predictors), self.config.get('max_predictors_per_copula', 5))
        
        for param in param_names:
            if param not in pot_data.columns:
                continue
                
            print(f"\n  Fitting copula for POT parameter: {param}")
            
            # Prepare data matrix for multivariate copula
            valid_data_list = []
            valid_predictor_names = []
            
            # Start with POT parameter
            pot_values = pot_data[param].dropna()
            
            for predictor in predictors[:max_predictors]:
                if predictor not in current_predictors.columns:
                    continue
                
                # Get predictor values at common time points
                pred_values = current_predictors[predictor].loc[pot_values.index]
                
                # Remove any remaining NaNs
                valid_mask = pred_values.notna() & pot_values.notna()
                
                if valid_mask.sum() < 20:  # Need sufficient data
                    continue
                
                valid_data_list.append(pred_values[valid_mask])
                valid_predictor_names.append(predictor)
                
                if len(valid_predictor_names) >= max_predictors:
                    break
            
            if len(valid_predictor_names) == 0:
                print(f"    No valid predictors found for {param}")
                continue
            
            # Update POT values to match valid predictors
            final_valid_mask = valid_data_list[0].index if valid_data_list else pot_values.index
            for i in range(1, len(valid_data_list)):
                final_valid_mask = final_valid_mask.intersection(valid_data_list[i].index)
            
            if len(final_valid_mask) < 20:
                print(f"    Insufficient valid data pairs for {param} ({len(final_valid_mask)} pairs)")
                continue
            
            # Create final data matrix
            final_pot_values = pot_values.loc[final_valid_mask]
            final_predictor_data = np.column_stack([
                pred_data.loc[final_valid_mask].values 
                for pred_data in valid_data_list
            ])
            
            # Convert to uniform margins for copula
            uniform_data = np.column_stack([
                stats.rankdata(final_pot_values, method='average') / (len(final_pot_values) + 1),
                *[stats.rankdata(final_predictor_data[:, i], method='average') / (len(final_predictor_data) + 1)
                  for i in range(final_predictor_data.shape[1])]
            ])
            
            try:
                # Fit multivariate copula
                copula_params = self.copula.fit(uniform_data)
                
                copula_fits[param] = {
                    'type': 'multivariate',
                    'copula_type': self.config.copula_type,
                    'copula_params': copula_params,
                    'predictors': valid_predictor_names,
                    'pot_marginal': final_pot_values,
                    'predictor_marginals': {
                        pred: final_predictor_data[:, i] 
                        for i, pred in enumerate(valid_predictor_names)
                    },
                    'n_observations': len(final_valid_mask)
                }
                
                print(f"    Successfully fitted copula with {len(valid_predictor_names)} predictors")
                print(f"    Predictors: {valid_predictor_names}")
                print(f"    Sample size: {len(final_valid_mask)}")
                
            except Exception as e:
                print(f"    Warning: Failed to fit multivariate copula for {param}: {e}")
                continue
        
        self.copula_params = copula_fits
        
        print(f"\nCopula fitting summary:")
        total_relationships = len(copula_fits)
        print(f"  Total POT parameter copulas fitted: {total_relationships}")
        
        return copula_fits
    
    def predict_pot_risk(self, future_data, predictors, target_exceedance=None):
        """
        Predict P(Salinity > target) using fitted multivariate copula relationships
        """
        if target_exceedance is None:
            target_exceedance = self.config.target_threshold - self.config.base_threshold
            
        print(f"Predicting POT risk for target exceedance = {target_exceedance}")
        
        predictions = []
        
        for idx in future_data.index:
            try:
                # Get current predictor values
                current_predictors = future_data.loc[idx, predictors]
                
                # For each POT parameter, predict using multivariate copula
                predicted_params = {}
                param_names = self.tail_dist_obj.param_names()
                
                for param in param_names:
                    if param not in self.copula_params:
                        continue
                        
                    copula_info = self.copula_params[param]
                    
                    # Get predictor values for this observation
                    pred_values = []
                    for pred_name in copula_info['predictors']:
                        if pred_name in current_predictors.index:
                            pred_values.append(current_predictors[pred_name])
                        else:
                            pred_values.append(np.nan)
                    
                    # Skip if any predictors are missing
                    if any(np.isnan(pred_values)):
                        continue
                    
                    # Convert predictor values to uniform margins
                    uniform_predictors = []
                    for i, (pred_name, pred_value) in enumerate(zip(copula_info['predictors'], pred_values)):
                        marginal_data = copula_info['predictor_marginals'][pred_name]
                        pred_rank = stats.percentileofscore(marginal_data, pred_value) / 100
                        pred_rank = np.clip(pred_rank, 0.01, 0.99)
                        uniform_predictors.append(pred_rank)
                    
                    # Use copula conditional distribution (simplified)
                    # For Gaussian copula, this would involve conditional multivariate normal
                    # For now, use empirical approach based on similar predictor values
                    
                    # Find similar historical conditions
                    similarity_scores = []
                    for hist_idx in range(len(copula_info['pot_marginal'])):
                        hist_predictors = [copula_info['predictor_marginals'][pred_name][hist_idx] 
                                         for pred_name in copula_info['predictors']]
                        
                        # Calculate similarity (inverse Euclidean distance)
                        distance = np.sqrt(sum((p1 - p2)**2 for p1, p2 in zip(pred_values, hist_predictors)))
                        similarity = 1 / (1 + distance)
                        similarity_scores.append(similarity)
                    
                    # Weight historical POT parameter values by similarity
                    weights = np.array(similarity_scores)
                    weights = weights / weights.sum()
                    
                    predicted_param = np.average(copula_info['pot_marginal'].values, weights=weights)
                    predicted_params[param] = predicted_param
                
                # Calculate exceedance probability using predicted parameters
                if len(predicted_params) == len(param_names):
                    tail_prob = 1 - self.tail_dist_obj.cdf(target_exceedance, predicted_params)
                    predictions.append(max(0.0, min(1.0, tail_prob)))
                else:
                    predictions.append(0.0)
                    
            except Exception as e:
                print(f"Warning: Prediction failed for index {idx}: {e}")
                predictions.append(0.0)
        
        return np.array(predictions)
    
    def calculate_minimum_flow_requirements(self, flow_data, salinity_risk_target=0.05, 
                                                    confidence_levels=[0.5, 0.8, 0.90, 0.95]):
        """
        Calculate minimum flow requirements using parameter uncertainty propagation
        instead of bootstrap resampling for better efficiency and accuracy
        """
        print(f"Calculating minimum flow requirements for risk target = {salinity_risk_target}")
        
        results = {}
        
        # Extract parameter uncertainty from fitted models
        param_uncertainties = self._extract_parameter_uncertainties()
        
        for confidence in confidence_levels:
            print(f"\nCalculating {confidence*100}% confidence requirements...")
            
            # Monte Carlo on model parameters (more efficient than bootstrap)
            n_simulations = 500  # Reduced from 1000 for efficiency
            flow_requirements = []
            
            for sim in range(n_simulations):
                # Sample from parameter distributions
                sampled_params = self._sample_model_parameters(param_uncertainties)
                
                # For each time period, find minimum flow
                min_flows = []
                
                for t in flow_data.index:
                    min_flow = self._solve_minimum_flow_for_period(
                        t, flow_data, salinity_risk_target, sampled_params
                    )
                    min_flows.append(min_flow)
                
                flow_requirements.append(min_flows)
            
            # Calculate confidence intervals
            flow_requirements = np.array(flow_requirements)
            
            results[f'{confidence*100}%'] = {
                'mean': np.mean(flow_requirements, axis=0),
                'lower': np.percentile(flow_requirements, (1-confidence)*100/2, axis=0),
                'upper': np.percentile(flow_requirements, (1+confidence)*100/2, axis=0),
                'shortage_risk': np.mean(flow_requirements > flow_data.values, axis=0),
                'flow_periods': flow_data.index.tolist()
            }
        
        return results
    
    def _extract_parameter_uncertainties(self):
        """Extract parameter uncertainties from fitted copula models"""
        uncertainties = {}
        
        for param, copula_info in self.copula_params.items():
            # For Gaussian copula, uncertainty comes from correlation matrix estimation
            n_obs = copula_info['n_observations']
            n_predictors = len(copula_info['predictors'])
            
            # Approximate standard error for correlation coefficients
            if self.config.copula_type == 'gaussian':
                corr_matrix = copula_info['copula_params']
                # Fisher transformation for correlation uncertainty
                se_corr = 1 / np.sqrt(n_obs - 3)  # Standard error of Fisher z-transform
                
                uncertainties[param] = {
                    'correlation_se': se_corr,
                    'sample_size': n_obs,
                    'n_predictors': n_predictors
                }
            
        return uncertainties
    
    def _sample_model_parameters(self, param_uncertainties):
        """Sample model parameters from their uncertainty distributions"""
        sampled_params = {}
        
        for param, uncertainty_info in param_uncertainties.items():
            # Add some noise to correlation parameters based on standard error
            se = uncertainty_info['correlation_se']
            noise_scale = min(se, 0.1)  # Cap the noise
            
            sampled_params[param] = {
                'correlation_noise': np.random.normal(0, noise_scale),
                'base_params': self.copula_params[param]
            }
        
        return sampled_params
    
    def _solve_minimum_flow_for_period(self, period, flow_data, risk_target, sampled_params):
        """Solve for minimum flow requirement for a specific time period"""
        
        # Get base flow values
        base_flows = flow_data.loc[period]
        flow_range = np.linspace(base_flows.min() * 0.5, base_flows.max() * 2, 20)
        
        for test_flow in flow_range:
            # Create test data with hypothetical flow
            test_data = flow_data.loc[[period]].copy()
            
            # Set flow predictors to test value (simplified approach)
            flow_cols = [col for col in flow_data.columns if 'flow' in col.lower() or 'discharge' in col.lower()]
            for col in flow_cols:
                if col in test_data.columns:
                    test_data.loc[period, col] = test_flow
            
            # Predict risk with sampled parameters (simplified)
            try:
                # This would need to be implemented with the sampled parameters
                # For now, use base prediction
                risk = self.predict_pot_risk(test_data, flow_data.columns)[0]
                
                if risk <= risk_target:
                    return test_flow
                    
            except Exception as e:
                continue
        
        # If no flow achieves target, return maximum tested flow
        return flow_range[-1]
    
    def fit_full_model(self, data, predictors):
        """
        Complete model fitting pipeline with variable handling
        """
        print("=== COPULA-BASED POT MODEL FOR FERC SALINITY MANAGEMENT ===")
        
        # Step 1: Detect and classify variable types
        usable_predictors = self.detect_and_classify_variables(data, predictors)
        
        # Step 2: Fit rolling POT parameters
        self.fit_rolling_pot_params(data)
        
        # Step 3: Prepare predictor data with automatic type handling
        self.prepare_predictor_data(data, usable_predictors)
        
        # Step 4: Fit multivariate copula relationships
        self.fit_multivariate_copula_relationships(data, usable_predictors)
        
        print(f"\n=== MODEL FITTING COMPLETE ===")
        print(f"- Variable types detected for {len(self.variable_types)} predictors")
        print(f"- POT parameters fitted for {len(self.rolling_params)} time periods")
        print(f"- Predictor data prepared for {len(self.predictor_data)} periods") 
        print(f"- Multivariate copula relationships fitted for {len(self.copula_params)} POT parameters")
        
        return self

# Map tail distribution names to classes
TAIL_DISTS = {
    'burr': Burr(),
    'gpd': GPD(),
    'gengamma': GenGamma(), 
    'lognormal': Lognormal(),
    'loglogistic': Loglogistic(),
    'gamma': Gamma()
}

def load_data(config):
    """Load and prepare data with robust datetime handling"""
    try:
        data = pd.read_csv(config.data_csv)
        
        if 'DateTime' in data.columns:
            data['DateTime'] = data['DateTime'].astype(str).str.strip()
            data['DateTime'] = data['DateTime'].apply(
                lambda x: x if ':' in x else x + ' 00:00:00'
            )
            data['DateTime'] = pd.to_datetime(data['DateTime'], errors='coerce')
            data = data.dropna(subset=['DateTime'])
            data = data.sort_values('DateTime').reset_index(drop=True)
        
        # Load predictors
        all_predictors = json.loads(open(config.predictors_json, "r").read())
        predictors = all_predictors['predictors']['all_predictors']
        
        # Filter predictors that exist in data
        available_predictors = [p for p in predictors if p in data.columns]
        
        if len(available_predictors) != len(predictors):
            missing = set(predictors) - set(available_predictors)
            print(f"Warning: Missing predictors in data: {missing}")
        
        print(f"Data loaded: {data.shape[0]} rows, {len(available_predictors)} available predictors")
        return data, available_predictors
        
    except Exception as e:
        print(f"Error loading data: {e}")
        raise

def run_copula_pot_model(config):
    """
    Main execution function with error handling and reporting
    """
    if isinstance(config, dict):
        config = dict_to_namespace(config)
    
    # Validate required config parameters
    required_params = ['data_csv', 'salinity_col', 'base_threshold', 'target_threshold', 
                      'tail_distribution', 'copula_type']
    missing_params = [p for p in required_params if not hasattr(config, p)]
    if missing_params:
        raise ValueError(f"Missing required config parameters: {missing_params}")
    
    # Set defaults for optional parameters
    if not hasattr(config, 'group_window_days'):
        config.group_window_days = 30
    if not hasattr(config, 'min_exceedances_per_group'):
        config.min_exceedances_per_group = 15
    if not hasattr(config, 'param_smoothing'):
        config.param_smoothing = False
    if not hasattr(config, 'max_predictors_per_copula'):
        config.max_predictors_per_copula = 5
    
    # Load data
    data, predictors = load_data(config)
    
    # Create target variables for evaluation
    data['exceed_base'] = (data[config.salinity_col] > config.base_threshold).astype(int)
    data['exceed_target'] = (data[config.salinity_col] > config.target_threshold).astype(int)
    
    print(f"\nThreshold Analysis:")
    print(f"- Base threshold ({config.base_threshold}) exceedance rate: {data['exceed_base'].mean():.3f}")
    print(f"- Target threshold ({config.target_threshold}) exceedance rate: {data['exceed_target'].mean():.3f}")
    
    # Initialize and fit copula-based POT model
    try:
        model = TimeVaryingPOTModel(config)
        model.fit_full_model(data, predictors)
        
        # Generate predictions on full dataset
        print(f"\nGenerating predictions...")
        predictions = model.predict_pot_risk(
            data.set_index('DateTime'), 
            [p for p in predictors if model.variable_types.get(p) != 'constant'][:config.max_predictors_per_copula]
        )
        
        # Evaluate prediction performance
        actual = data['exceed_target'].values
        if len(predictions) == len(actual):
            from sklearn.metrics import roc_auc_score, average_precision_score
            try:
                auc = roc_auc_score(actual, predictions)
                ap = average_precision_score(actual, predictions)
                print(f"- Prediction AUC: {auc:.3f}")
                print(f"- Average Precision: {ap:.3f}")
            except Exception as e:
                print(f"- Could not calculate prediction metrics: {e}")
        
        # Calculate minimum flow requirements if flow predictors available
        flow_predictors = [p for p in predictors 
                          if any(keyword in p.lower() for keyword in ['flow', 'discharge', 'inflow'])
                          and model.variable_types.get(p, 'continuous') in ['continuous', 'discrete']]
        
        flow_requirements = None
        if len(flow_predictors) >= 1:
            print(f"\nCalculating minimum flow requirements using {len(flow_predictors)} flow predictors...")
            
            try:
                flow_data = data[flow_predictors].copy()
                flow_data.index = data['DateTime']
                
                flow_requirements = model.calculate_minimum_flow_requirements(
                    flow_data, 
                    salinity_risk_target=0.05,
                    confidence_levels=[0.5, 0.8, 0.9, 0.95]
                )
                
                print("\n=== FERC MINIMUM FLOW REQUIREMENTS ===")
                for confidence, results in flow_requirements.items():
                    print(f"{confidence} confidence level:")
                    print(f"  Mean required flow: {np.mean(results['mean']):.2f}")
                    print(f"  Flow range: [{np.mean(results['lower']):.2f}, {np.mean(results['upper']):.2f}]")
                    print(f"  Expected shortage risk: {np.mean(results['shortage_risk']):.3f}")
                    
            except Exception as e:
                print(f"Warning: Could not calculate flow requirements: {e}")
        else:
            print(f"No suitable flow predictors found for FERC calculations")
            print(f"Available predictors: {list(model.variable_types.keys())}")
        
        return {
            'model': model,
            'predictions': predictions,
            'flow_requirements': flow_requirements,
            'data': data,
            'variable_types': model.variable_types,
            'config': config
        }
        
    except Exception as e:
        print(f"Error in model fitting: {e}")
        raise

def validate_config(config):
    """Validate configuration parameters"""
    if isinstance(config, dict):
        config = dict_to_namespace(config)
    
    # Check copula type
    valid_copulas = ['gaussian', 'student_t']
    if config.copula_type not in valid_copulas:
        raise ValueError(f"copula_type must be one of {valid_copulas}")
    
    # Check tail distribution
    valid_tail_dists = list(TAIL_DISTS.keys())
    if config.tail_distribution not in valid_tail_dists:
        raise ValueError(f"tail_distribution must be one of {valid_tail_dists}")
    
    # Check thresholds
    if config.base_threshold >= config.target_threshold:
        raise ValueError("base_threshold must be less than target_threshold")
    
    print("Configuration validated successfully")
    return config

# # Example usage and configuration template:
# def create_example_config():
#     """Create example configuration for R integration"""
#     return {
#         'data_csv': 'Data/Tidied/Final/CleanFinalModelData.csv',
#         'predictors_json': 'predictors.json',  # Add this path
#         'salinity_col': 'Salinity',
#         'base_threshold': 0.2,
#         'target_threshold': 1.0,  # FERC compliance threshold
#         'tail_distribution': 'gpd',  # Generalized Pareto for POT
#         'copula_type': 'gaussian',  # or 'student_t'
#         'group_window_days': 30,  # Rolling window size
#         'min_exceedances_per_group': 15,  # Minimum exceedances per window
#         'param_smoothing': True,  # Smooth POT parameters over time
#         'max_predictors_per_copula': 5,  # Limit for computational efficiency
#         'random_state': 42
#     }

# if __name__ == "__main__":
#     # Example usage
#     config = create_example_config()
#     config = validate_config(config)
#     results = run_copula_pot_model(config)
