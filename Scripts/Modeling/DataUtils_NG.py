# =============================================================================
# Script Name:    DataUtils_NG.py
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-07-28
# Last Updated:   2025-07-28
# Description:    Functions for loading and preprocessing data, and cross-validation.
#                 Also sets up extreme event thresholds and masks.
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
import pandas as pd
import numpy as np
from sklearn.model_selection import TimeSeriesSplit
from sklearn.preprocessing import RobustScaler, StandardScaler
from sklearn.metrics import r2_score, mean_squared_error, mean_absolute_error
import warnings
from datetime import datetime, timedelta
from scipy.stats import norm
import os

class SalinityDataProcessor:
    """Handle all data loading and preprocessing for salinity modeling"""
    
    def __init__(self, data_path, selected_predictors, target_variable, data_config, holdout_events=None):
        """
        Initialize with config parameters
        
        Parameters:
        -----------
        data_path : str
            Path to the CSV data file (from Config_NG.DATA_PATH)
        selected_predictors : list
            List of predictor column names (from Config_NG.SELECTED_PREDICTORS)
        target_variable : str
            Target variable name (from Config_NG.TARGET_VARIABLE)
        data_config : dict
            Data processing configuration (from Config_NG.DATA_CONFIG)
        holdout_events : dict, optional
            Holdout event configuration (from Config_NG.HOLDOUT_EVENTS)
        """
        self.data_path = data_path
        self.selected_predictors = selected_predictors
        self.target_variable = target_variable
        self.data_config = data_config
        self.holdout_events = holdout_events
        self.scaler = None
        self.data = None
        self.holdout_data = None
        
    def load_data(self):
        """Load and basic preprocessing of salinity data"""
        print("Loading data...")
        
        # Verify file exists
        if not os.path.exists(self.data_path):
            raise FileNotFoundError(f"Data file not found: {self.data_path}")
        
        # Load the data
        self.data = pd.read_csv(self.data_path)
        
        # Convert datetime if needed
        if 'DateTime' in self.data.columns:
            self.data['DateTime'] = pd.to_datetime(self.data['DateTime'])
            self.data = self.data.set_index('DateTime')
        elif 'Date' in self.data.columns:
            self.data['Date'] = pd.to_datetime(self.data['Date'])
            self.data = self.data.set_index('Date')
        
        print(f"Loaded {len(self.data)} observations")
        print(f"Available columns: {list(self.data.columns)}")
        if hasattr(self.data.index, 'min'):
            print(f"Date range: {self.data.index.min()} to {self.data.index.max()}")
        
        return self.data
    
    def prepare_features_target(self):
        """Prepare X and y arrays with missing value handling using config settings"""
        
        # Verify all required columns exist
        required_cols = self.selected_predictors + [self.target_variable]
        missing_cols = [col for col in required_cols if col not in self.data.columns]
        if missing_cols:
            raise ValueError(f"Missing columns in dataset: {missing_cols}")
        
        # Create feature matrix and target
        X = self.data[self.selected_predictors].copy()
        y = self.data[self.target_variable].copy()
        
        print(f"Original dataset: {len(X)} observations, {len(self.selected_predictors)} predictors")
        
        # Handle missing values according to config
        if self.data_config['missing_values'] == 'drop':
            mask = ~(X.isnull().any(axis=1) | y.isnull())
            X = X[mask]
            y = y[mask]
            print(f"Dropped {(~mask).sum()} rows with missing values")
        elif self.data_config['missing_values'] == 'impute':
            # Simple forward fill for time series data
            X = X.fillna(method='ffill').fillna(method='bfill')
            y = y.fillna(method='ffill').fillna(method='bfill')
            print("Imputed missing values using forward/backward fill")
        
        print(f"Final dataset: {len(X)} observations, {len(self.selected_predictors)} predictors")
        print(f"Target variable ({self.target_variable}) range: {y.min():.3f} to {y.max():.3f}")
        
        return X, y
    
    def create_holdout_split(self, X, y):
        """Create holdout set for extreme events if specified in config"""
        
        if self.holdout_events is None:
            return X, y, None, None
        
        holdout_masks = {}
        
        for event_name, event_config in self.holdout_events.items():
            start_date = pd.to_datetime(event_config['start_date'])
            end_date = pd.to_datetime(event_config['end_date'])
            
            # Create mask for holdout period
            if hasattr(X, 'index') and pd.api.types.is_datetime64_any_dtype(X.index):
                mask = (X.index >= start_date) & (X.index <= end_date)
                holdout_masks[event_name] = mask
                print(f"Holding out {mask.sum()} observations for {event_name} ({event_config['description']})")
            else:
                print(f"Warning: Cannot create holdout for {event_name} - no datetime index")
                continue
        
        if not holdout_masks:
            return X, y, None, None
        
        # Combine all holdout masks
        combined_mask = pd.Series(False, index=X.index)
        for mask in holdout_masks.values():
            combined_mask = combined_mask | mask
        
        # Split data
        X_train = X[~combined_mask]
        y_train = y[~combined_mask]
        X_holdout = X[combined_mask] 
        y_holdout = y[combined_mask]
        
        print(f"Training set: {len(X_train)} observations")
        print(f"Holdout set: {len(X_holdout)} observations")
        
        return X_train, y_train, X_holdout, y_holdout
    
    def scale_features(self, X_train, X_test=None):
        """Scale features using method specified in config"""
        
        scaling_method = self.data_config['scaling']
        
        if scaling_method == 'robust':
            self.scaler = RobustScaler()
        elif scaling_method == 'standard':
            self.scaler = StandardScaler()
        elif scaling_method is None or scaling_method == 'none':
            print("No feature scaling applied")
            return X_train, X_test
        else:
            raise ValueError(f"Unknown scaling method: {scaling_method}")
        
        print(f"Applying {scaling_method} scaling to features")
        
        # Fit on training data
        X_train_scaled = pd.DataFrame(
            self.scaler.fit_transform(X_train),
            index=X_train.index,
            columns=X_train.columns
        )
        
        if X_test is not None:
            X_test_scaled = pd.DataFrame(
                self.scaler.transform(X_test),
                index=X_test.index,
                columns=X_test.columns
            )
            return X_train_scaled, X_test_scaled
        
        return X_train_scaled, None
    
    def apply_target_transform(self, y):
        """Apply target transformation if specified in config"""
        
        transform_method = self.data_config.get('target_transform', None)
        
        if transform_method is None or transform_method == 'none':
            return y
        elif transform_method == 'log':
            # Add small constant to avoid log(0)
            return np.log(y + 1e-6)
        elif transform_method == 'sqrt':
            return np.sqrt(y)
        else:
            raise ValueError(f"Unknown target transform: {transform_method}")

class SalinityTimeSeriesCV:
    """Custom time series cross-validation for salinity data"""
    
    def __init__(self, cv_config):
        """Initialize with CV configuration from Config_NG.CV_CONFIG"""
        self.cv_config = cv_config
    
    def split(self, X, y):
        """Generate time series cross-validation splits with gaps"""
        
        n_splits = self.cv_config['n_splits']
        gap = self.cv_config.get('gap', 0)
        test_size = self.cv_config.get('test_size', None)
        
        print(f"Time Series CV: {n_splits} splits with {gap}-hour gap")
        
        # Calculate test size if specified as proportion
        if test_size is not None and test_size < 1.0:
            test_size = int(len(X) * test_size / n_splits)
        
        # Use sklearn's TimeSeriesSplit as base
        if test_size is not None:
            tscv = TimeSeriesSplit(n_splits=n_splits, test_size=test_size, gap=gap)
        else:
            tscv = TimeSeriesSplit(n_splits=n_splits, gap=gap)
        
        fold_count = 0
        for train_idx, test_idx in tscv.split(X):
            
            # Apply additional gap if specified and not already handled
            if gap > 0 and hasattr(tscv, '_gap_handled') and not tscv._gap_handled:
                # Remove gap points from beginning of test set
                test_idx = test_idx[gap:]
                if len(test_idx) == 0:
                    continue
            
            fold_count += 1
            print(f"  Fold {fold_count}: Train={len(train_idx)}, Test={len(test_idx)}")
            
            yield train_idx, test_idx
    
    def get_fold_info(self, X, fold_idx, train_idx, test_idx):
        """Get information about a specific CV fold"""
        
        info = {
            'fold': fold_idx,
            'train_size': len(train_idx),
            'test_size': len(test_idx),
        }
        
        # Add date ranges if available
        if hasattr(X, 'index') and pd.api.types.is_datetime64_any_dtype(X.index):
            info.update({
                'train_start': X.index[train_idx[0]],
                'train_end': X.index[train_idx[-1]],
                'test_start': X.index[test_idx[0]],
                'test_end': X.index[test_idx[-1]]
            })
        
        return info

def calculate_salinity_metrics(y_true, y_pred, y_pred_std=None, salinity_thresholds=None):
    """
    Calculate all core metrics for salinity prediction
    
    Parameters:
    -----------
    y_true : array-like
        True salinity values
    y_pred : array-like
        Predicted salinity values
    y_pred_std : array-like, optional
        Predicted standard deviations for probabilistic metrics
    salinity_thresholds : dict, optional
        Thresholds for event classification (from Config_NG.SALINITY_THRESHOLDS)
    """
    
    if salinity_thresholds is None:
        salinity_thresholds = {'moderate': 0.3, 'high': 0.5, 'extreme': 1.0}
    
    metrics = {}
    
    # Basic regression metrics
    metrics['r2'] = r2_score(y_true, y_pred)
    metrics['rmse'] = np.sqrt(mean_squared_error(y_true, y_pred))
    metrics['mae'] = mean_absolute_error(y_true, y_pred)
    
    # High salinity subset metrics
    high_threshold = salinity_thresholds['high']
    high_mask = y_true >= high_threshold
    
    if high_mask.sum() > 0:
        metrics['high_sal_r2'] = r2_score(y_true[high_mask], y_pred[high_mask])
        metrics['high_sal_rmse'] = np.sqrt(mean_squared_error(y_true[high_mask], y_pred[high_mask]))
        metrics['high_sal_mae'] = mean_absolute_error(y_true[high_mask], y_pred[high_mask])
    else:
        metrics['high_sal_r2'] = np.nan
        metrics['high_sal_rmse'] = np.nan
        metrics['high_sal_mae'] = np.nan
    
    # Classification metrics for high salinity events
    y_true_high = (y_true >= high_threshold).astype(int)
    y_pred_high = (y_pred >= high_threshold).astype(int)
    
    # Precision and recall for high salinity events
    tp = np.sum((y_true_high == 1) & (y_pred_high == 1))
    fp = np.sum((y_true_high == 0) & (y_pred_high == 1))
    fn = np.sum((y_true_high == 1) & (y_pred_high == 0))
    
    metrics['high_salinity_precision'] = tp / (tp + fp) if (tp + fp) > 0 else 0
    metrics['high_salinity_recall'] = tp / (tp + fn) if (tp + fn) > 0 else 0
    
    # Bias in extreme events (top 5%)
    extreme_threshold = np.percentile(y_true, 95)
    extreme_mask = y_true >= extreme_threshold
    if extreme_mask.sum() > 0:
        metrics['extreme_event_bias'] = np.mean(y_pred[extreme_mask] - y_true[extreme_mask])
    else:
        metrics['extreme_event_bias'] = 0
    
    # Temporal autocorrelation of residuals
    residuals = y_true - y_pred
    if len(residuals) > 1:
        try:
            metrics['residual_autocorr'] = np.corrcoef(residuals[:-1], residuals[1:])[0, 1]
        except:
            metrics['residual_autocorr'] = 0
    else:
        metrics['residual_autocorr'] = 0
    
    # Probabilistic metrics if std provided (NGBoost output)
    if y_pred_std is not None:
        # Log-likelihood (assuming normal distribution)
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            log_likelihood = -0.5 * np.sum(np.log(2 * np.pi * y_pred_std**2) + 
                                          ((y_true - y_pred)**2) / (y_pred_std**2))
            metrics['log_likelihood'] = log_likelihood / len(y_true)  # Average log-likelihood
        
        # Simple CRPS approximation (for normal distribution)
        try:
            standardized_residuals = (y_true - y_pred) / y_pred_std
            crps_values = y_pred_std * (
                standardized_residuals * (2 * norm.cdf(standardized_residuals) - 1) +
                2 * norm.pdf(standardized_residuals) - 1 / np.sqrt(np.pi)
            )
            metrics['crps_score'] = np.mean(crps_values)
        except:
            metrics['crps_score'] = np.nan
    
    return metrics

def calculate_threshold_probabilities(y_pred_dist, thresholds):
    """
    Calculate probability of exceeding thresholds from NGBoost distribution
    
    Parameters:
    -----------
    y_pred_dist : NGBoost distribution object
        Predicted distribution from NGBoost
    thresholds : dict or list
        Threshold values for probability calculation
    
    Returns:
    --------
    dict : Probabilities of exceeding each threshold
    """
    
    if isinstance(thresholds, dict):
        threshold_values = thresholds.values()
        threshold_names = thresholds.keys()
    else:
        threshold_values = thresholds
        threshold_names = [f'threshold_{t}' for t in thresholds]
    
    probabilities = {}
    
    for name, threshold in zip(threshold_names, threshold_values):
        # Calculate P(Y > threshold)
        prob_exceed = 1 - y_pred_dist.cdf(threshold)
        probabilities[f'prob_exceed_{name}'] = prob_exceed
    
    return probabilities

def create_experiment_summary(experiment_name, config, metrics_dict):
    """
    Create a summary dictionary for experiment results
    
    Parameters:
    -----------
    experiment_name : str
        Name of the experiment
    config : dict
        Configuration used for the experiment  
    metrics_dict : dict
        Calculated metrics from the experiment
    
    Returns:
    --------
    dict : Experiment summary
    """
    
    summary = {
        'experiment_name': experiment_name,
        'timestamp': datetime.now().isoformat(),
        'configuration': {
            'predictors': config.get('selected_predictors', []),
            'target': config.get('target_variable', ''),
            'cv_splits': config.get('cv_config', {}).get('n_splits', 0),
            'scaling_method': config.get('data_config', {}).get('scaling', ''),
        },
        'metrics': metrics_dict,
        'data_info': {
            'total_observations': len(config.get('data', [])) if 'data' in config else 0,
            'holdout_events': list(config.get('holdout_events', {}).keys()) if config.get('holdout_events') else []
        }
    }
    
    return summary

# Utility function to check data quality
def check_data_quality(data, predictors, target):
    """
    Perform basic data quality checks
    
    Parameters:
    -----------
    data : pd.DataFrame
        Input dataset
    predictors : list
        List of predictor column names
    target : str
        Target variable name
    
    Returns:
    --------
    dict : Data quality report
    """
    
    report = {
        'total_rows': len(data),
        'total_columns': len(data.columns),
        'missing_data': {},
        'duplicated_rows': data.duplicated().sum(),
        'target_stats': {},
        'predictor_stats': {}
    }
    
    # Missing data analysis
    for col in predictors + [target]:
        if col in data.columns:
            missing_count = data[col].isnull().sum()
            missing_pct = (missing_count / len(data)) * 100
            report['missing_data'][col] = {
                'count': missing_count,
                'percentage': missing_pct
            }
    
    # Target variable statistics
    if target in data.columns:
        report['target_stats'] = {
            'min': data[target].min(),
            'max': data[target].max(),
            'mean': data[target].mean(),
            'median': data[target].median(),
            'std': data[target].std(),
            'skewness': data[target].skew(),
            'zeros': (data[target] == 0).sum()
        }
    
    # Basic predictor statistics
    for pred in predictors:
        if pred in data.columns:
            report['predictor_stats'][pred] = {
                'min': data[pred].min(),
                'max': data[pred].max(),
                'mean': data[pred].mean(),
                'std': data[pred].std(),
                'unique_values': data[pred].nunique()
            }
    
    return report
