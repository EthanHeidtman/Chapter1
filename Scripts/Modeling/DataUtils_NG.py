# =============================================================================
# Script Name:    DataUtils_NG.py
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-07-28
# Last Updated:   2025-07-28
# Description:    Functions for loading and preprocessing data, and cross-validation.
#                 Also sets up extreme event thresholds and masks.
# =============================================================================

import pandas as pd
import numpy as np
from sklearn.model_selection import TimeSeriesSplit
from sklearn.preprocessing import RobustScaler, StandardScaler
from sklearn.metrics import r2_score, mean_squared_error, mean_absolute_error
import warnings
from datetime import datetime, timedelta

class SalinityDataProcessor:
    """Handle all data loading and preprocessing for salinity modeling"""
    
    def __init__(self, config):
        self.config = config
        self.scaler = None
        self.data = None
        self.holdout_data = None
        
    def load_data(self):
        """Load and basic preprocessing of salinity data"""
        print("Loading data...")
        
        # Load the data
        self.data = pd.read_csv(self.config['data_path'])
        
        # Convert datetime if needed
        if 'DateTime' in self.data.columns:
            self.data['DateTime'] = pd.to_datetime(self.data['DateTime'])
            self.data = self.data.set_index('DateTime')
        
        print(f"Loaded {len(self.data)} observations")
        print(f"Date range: {self.data.index.min()} to {self.data.index.max()}")
        
        return self.data
    
    def prepare_features_target(self, predictors, target):
        """Prepare X and y arrays with missing value handling"""
        
        # Select predictors and target
        required_cols = predictors + [target]
        missing_cols = [col for col in required_cols if col not in self.data.columns]
        if missing_cols:
            raise ValueError(f"Missing columns: {missing_cols}")
        
        # Create feature matrix and target
        X = self.data[predictors].copy()
        y = self.data[target].copy()
        
        # Handle missing values
        if self.config['data_config']['missing_values'] == 'drop':
            mask = ~(X.isnull().any(axis=1) | y.isnull())
            X = X[mask]
            y = y[mask]
            print(f"Dropped {(~mask).sum()} rows with missing values")
        
        print(f"Final dataset: {len(X)} observations, {len(predictors)} predictors")
        
        return X, y
    
    def create_holdout_split(self, X, y):
        """Create holdout set for 2016 extreme event if specified"""
        
        if not hasattr(self.config, 'holdout_events'):
            return X, y, None, None
        
        holdout_masks = {}
        
        for event_name, event_config in self.config['holdout_events'].items():
            start_date = pd.to_datetime(event_config['start_date'])
            end_date = pd.to_datetime(event_config['end_date'])
            
            # Create mask for holdout period
            if hasattr(X, 'index') and hasattr(X.index, 'to_pydatetime'):
                mask = (X.index >= start_date) & (X.index <= end_date)
            else:
                # If no datetime index, skip holdout
                print(f"Warning: Cannot create holdout for {event_name} - no datetime index")
                continue
            
            holdout_masks[event_name] = mask
            print(f"Holding out {mask.sum()} observations for {event_name}")
        
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
        """Scale features using specified scaling method"""
        
        scaling_method = self.config['data_config']['scaling']
        
        if scaling_method == 'robust':
            self.scaler = RobustScaler()
        elif scaling_method == 'standard':
            self.scaler = StandardScaler()
        elif scaling_method is None or scaling_method == 'none':
            return X_train, X_test
        else:
            raise ValueError(f"Unknown scaling method: {scaling_method}")
        
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

class SalinityTimeSeriesCV:
    """Custom time series cross-validation for salinity data"""
    
    def __init__(self, cv_config):
        self.cv_config = cv_config
    
    def split(self, X, y):
        """Generate time series cross-validation splits with gaps"""
        
        n_splits = self.cv_config['n_splits']
        gap = self.cv_config.get('gap', 0)
        test_size = self.cv_config.get('test_size', 0.2)
        
        # Use sklearn's TimeSeriesSplit as base
        tscv = TimeSeriesSplit(n_splits=n_splits, test_size=None, gap=gap)
        
        for train_idx, test_idx in tscv.split(X):
            
            # Apply gap if specified
            if gap > 0:
                # Remove gap points from beginning of test set
                test_idx = test_idx[gap:]
                if len(test_idx) == 0:
                    continue
            
            yield train_idx, test_idx
    
    def get_fold_info(self, X, fold_idx, train_idx, test_idx):
        """Get information about a specific CV fold"""
        
        info = {
            'fold': fold_idx,
            'train_size': len(train_idx),
            'test_size': len(test_idx),
        }
        
        # Add date ranges if available
        if hasattr(X, 'index') and hasattr(X.index, 'to_pydatetime'):
            info.update({
                'train_start': X.index[train_idx[0]],
                'train_end': X.index[train_idx[-1]],
                'test_start': X.index[test_idx[0]],
                'test_end': X.index[test_idx[-1]]
            })
        
        return info

def calculate_salinity_metrics(y_true, y_pred, y_pred_std=None, thresholds=None):
    """Calculate all core metrics for salinity prediction"""
    
    if thresholds is None:
        thresholds = {'moderate': 0.3, 'high': 0.5, 'extreme': 1.0}
    
    metrics = {}
    
    # Basic regression metrics
    metrics['r2'] = r2_score(y_true, y_pred)
    metrics['rmse'] = np.sqrt(mean_squared_error(y_true, y_pred))
    metrics['mae'] = mean_absolute_error(y_true, y_pred)
    
    # Extreme event metrics
    high_threshold = thresholds['high']
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
        metrics['residual_autocorr'] = np.corrcoef(residuals[:-1], residuals[1:])[0, 1]
    else:
        metrics['residual_autocorr'] = 0
    
    # Probabilistic metrics if std provided
    if y_pred_std is not None:
        # Log-likelihood (assuming normal distribution for now)
        log_likelihood = -0.5 * np.sum(np.log(2 * np.pi * y_pred_std**2) + 
                                      ((y_true - y_pred)**2) / (y_pred_std**2))
        metrics['log_likelihood'] = log_likelihood / len(y_true)  # Average log-likelihood
        
        # Simple CRPS approximation (for normal distribution)
        crps_values = y_pred_std * (
            (y_true - y_pred) / y_pred_std * (2 * norm.cdf((y_true - y_pred) / y_pred_std) - 1) +
            2 * norm.pdf((y_true - y_pred) / y_pred_std) - 1 / np.sqrt(np.pi)
        )
        metrics['crps_score'] = np.mean(crps_values)
    
    return metrics

# Import for CRPS calculation
from scipy.stats import norm
