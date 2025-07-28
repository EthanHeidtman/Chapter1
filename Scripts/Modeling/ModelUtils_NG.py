# =============================================================================
# Script Name:    ModelUtils_NG.py
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-06-01
# Last Updated:   2025-07-28
# Description:    Creates NGBoostModel class and defines methods to evaluate 
#                 performance, and compare performance to other model types.
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
import numpy as np
import pandas as pd
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
from sklearn.linear_model import LinearRegression
from sklearn.ensemble import RandomForestRegressor
from ngboost import NGBoost
from ngboost.learners import default_tree_learner
from Config_NG import *

class NGBoostModel:
    """
    Wrapper class for NGBoost model with convenient methods.
    """
    
    def __init__(self, distribution=None, score=None, **params):
        """
        Initialize NGBoost model.
        
        Parameters:
        -----------
        distribution : ngboost distribution
            Distribution for the target variable
        score : ngboost score
            Scoring function 
        **params : dict
            Additional NGBoost parameters
        """
        # Use defaults from config if not provided
        if distribution is None:
            distribution = DISTRIBUTIONS['normal']
        if score is None:
            score = SCORING_FUNCTIONS['mle']
            
        # Merge with default parameters
        model_params = NGBOOST_PARAMS.copy()
        model_params.update(params)
        
        self.model = NGBoost(
            Base=default_tree_learner,
            Dist=distribution,
            Score=score(),
            **model_params
        )
        
        self.is_fitted = False
        self.feature_names = None
        
    def fit(self, X, y):
        """
        Fit the NGBoost model.
        
        Parameters:
        -----------
        X : pd.DataFrame or np.array
            Features
        y : pd.Series or np.array
            Target variable
        """
        if isinstance(X, pd.DataFrame):
            self.feature_names = X.columns.tolist()
        
        self.model.fit(X, y)
        self.is_fitted = True
        
    def predict(self, X, return_std=False):
        """
        Make predictions with the fitted model.
        
        Parameters:
        -----------
        X : pd.DataFrame or np.array
            Features
        return_std : bool
            Whether to return prediction uncertainty
            
        Returns:
        --------
        np.array or tuple
            Predictions, optionally with standard deviations
        """
        if not self.is_fitted:
            raise ValueError("Model must be fitted before making predictions")
            
        pred_dist = self.model.pred_dist(X)
        predictions = pred_dist.loc
        
        if return_std:
            std = pred_dist.scale
            return predictions, std
        
        return predictions
    
    def predict_quantiles(self, X, quantiles=[0.05, 0.25, 0.5, 0.75, 0.95]):
        """
        Get quantile predictions.
        
        Parameters:
        -----------
        X : pd.DataFrame or np.array
            Features
        quantiles : list
            List of quantiles to predict
            
        Returns:
        --------
        pd.DataFrame
            Quantile predictions
        """
        if not self.is_fitted:
            raise ValueError("Model must be fitted before making predictions")
            
        pred_dist = self.model.pred_dist(X)
        quantile_preds = {}
        
        for q in quantiles:
            quantile_preds[f'q{int(q*100)}'] = pred_dist.ppf(q)
            
        return pd.DataFrame(quantile_preds)
    
    def get_feature_importance(self):
        """
        Get feature importance if available.
        
        Returns:
        --------
        pd.Series or None
            Feature importance scores
        """
        if not self.is_fitted:
            raise ValueError("Model must be fitted before getting feature importance")
            
        if hasattr(self.model, 'feature_importances_'):
            importance = self.model.feature_importances_
            if self.feature_names:
                return pd.Series(importance, index=self.feature_names).sort_values(ascending=False)
            else:
                return importance
        
        return None

def calculate_metrics(y_true, y_pred, extreme_mask=None):
    """
    Calculate model performance metrics.
    
    Parameters:
    -----------
    y_true : array-like
        True values
    y_pred : array-like
        Predicted values
    extreme_mask : array-like, optional
        Boolean mask for extreme events
        
    Returns:
    --------
    dict
        Dictionary of metrics
    """
    metrics = {}
    
    # Overall metrics
    metrics['r2'] = r2_score(y_true, y_pred)
    metrics['rmse'] = np.sqrt(mean_squared_error(y_true, y_pred))
    metrics['mae'] = mean_absolute_error(y_true, y_pred)
    
    # Extreme event metrics if mask provided
    if extreme_mask is not None and extreme_mask.sum() > 0:
        y_true_extreme = y_true[extreme_mask]
        y_pred_extreme = y_pred[extreme_mask]
        
        metrics['r2_extreme'] = r2_score(y_true_extreme, y_pred_extreme)
        metrics['rmse_extreme'] = np.sqrt(mean_squared_error(y_true_extreme, y_pred_extreme))
        metrics['mae_extreme'] = mean_absolute_error(y_true_extreme, y_pred_extreme)
    
    return metrics

def cross_validate_ngboost(X, y, model_params=None, cv_splits=None, extreme_percentile=95):
    """
    Perform cross-validation for NGBoost model.
    
    Parameters:
    -----------
    X : pd.DataFrame
        Features
    y : pd.Series
        Target variable
    model_params : dict, optional
        NGBoost model parameters
    cv_splits : list, optional
        Cross-validation splits
    extreme_percentile : float
        Percentile for extreme events
        
    Returns:
    --------
    dict
        Cross-validation results
    """
    if model_params is None:
        model_params = {}
    
    if cv_splits is None:
        from DataUtils_NG import create_time_series_splits
        cv_splits = create_time_series_splits(pd.DataFrame(index=X.index))
    
    # Get extreme events mask
    from DataUtils_NG import get_extreme_events_mask
    extreme_mask = get_extreme_events_mask(y, extreme_percentile)
    
    cv_results = {
        'r2': [],
        'rmse': [],
        'mae': [],
        'r2_extreme': [],
        'rmse_extreme': [],
        'mae_extreme': []
    }
    
    print(f"Running {len(cv_splits)}-fold cross-validation...")
    
    for fold, (train_idx, test_idx) in enumerate(cv_splits):
        print(f"  Fold {fold + 1}/{len(cv_splits)}")
        
        # Split data
        X_train, X_test = X.iloc[train_idx], X.iloc[test_idx]
        y_train, y_test = y.iloc[train_idx], y.iloc[test_idx]
        extreme_test = extreme_mask[test_idx]
        
        # Train model
        model = NGBoostModel(**model_params)
        model.fit(X_train, y_train)
        
        # Make predictions
        y_pred = model.predict(X_test)
        
        # Calculate metrics
        fold_metrics = calculate_metrics(y_test.values, y_pred, extreme_test)
        
        # Store results
        for metric, value in fold_metrics.items():
            if metric in cv_results:
                cv_results[metric].append(value)
    
    # Calculate summary statistics
    cv_summary = {}
    for metric, values in cv_results.items():
        if values:  # Only if we have values
            cv_summary[f'{metric}_mean'] = np.mean(values)
            cv_summary[f'{metric}_std'] = np.std(values)
    
    print("Cross-validation completed!")
    return cv_summary, cv_results

def compare_models(X, y, cv_splits=None):
    """
    Compare NGBoost against baseline models.
    
    Parameters:
    -----------
    X : pd.DataFrame
        Features
    y : pd.Series
        Target variable
    cv_splits : list, optional
        Cross-validation splits
        
    Returns:
    --------
    pd.DataFrame
        Comparison results
    """
    if cv_splits is None:
        from DataUtils_NG import create_time_series_splits
        cv_splits = create_time_series_splits(pd.DataFrame(index=X.index))
    
    models = {
        'NGBoost': NGBoostModel(),
        'Linear': LinearRegression(),
        'RandomForest': RandomForestRegressor(n_estimators=100, random_state=RANDOM_SEED)
    }
    
    results = []
    
    for model_name, model in models.items():
        print(f"\nTesting {model_name}...")
        fold_scores = []
        
        for train_idx, test_idx in cv_splits:
            X_train, X_test = X.iloc[train_idx], X.iloc[test_idx]
            y_train, y_test = y.iloc[train_idx], y.iloc[test_idx]
            
            # Fit model
            if model_name == 'NGBoost':
                model.fit(X_train, y_train)
                y_pred = model.predict(X_test)
            else:
                model.fit(X_train, y_train)
                y_pred = model.predict(X_test)
            
            # Calculate R²
            fold_r2 = r2_score(y_test, y_pred)
            fold_scores.append(fold_r2)
        
        results.append({
            'Model': model_name,
            'R2_mean': np.mean(fold_scores),
            'R2_std': np.std(fold_scores)
        })
    
    return pd.DataFrame(results)

def grid_search_ngboost(X, y, param_grid, cv_splits=None):
    """
    Simple grid search for NGBoost hyperparameters.
    
    Parameters:
    -----------
    X : pd.DataFrame
        Features
    y : pd.Series
        Target variable
    param_grid : dict
        Parameter grid to search
    cv_splits : list, optional
        Cross-validation splits
        
    Returns:
    --------
    dict
        Best parameters and scores
    """
    if cv_splits is None:
        from DataUtils_NG import create_time_series_splits
        cv_splits = create_time_series_splits(pd.DataFrame(index=X.index))
    
    from itertools import product
    
    # Generate all parameter combinations
    param_names = list(param_grid.keys())
    param_values = list(param_grid.values())
    param_combinations = list(product(*param_values))
    
    best_score = -np.inf
    best_params = None
    results = []
    
    print(f"Testing {len(param_combinations)} parameter combinations...")
    
    for i, param_combo in enumerate(param_combinations):
        params = dict(zip(param_names, param_combo))
        print(f"  Combination {i+1}/{len(param_combinations)}: {params}")
        
        # Cross-validate with these parameters
        cv_summary, _ = cross_validate_ngboost(X, y, params, cv_splits)
        
        mean_r2 = cv_summary.get('r2_mean', -np.inf)
        results.append({**params, 'cv_r2': mean_r2})
        
        if mean_r2 > best_score:
            best_score = mean_r2
            best_params = params
    
    print(f"\nBest parameters: {best_params}")
    print(f"Best CV R²: {best_score:.4f}")
    
    return {
        'best_params': best_params,
        'best_score': best_score,
        'all_results': results
    }
