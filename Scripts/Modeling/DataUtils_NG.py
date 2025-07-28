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
from sklearn.preprocessing import StandardScaler, RobustScaler
from Config_NG import *

def load_model_data(file_path = DATA_PATH):
    """
    Load the cleaned model data CSV file.
    
    Parameters:
    -----------
    file_path : str
        Path to the CSV file
        
    Returns:
    --------
    pd.DataFrame
        Loaded model data
    """
    try:
        data = pd.read_csv(
            file_path, 
            quotechar='"', 
            engine='python', 
            skipinitialspace=True, 
            delimiter=','
        )
        print(f"Data loaded successfully: {data.shape[0]} rows, {data.shape[1]} columns")
        return data
    except Exception as e:
        print(f"Error loading data: {e}")
        return None
     
def check_data_quality(data, predictors, target):
    """
    Perform basic data quality checks.
    
    Parameters:
    -----------
    data : pd.DataFrame
        Input data
    predictors : list
        List of predictor column names
    target : str
        Target variable name
        
    Returns:
    --------
    dict
        Data quality summary
    """
    quality_report = {}
    
    # Check if all columns exist
    missing_cols = [col for col in predictors + [target] if col not in data.columns]
    quality_report['missing_columns'] = missing_cols
    
    if missing_cols:
        print(f"Warning: Missing columns: {missing_cols}")
        available_predictors = [col for col in predictors if col in data.columns]
        predictors = available_predictors
    
    # Check for missing values
    subset_data = data[predictors + [target]]
    missing_values = subset_data.isnull().sum()
    quality_report['missing_values'] = missing_values[missing_values > 0].to_dict()
    
    # Check data types
    quality_report['data_types'] = subset_data.dtypes.to_dict()
    
    # Basic statistics for target variable
    target_stats = data[target].describe()
    quality_report['target_stats'] = target_stats.to_dict()
    
    # Check for infinite values
    inf_values = np.isinf(subset_data.select_dtypes(include=[np.number])).sum()
    quality_report['infinite_values'] = inf_values[inf_values > 0].to_dict()
    
    print("Data Quality Summary:")
    print(f"  Total rows: {len(data)}")
    print(f"  Available predictors: {len([col for col in predictors if col in data.columns])}")
    print(f"  Missing values: {sum(missing_values)}")
    print(f"  Target range: {target_stats['min']:.4f} to {target_stats['max']:.4f}")
    
    return quality_report

def prepare_modeling_data(data, predictors=SELECTED_PREDICTORS, target=TARGET_VARIABLE, 
                         scaler_type='none', handle_missing='drop'):
    """
    Prepare data for modeling by handling missing values and scaling.
    
    Parameters:
    -----------
    data : pd.DataFrame
        Input data
    predictors : list
        List of predictor column names
    target : str
        Target variable name
    scaler_type : str
        Type of scaler ('standard', 'robust', 'none')
    handle_missing : str
        How to handle missing values ('drop', 'interpolate', 'fill')
        
    Returns:
    --------
    tuple
        (X, y, scaler) where X is predictors, y is target, scaler is fitted scaler
    """
    # Filter to available columns
    available_predictors = [col for col in predictors if col in data.columns]
    if len(available_predictors) < len(predictors):
        missing = set(predictors) - set(available_predictors)
        print(f"Warning: Using {len(available_predictors)} of {len(predictors)} predictors. Missing: {missing}")
    
    # Select relevant columns
    model_data = data[available_predictors + [target]].copy()
    
    # Handle missing values
    if handle_missing == 'drop':
        model_data = model_data.dropna()
    elif handle_missing == 'interpolate':
        model_data = model_data.interpolate(method='linear')
    elif handle_missing == 'fill':
        model_data = model_data.fillna(model_data.mean())
    
    print(f"After missing value handling: {len(model_data)} rows")
    
    # Separate features and target
    X = model_data[available_predictors]
    y = model_data[target]
    
    # Apply scaling if requested
    scaler = None
    if scaler_type == 'standard':
        scaler = StandardScaler()
        X_scaled = pd.DataFrame(
            scaler.fit_transform(X), 
            columns=X.columns, 
            index=X.index
        )
        X = X_scaled
    elif scaler_type == 'robust':
        scaler = RobustScaler()
        X_scaled = pd.DataFrame(
            scaler.fit_transform(X), 
            columns=X.columns, 
            index=X.index
        )
        X = X_scaled
    
    print(f"Final modeling data: {X.shape[0]} rows, {X.shape[1]} predictors")
    
    return X, y, scaler

def create_time_series_splits(data, n_splits=5, test_size_months=2):
    """
    Create time series cross-validation splits.
    
    Parameters:
    -----------
    data : pd.DataFrame
        Input data with datetime index or column
    n_splits : int
        Number of CV splits
    test_size_months : int
        Approximate test size in months
        
    Returns:
    --------
    list
        List of (train_idx, test_idx) tuples
    """
    n = len(data)
    
    # Estimate test size (assuming ~720 observations per month for 15-min data)
    test_size = min(test_size_months * 720 * 4, n // (n_splits + 1))  # 4 weeks per month roughly
    
    splits = []
    for i in range(n_splits):
        # Progressive training size
        train_end = n - test_size * (n_splits - i)
        test_start = train_end
        test_end = test_start + test_size
        
        if test_end > n:
            test_end = n
        
        train_idx = np.arange(0, train_end)
        test_idx = np.arange(test_start, test_end)
        
        splits.append((train_idx, test_idx))
    
    print(f"Created {len(splits)} time series splits")
    print(f"Average train size: {np.mean([len(split[0]) for split in splits]):.0f}")
    print(f"Average test size: {np.mean([len(split[1]) for split in splits]):.0f}")
    
    return splits

def get_extreme_events_mask(y, percentile=95):
    """
    Create a mask for extreme salinity events.
    
    Parameters:
    -----------
    y : pd.Series or np.array
        Target variable (salinity)
    percentile : float
        Percentile threshold for extreme events
        
    Returns:
    --------
    np.array
        Boolean mask for extreme events
    """
    threshold = np.percentile(y, percentile)
    extreme_mask = y >= threshold
    
    print(f"Extreme events (>{percentile}th percentile): {extreme_mask.sum()} of {len(y)} ({100*extreme_mask.mean():.1f}%)")
    print(f"Threshold: {threshold:.4f}")
    
    return extreme_mask
