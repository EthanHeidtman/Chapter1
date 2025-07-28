# =============================================================================
# Script Name:    Experiments_NG.py
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-07-28
# Last Updated:   2025-07-28
# Description:    Allows for quick and easy modification of hyperparameters to 
#                 test a number of different experiments
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
import pandas as pd
import numpy as np
from Config_NG import *
from DataUtils_NG import *
from ModelUtils_NG import *

def quick_experiment(n_estimators=500, learning_rate=0.01, distribution='normal', 
                    n_splits=3, subset_size=None):
    """
    Run a quick NGBoost experiment with specified parameters.
    
    Parameters:
    -----------
    n_estimators : int
        Number of boosting rounds
    learning_rate : float
        Learning rate
    distribution : str
        Distribution name ('normal', 'lognormal', 'exponential')
    n_splits : int
        Number of CV splits
    subset_size : int, optional
        Use only first N observations (for speed)
    
    Returns:
    --------
    dict
        Experiment results
    """
    print(f"Quick Experiment: n_est={n_estimators}, lr={learning_rate}, dist={distribution}")
    
    # Load and prepare data
    data = load_model_data()
    X, y, _ = prepare_modeling_data(data, SELECTED_PREDICTORS, TARGET_VARIABLE)
    
    
    # Subset if requested
    if subset_size and len(X) > subset_size:
        X = X.head(subset_size)
        y = y.head(subset_size)
        print(f"Using subset of {len(X)} observations")
    
    # Create CV splits
    cv_splits = create_time_series_splits(X, n_splits=n_splits)
    
    
    
    # Set up model parameters
    model_params = {
        'n_estimators': n_estimators,
        'learning_rate': learning_rate,
        'distribution': DISTRIBUTIONS[distribution]
    }
    
    # Run cross-validation
    cv_summary, cv_details = cross_validate_ngboost(X, y, model_params, cv_splits)
    
    # Print results
    print("Results:")
    print(f"  CV R²: {cv_summary['r2_mean']:.4f} ± {cv_summary['r2_std']:.4f}")
    print(f"  CV RMSE: {cv_summary['rmse_mean']:.4f} ± {cv_summary['rmse_std']:.4f}")
    if 'r2_extreme_mean' in cv_summary:
        print(f"  Extreme R²: {cv_summary['r2_extreme_mean']:.4f} ± {cv_summary['r2_extreme_std']:.4f}")
    
    return cv_summary

def compare_distributions(n_estimators=250, learning_rate=0.01, n_splits=3):
    """
    Compare different probability distributions.
    
    Parameters:
    -----------
    n_estimators : int
        Number of boosting rounds
    learning_rate : float
        Learning rate
    n_splits : int
        Number of CV splits
    
    Returns:
    --------
    pd.DataFrame
        Comparison results
    """
    print("Comparing distributions...")
    
    # Load and prepare data  
    data = load_model_data()
    X, y, _ = prepare_modeling_data(data, SELECTED_PREDICTORS, TARGET_VARIABLE)
    # Add this right after prepare_modeling_data()
    print(f"X index range: {X.index.min()} to {X.index.max()}")
    print(f"X index is continuous: {X.index.equals(pd.RangeIndex(len(X)))}")
    print(f"First few indices: {X.index[:10].tolist()}")
    print(f"Last few indices: {X.index[-10:].tolist()}")
    
    cv_splits = create_time_series_splits(X, n_splits=n_splits)
    # Add this right after cv_splits creation
    for i, (train_idx, test_idx) in enumerate(cv_splits):
        print(f"Fold {i+1}: train indices {train_idx[:5]}...{train_idx[-5:]}")
        print(f"Fold {i+1}: test indices {test_idx[:5]}...{test_idx[-5:]}")
        print(f"Max train idx: {max(train_idx)}, Max test idx: {max(test_idx)}")
    
    results = []
    
    for dist_name in DISTRIBUTIONS.keys():
        print(f"\nTesting {dist_name}...")
        try:
            model_params = {
                'n_estimators': n_estimators,
                'learning_rate': learning_rate,
                'distribution': DISTRIBUTIONS[dist_name]
            }
            
            cv_summary, _ = cross_validate_ngboost(X, y, model_params, cv_splits)
            
            results.append({
                'Distribution': dist_name,
                'R2_mean': cv_summary['r2_mean'],
                'R2_std': cv_summary['r2_std'],
                'RMSE_mean': cv_summary['rmse_mean'],
                'RMSE_std': cv_summary['rmse_std'],
                'R2_extreme_mean': cv_summary.get('r2_extreme_mean', np.nan),
                'R2_extreme_std': cv_summary.get('r2_extreme_std', np.nan)
            })
            
        except Exception as e:
            print(f"Error with {dist_name}: {e}")
            results.append({
                'Distribution': dist_name,
                'R2_mean': np.nan,
                'R2_std': np.nan,
                'RMSE_mean': np.nan,
                'RMSE_std': np.nan,
                'R2_extreme_mean': np.nan,
                'R2_extreme_std': np.nan
            })
    
    results_df = pd.DataFrame(results)
    print("\nDistribution Comparison Results:")
    print(results_df.round(4).to_string(index=False))
    
    return results_df

def learning_rate_experiment(rates=[0.005, 0.01, 0.02, 0.05], n_estimators=500, 
                           distribution='normal', n_splits=3):
    """
    Test different learning rates.
    
    Parameters:
    -----------
    rates : list
        Learning rates to test
    n_estimators : int
        Number of boosting rounds
    distribution : str
        Distribution to use
    n_splits : int
        Number of CV splits
    
    Returns:
    --------
    pd.DataFrame
        Learning rate comparison results
    """
    print("Testing learning rates...")
    
    # Load and prepare data
    data = load_model_data()
    X, y, _ = prepare_modeling_data(data, SELECTED_PREDICTORS, TARGET_VARIABLE)
    # Add this right after prepare_modeling_data()
    print(f"X index range: {X.index.min()} to {X.index.max()}")
    print(f"X index is continuous: {X.index.equals(pd.RangeIndex(len(X)))}")
    print(f"First few indices: {X.index[:10].tolist()}")
    print(f"Last few indices: {X.index[-10:].tolist()}")
    cv_splits = create_time_series_splits(X, n_splits=n_splits)
    # Add this right after cv_splits creation
    for i, (train_idx, test_idx) in enumerate(cv_splits):
        print(f"Fold {i+1}: train indices {train_idx[:5]}...{train_idx[-5:]}")
        print(f"Fold {i+1}: test indices {test_idx[:5]}...{test_idx[-5:]}")
        print(f"Max train idx: {max(train_idx)}, Max test idx: {max(test_idx)}")
    
    results = []
    
    for lr in rates:
        print(f"\nTesting learning rate: {lr}")
        
        model_params = {
            'n_estimators': n_estimators,
            'learning_rate': lr,
            'distribution': DISTRIBUTIONS[distribution]
        }
        
        cv_summary, _ = cross_validate_ngboost(X, y, model_params, cv_splits)
        
        results.append({
            'Learning_Rate': lr,
            'R2_mean': cv_summary['r2_mean'],
            'R2_std': cv_summary['r2_std'],
            'RMSE_mean': cv_summary['rmse_mean'],
            'RMSE_std': cv_summary['rmse_std']
        })
    
    results_df = pd.DataFrame(results)
    print("\nLearning Rate Comparison:")
    print(results_df.round(4).to_string(index=False))
    
    return results_df

def n_estimators_experiment(n_est_values=[100, 250, 500, 750, 1000], 
                          learning_rate=0.01, distribution='normal', n_splits=3):
    """
    Test different numbers of estimators.
    
    Parameters:
    -----------
    n_est_values : list
        Numbers of estimators to test
    learning_rate : float
        Learning rate
    distribution : str
        Distribution to use
    n_splits : int
        Number of CV splits
    
    Returns:
    --------
    pd.DataFrame
        N_estimators comparison results
    """
    print("Testing n_estimators...")
    
    # Load and prepare data
    data = load_model_data()
    X, y, _ = prepare_modeling_data(data, SELECTED_PREDICTORS, TARGET_VARIABLE)
    # Add this right after prepare_modeling_data()
    print(f"X index range: {X.index.min()} to {X.index.max()}")
    print(f"X index is continuous: {X.index.equals(pd.RangeIndex(len(X)))}")
    print(f"First few indices: {X.index[:10].tolist()}")
    print(f"Last few indices: {X.index[-10:].tolist()}")
    cv_splits = create_time_series_splits(X, n_splits=n_splits)
    # Add this right after cv_splits creation
    for i, (train_idx, test_idx) in enumerate(cv_splits):
        print(f"Fold {i+1}: train indices {train_idx[:5]}...{train_idx[-5:]}")
        print(f"Fold {i+1}: test indices {test_idx[:5]}...{test_idx[-5:]}")
        print(f"Max train idx: {max(train_idx)}, Max test idx: {max(test_idx)}")
    
    results = []
    
    for n_est in n_est_values:
        print(f"\nTesting n_estimators: {n_est}")
        
        model_params = {
            'n_estimators': n_est,
            'learning_rate': learning_rate,
            'distribution': DISTRIBUTIONS[distribution]
        }
        
        cv_summary, _ = cross_validate_ngboost(X, y, model_params, cv_splits)
        
        results.append({
            'N_Estimators': n_est,
            'R2_mean': cv_summary['r2_mean'],
            'R2_std': cv_summary['r2_std'],
            'RMSE_mean': cv_summary['rmse_mean'],
            'RMSE_std': cv_summary['rmse_std']
        })
    
    results_df = pd.DataFrame(results)
    print("\nN_Estimators Comparison:")
    print(results_df.round(4).to_string(index=False))
    
    return results_df

def feature_subset_experiment(feature_groups=None, distribution='normal', 
                            n_estimators=500, learning_rate=0.01, n_splits=3):
    """
    Test different subsets of features.
    
    Parameters:
    -----------
    feature_groups : dict, optional
        Dictionary of feature group names and lists
    distribution : str
        Distribution to use
    n_estimators : int
        Number of boosting rounds
    learning_rate : float
        Learning rate
    n_splits : int
        Number of CV splits
    
    Returns:
    --------
    pd.DataFrame
        Feature subset comparison results
    """
    if feature_groups is None:
        feature_groups = {
            'All_Features': SELECTED_PREDICTORS,
            'Flow_Only': ['Norm_PowLagDischarge72', 'Norm_RollingPowDischarge14', 
                         'Norm_PowLagInflows96', 'Norm_RollingPowInflows10'],
            'Tide_Flow': ['Norm_TideRange24', 'Norm_PowLagDischarge72', 
                         'Norm_RollingPowDischarge14'],
            'Stress_Features': ['IsLowInflow', 'Norm_CumulativeInflowDeficit30', 
                               'Norm_CumulativeInflowDeficit30_x_DayOfYear']
        }
    
    print("Testing feature subsets...")
    
    # Load and prepare data
    data = load_model_data()
    
    results = []
    
    for group_name, features in feature_groups.items():
        print(f"\nTesting {group_name}: {len(features)} features")
        
        # Prepare data with this feature subset
        available_features = [f for f in features if f in data.columns]
        if len(available_features) == 0:
            print(f"  No features available for {group_name}")
            continue
            
        X, y, _ = prepare_modeling_data(data, available_features, TARGET_VARIABLE)
        # Add this right after prepare_modeling_data()
        print(f"X index range: {X.index.min()} to {X.index.max()}")
        print(f"X index is continuous: {X.index.equals(pd.RangeIndex(len(X)))}")
        print(f"First few indices: {X.index[:10].tolist()}")
        print(f"Last few indices: {X.index[-10:].tolist()}")
        cv_splits = create_time_series_splits(X, n_splits=n_splits)
        # Add this right after cv_splits creation
        for i, (train_idx, test_idx) in enumerate(cv_splits):
            print(f"Fold {i+1}: train indices {train_idx[:5]}...{train_idx[-5:]}")
            print(f"Fold {i+1}: test indices {test_idx[:5]}...{test_idx[-5:]}")
            print(f"Max train idx: {max(train_idx)}, Max test idx: {max(test_idx)}")
        
        model_params = {
            'n_estimators': n_estimators,
            'learning_rate': learning_rate,
            'distribution': DISTRIBUTIONS[distribution]
        }
        
        cv_summary, _ = cross_validate_ngboost(X, y, model_params, cv_splits)
        
        results.append({
            'Feature_Group': group_name,
            'N_Features': len(available_features),
            'R2_mean': cv_summary['r2_mean'],
            'R2_std': cv_summary['r2_std'],
            'RMSE_mean': cv_summary['rmse_mean'],
            'RMSE_std': cv_summary['rmse_std']
        })
    
    results_df = pd.DataFrame(results)
    print("\nFeature Subset Comparison:")
    print(results_df.round(4).to_string(index=False))
    
    return results_df

# Example usage functions
def run_basic_experiments():
    """
    Run a set of basic experiments to understand model behavior.
    """
    print("="*60)
    print("RUNNING BASIC NGBOOST EXPERIMENTS")
    print("="*60)
    
    # 1. Compare distributions
    print("\n1. Distribution Comparison")
    dist_results = compare_distributions(n_estimators=250, n_splits=3)
    
    # 2. Learning rate experiment
    print("\n2. Learning Rate Experiment") 
    lr_results = learning_rate_experiment(n_splits=3)
    
    # 3. N_estimators experiment (with smaller values for speed)
    print("\n3. N_Estimators Experiment")
    nest_results = n_estimators_experiment([100, 250, 500], n_splits=3)
    
    return {
        'distributions': dist_results,
        'learning_rates': lr_results,
        'n_estimators': nest_results
    }

if __name__ == "__main__":
    # Run basic experiments
    results = run_basic_experiments()
    
    # Or run individual experiments:
    # quick_experiment(n_estimators=250, learning_rate=0.01, distribution='normal')
    # compare_distributions()
    # learning_rate_experiment()
    # feature_subset_experiment()
