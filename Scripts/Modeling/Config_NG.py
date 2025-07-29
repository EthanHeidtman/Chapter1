# =============================================================================
# Script Name:    Config_NG.py
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-07-28
# Last Updated:   2025-07-28
# Description:    Configures the hyperparameters and specifications for the
#                 different NGBoost experiments. Specifies distributions to test,
#                 as well as metrics and data processing strategies.
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
import os
import json
import multiprocessing
from ngboost import NGBoost
from ngboost.distns import Normal, LogNormal, Gamma
from ngboost.scores import CRPS, LogScore

# =============================================================================
# FILE PATHS
# =============================================================================
DATA_PATH = 'Data/Tidied/Final/CleanFinalModelData.csv'
RESULTS_DIR = 'Outputs/Experiments/NGBoost'
MODELS_DIR = 'Outputs/Experiments/NGBoost'
PLOTS_DIR = 'Outputs/Plots/NGBoost'
LOGS_DIR = 'Outputs/Logs/NGBoost'

# Create directories if they don't exist
for directory in [RESULTS_DIR, MODELS_DIR, PLOTS_DIR, LOGS_DIR]:
    os.makedirs(directory, exist_ok=True)

# =============================================================================
# SELECTED PREDICTORS (FROM LINEAR MODEL ANALYSIS)
# =============================================================================
# Load the JSON file
data = json.loads(open("Outputs/Experiments/LinearModeling/LinearPredictors.json", "r").read())

# Extract all predictors from the correct section
SELECTED_PREDICTORS = data["predictors"]["all_predictors"]

TARGET_VARIABLE = 'Salinity'

# # Additional predictor subsets for ablation studies
# PREDICTOR_SUBSETS = {
#     'minimal': ['Norm_TideRange24', 'Norm_PowLagDischarge72', 'Norm_RollingPowDischarge14'],
#     'flow_focused': ['Norm_PowLagDischarge72', 'Norm_RollingPowDischarge14', 'Norm_PowLagInflows96', 'Norm_RollingPowInflows10'],
#     'stress_focused': ['IsLowInflow', 'Norm_CumulativeInflowDeficit30', 'Norm_TideRange24'],
#     'temporal_focused': ['DayOfYear', 'Norm_TideRange24'],
#     'all_predictors': SELECTED_PREDICTORS
# }

# =============================================================================
# HYPERPARAMETER GRIDS, DISTRIBUTIONS, AND SCORING
# =============================================================================
# Step 1: Find optimal learning rate and n_estimators
GRID_1_LEARNING = {
    'n_estimators': [500, 800, 1200],
    'learning_rate': [0.01, 0.02, 0.05],
    'minibatch_frac': [1.0],
    'col_sample': [1.0]
}

# Step 2: Add regularization (use best from Step 1)
GRID_2_REGULARIZATION = {
    'n_estimators': [800],  # Best from Step 1
    'learning_rate': [0.01],  # Best from Step 1
    'minibatch_frac': [0.7, 0.8, 1.0],
    'col_sample': [0.8, 0.9, 1.0]
}

# Quick test grid for development
GRID_QUICK = {
    'n_estimators': [200, 500],
    'learning_rate': [0.01, 0.05],
    'minibatch_frac': [1.0],
    'col_sample': [1.0]
}

# Base parameters (always used)
BASE_PARAMS = {
    'random_state': 42,
    'verbose': True,
    'tol': 1e-5
}

DISTRIBUTIONS = {
    'normal': Normal,
    'lognormal': LogNormal,    
    'gamma': Gamma
}

SCORING_FUNCTIONS = {
    'LogScore': LogScore,
    'CRPS': CRPS
}


# =============================================================================
# VALIDATION STRATEGY
# =============================================================================
# Time series cross-validation with gaps
CV_CONFIG = {
    'method': 'TimeSeriesSplit',
    'n_splits': 5,
    'gap': 96,  # 24-hour gap (96 15-min intervals)
    'test_size': 0.2
}

# Hold out 2016 extreme event for final testing
HOLDOUT_EVENTS = {
    '2016_extreme': {
        'start_date': '2016-10-01',
        'end_date': '2016-10-31',
        'description': 'October 2016 extreme salinity event'
    }
}

# =============================================================================
# EVALUATION METRICS (Essential Only)
# =============================================================================

# Core metrics calculated for every experiment
CORE_METRICS = {
    # Overall performance
    'r2': 'R-squared',
    'rmse': 'Root Mean Square Error', 
    'mae': 'Mean Absolute Error',
    
    # Extreme event performance
    'high_sal_r2': 'R-squared',
    'high_sal_rmse': 'Root Mean Square Error', 
    'high_sal_mae': 'Mean Absolute Error',
    
    # Probabilistic performance  
    'log_likelihood': 'Model log-likelihood',
    'crps_score': 'Continuous Ranked Probability Score',
    
    # Extreme event performance (most important for your application)
    'high_salinity_precision': 'Precision for salinity > 0.5',
    'high_salinity_recall': 'Recall for salinity > 0.5',
    'extreme_event_bias': 'Mean bias for top 5% salinity values',
    
    # Temporal structure check
    'residual_autocorr': 'Lag-1 autocorrelation of residuals'
}

# Thresholds for salinity events
SALINITY_THRESHOLDS = {
    'moderate': 0.3,   # Moderate concern level
    'high': 0.5,       # High concern level  
    'extreme': 1.0     # Extreme event level (like 2016)
}

# =============================================================================
# EXPERIMENT TYPES (Essential Only)
# =============================================================================

EXPERIMENTS = {
    'quick_test': {
        'hyperparameter_grid': GRID_QUICK,
        'distributions': ['normal', 'lognormal'],
        'n_runs': 1,
        'description': 'Fast test for development'
    },
    
    'find_best_params': {
        'hyperparameter_grid': GRID_1_LEARNING,
        'distributions': ['lognormal'],
        'n_runs': 3,
        'description': 'Find optimal learning rate and n_estimators'
    },
    
    'optimize_regularization': {
        'hyperparameter_grid': GRID_2_REGULARIZATION,
        'distributions': ['lognormal'],
        'n_runs': 3,
        'description': 'Optimize regularization with best base params'
    },
    
    'final_model': {
        'hyperparameter_grid': 'best_params',  # Use best from previous steps
        'distributions': ['lognormal'],
        'include_2016_holdout': True,
        'n_runs': 5,
        'description': 'Final model with 2016 event validation'
    }
}

# =============================================================================
# COMPUTATIONAL SETTINGS
# =============================================================================

# Auto-detect cores, but be conservative
MAX_CORES = min(multiprocessing.cpu_count() - 1, 8)

PARALLEL_CONFIG = {
    'ngboost_n_jobs': min(4, MAX_CORES),           # NGBoost internal parallelization
    'cv_n_jobs': min(3, MAX_CORES),               # Parallelize CV folds
    'hyperparameter_n_jobs': 1,                   # Run hyperparams sequentially
    'backend': 'threading'                        # Better for NGBoost than multiprocessing
}
# =============================================================================
# DATA PROCESSING (Simple)
# =============================================================================

DATA_CONFIG = {
    'missing_values': 'drop',           # Drop rows with missing values
    'scaling': 'robust',                # RobustScaler (good with outliers)
    'target_transform': None            # No transformation (NGBoost handles distribution)
}

# =============================================================================
# MODEL COMPARISON PARAMETERS
# =============================================================================
# Models to compare against NGBoost
BASELINE_MODELS = ['linear', 'random_forest']

# Feature importance calculation
CALCULATE_FEATURE_IMPORTANCE = True


# =============================================================================
# RESULTS AND LOGGING
# =============================================================================

RESULTS_CONFIG = {
    'save_models': True,
    'save_predictions': True,
    'save_feature_importance': True,
    'create_plots': True,
    'experiment_prefix': 'ngboost_salinity'
}
