# =============================================================================
# Script Name:    Config_NG.py
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-07-28
# Last Updated:   2025-07-30
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
from ngboost.distns import LogNormal, Gamma

# =============================================================================
# FILE PATHS
# =============================================================================
DATA_PATH = 'Data/Tidied/Final/CleanFinalModelData.csv'
BASE_EXPERIMENTS_DIR = 'Outputs/Experiments/NGBoost'
LOGS_DIR = 'Outputs/Logs/NGBoost'

# Individual experiment directories will be created dynamically
# Structure: Outputs/Experiments/NGBoost/{experiment_name}/
#   ├── models/          # Saved NGBoost models
#   ├── results/         # Performance metrics, summaries
#   ├── plot_data/       # CSV files for R plotting
#   └── predictions/     # Raw prediction outputs

# Create base directories if they don't exist
for directory in [BASE_EXPERIMENTS_DIR, LOGS_DIR]:
    os.makedirs(directory, exist_ok=True)

# =============================================================================
# EXPERIMENT DIRECTORY STRUCTURE
# =============================================================================
def get_experiment_paths(experiment_name):
    """
    Create standardized directory structure for each experiment.
    
    Returns dict with paths for models, results, plot_data, and predictions.
    """
    base_dir = os.path.join(BASE_EXPERIMENTS_DIR, experiment_name)
    
    paths = {
        'base': base_dir,
        'models': os.path.join(base_dir, 'models'),
        'results': os.path.join(base_dir, 'results'), 
        'plot_data': os.path.join(base_dir, 'plot_data'),
        'predictions': os.path.join(base_dir, 'predictions')
    }
    
    # Create all directories
    for path in paths.values():
        os.makedirs(path, exist_ok=True)
    
    return paths

# Load the JSON file
data = json.loads(open("Outputs/Experiments/LinearModeling/LinearPredictors.json", "r").read())

# Extract all predictors from the correct section
SELECTED_PREDICTORS = data["predictors"]["all_predictors"]

TARGET_VARIABLE = 'Salinity'

# =============================================================================
# HYPERPARAMETER GRIDS
# =============================================================================
# Step 1: Find optimal learning rate and n_estimators
GRID_1_LEARNING = {
    'n_estimators': [500, 800, 1200],     # Number of boosting rounds (# of trees)
    'learning_rate': [0.01, 0.02, 0.05],  # How much each tree contributes to final prediction
    'minibatch_frac': [1.0],              # Fraction of training data used in each boosting round
    'col_sample': [1.0]                   # Fraction of the engineered predictors to use in each tree
}

# Step 2: Add regularization (test different fractions and sampling schemes)
GRID_2_REGULARIZATION = {
    'n_estimators': [800],                # Best result from Grid 1
    'learning_rate': [0.01],              # Best result from Grid 1
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

# =============================================================================
# DISTRIBUTIONS - NGBoost handles scoring internally
# =============================================================================
# Only realistic distributions for positive, right-skewed salinity data
DISTRIBUTIONS = {
    'lognormal': LogNormal,    
    'gamma': Gamma
}

POSTHOC_SCORING = True

# =============================================================================
# VALIDATION STRATEGY
# =============================================================================
CV_CONFIG = {
    'method': 'TimeSeriesSplit',
    'n_splits': 5,
    'gap': 24,             # 24-hour gap for hourly data
    'test_size': 0.2
}

# Hold out 2016 extreme event for final testing
HOLDOUT_EVENTS = {
    '2016_extreme': {
        'start_date': '2016-10-01',
        'end_date': '2016-10-31',
        'description': 'October 2016 extreme salinity event (max=1.73)'
    }
}

# =============================================================================
# EVALUATION METRICS - Calculated post-prediction
# =============================================================================
# Core metrics calculated for every experiment
CORE_METRICS = {
    # Overall performance
    'r2': 'R-squared',
    'rmse': 'Root Mean Square Error', 
    'mae': 'Mean Absolute Error',
    
    # Extreme event performance
    'high_sal_r2': 'R-squared for salinity > 0.5',
    'high_sal_rmse': 'RMSE for salinity > 0.5', 
    'high_sal_mae': 'MAE for salinity > 0.5',
    
    # Probabilistic performance 
    'log_likelihood': 'Model log-likelihood',
    'crps_score': 'Continuous Ranked Probability Score',
    
    # Risk-based metrics 
    'high_salinity_precision': 'Precision for salinity > 0.5',
    'high_salinity_recall': 'Recall for salinity > 0.5',
    'extreme_event_bias': 'Mean bias for top 5% salinity values',
    
    # Temporal structure check
    'residual_autocorr': 'Lag-1 autocorrelation of residuals'
}

# Thresholds for salinity events (risk-based management)
SALINITY_THRESHOLDS = {
    'moderate': 0.3,   # Moderate concern level
    'high': 0.5,       # High concern level 
    'extreme': 1.0     # Extreme event level (like 2016)
}

# =============================================================================
# EXPERIMENT PIPELINE
# =============================================================================
EXPERIMENTS = {
    # Phase 1: Quick development test
    'quick_test': {
        'hyperparameter_grid': GRID_QUICK,
        'distributions': ['lognormal'],  
        'n_runs': 1,
        'description': 'Fast development test with LogNormal'
    },
    
    # Phase 2: Distribution comparison
    'distribution_comparison': {
        'hyperparameter_grid': GRID_QUICK,
        'distributions': ['lognormal', 'gamma'],
        'n_runs': 2,
        'description': 'Compare LogNormal vs Gamma distributions'
    },
    
    # Phase 3: Optimize base parameters with best distribution
    'optimize_learning': {
        'hyperparameter_grid': GRID_1_LEARNING,
        'distributions': ['lognormal'],            # Best distribution from phase 2
        'n_runs': 3,
        'description': 'Find optimal learning rate and n_estimators'
    },
    
    # Phase 4: Add regularization
    'optimize_regularization': {
        'hyperparameter_grid': GRID_2_REGULARIZATION,
        'distributions': ['lognormal'],            # Best distribution from phase 2
        'n_runs': 3,
        'description': 'Optimize regularization with best base params'
    },
    
    # Phase 5: Final model with extreme event testing
    'final_model': {
        'hyperparameter_grid': 'best_params',  
        'distributions': ['lognormal'],  
        'include_2016_holdout': True,
        'n_runs': 5,
        'description': 'Final model with 2016 extreme event validation'
    }
}

# =============================================================================
# COMPUTATIONAL SETTINGS
# =============================================================================
MAX_CORES = min(multiprocessing.cpu_count() - 1, 8)

PARALLEL_CONFIG = {
    'ngboost_n_jobs': min(4, MAX_CORES),           # NGBoost internal parallelization
    'cv_n_jobs': min(3, MAX_CORES),                # Parallelize CV folds
    'hyperparameter_n_jobs': 1,                    # Run hyperparams sequentially
    'backend': 'threading'                         # Better for NGBoost than multiprocessing
}

# =============================================================================
# DATA PROCESSING
# =============================================================================
DATA_CONFIG = {
    'missing_values': 'drop',           # Drop rows with missing values
    'scaling': 'robust',                # RobustScaler (good with outliers)
    'target_transform': None            # No transformation (NGBoost handles distribution)
}

# =============================================================================
# MODEL COMPARISON AND ANALYSIS
# =============================================================================
# Baseline models to compare against NGBoost
BASELINE_MODELS = ['linear', 'random_forest']

# Feature importance calculation
CALCULATE_FEATURE_IMPORTANCE = True

# Probabilistic prediction analysis
PROBABILISTIC_ANALYSIS = {
    'confidence_intervals': [0.5, 0.8, 0.9, 0.95],   # For uncertainty quantification
    'risk_thresholds': [0.3, 0.5, 1.0],              # P(Salinity > threshold)
    'save_calibration_data': True,                   
    'save_prediction_intervals': True,               
}

# =============================================================================
# R PLOTTING DATA EXPORT
# =============================================================================
R_PLOT_CONFIG = {
    'export_format': 'csv',             
    
    'export_types': {
        'predictions_vs_observed': True,        # Scatter plots, residual plots
        'time_series_predictions': True,        # Time series with uncertainty bands
        'feature_importance': True,             # Bar plots, importance rankings
        'distribution_comparisons': True,       # Model comparison plots
        'extreme_event_analysis': True,         # 2016 event detailed analysis
        'calibration_data': True,               # Probability calibration plots
        'cross_validation_results': True,       # CV performance across folds
        'hyperparameter_optimization': True,    # Tuning process visualization
    },
    
    # Include metadata for R plotting
    'include_metadata': True,  # Experiment details, model specs, etc.
    'timestamp_format': 'ISO',  # Standard datetime format for R
    
    # File naming convention
    'filename_format': '{experiment_name}_{data_type}_{timestamp}.csv'
}

# =============================================================================
# RESULTS AND LOGGING
# =============================================================================
RESULTS_CONFIG = {
    'save_models': True,
    'save_predictions': True,
    'save_feature_importance': True,
    'create_plots': False,  
    'save_plot_data': True,  
    'save_probabilistic_outputs': True,  
    'experiment_prefix': 'ngboost_salinity',
    
    # Organize results within each experiment directory
    'use_experiment_structure': True  # Use get_experiment_paths() function
}

# =============================================================================
# EXTREME EVENT CONFIGURATION
# =============================================================================
EXTREME_EVENT_CONFIG = {
    'focus_on_tails': True,
    'tail_threshold': 0.95,  # Focus on top 5% of salinity events
    'extreme_event_weight': 2.0,  # Weight extreme events more heavily in evaluation
    'risk_based_evaluation': True,  # Evaluate P(Salinity > threshold) accuracy
}
