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
from ngboost import NGBoost
from ngboost.distns import Normal, LogNormal, Exponential
from ngboost.scores import CRPS, LogScore

# =============================================================================
# FILE PATHS
# =============================================================================
DATA_PATH = 'Data/Tidied/Final/CleanFinalModelData.csv'
RESULTS_DIR = 'Outputs/NGBoost'
MODELS_DIR = 'Models/NGBoost'

# Create directories if they don't exist
os.makedirs(RESULTS_DIR, exist_ok=True)
os.makedirs(MODELS_DIR, exist_ok=True)

# =============================================================================
# SELECTED PREDICTORS (FROM LINEAR MODEL ANALYSIS)
# =============================================================================
# Load the JSON file
data = json.loads(open("Outputs/Experiments/LinearModeling/LinearPredictors.json", "r").read())

# Extract all predictors from the correct section
SELECTED_PREDICTORS = data["predictors"]["all_predictors"]

TARGET_VARIABLE = 'Salinity'

# =============================================================================
# NGBOOST HYPERPARAMETERS
# =============================================================================
NGBOOST_PARAMS = {
    'n_estimators': 500,
    'learning_rate': 0.01,
    'minibatch_frac': 1.0,
    'col_sample': 1.0,
    'tol': 1e-5,
    'random_state': 42
}

# Distribution options to test
DISTRIBUTIONS = {
    'normal': Normal,
    'lognormal': LogNormal,
    'exponential': Exponential
}

# Scoring functions to test
SCORING_FUNCTIONS = {
    'LogScore': LogScore,
    'crps': CRPS
}


# =============================================================================
# VALIDATION PARAMETERS
# =============================================================================
# Time series cross-validation
CV_PARAMS = {
    'n_splits': 5,
    'test_size': None,  # Will be calculated based on data size
    'gap': 0            # No gap between train/test sets initially
}

# Validation metrics to track
METRICS = [
    'r2',
    'rmse', 
    'mae',
    'log_likelihood'
]

# =============================================================================
# DATA PROCESSING PARAMETERS
# =============================================================================
# Scaler options
SCALER_OPTIONS = {
    'standard': 'StandardScaler',
    'robust': 'RobustScaler',
    'none': None
}

# Missing value handling
MISSING_VALUE_STRATEGY = 'drop'  # Options: 'drop', 'interpolate', 'fill'

# =============================================================================
# MODEL COMPARISON PARAMETERS
# =============================================================================
# Models to compare against NGBoost
BASELINE_MODELS = ['linear', 'random_forest']

# Feature importance calculation
CALCULATE_FEATURE_IMPORTANCE = True

# =============================================================================
# EXPERIMENT TRACKING
# =============================================================================
# Whether to save intermediate results
SAVE_INTERMEDIATE = True

# Experiment naming convention
EXPERIMENT_PREFIX = 'ngboost_salinity'

# Random seed for reproducibility
RANDOM_SEED = 42
