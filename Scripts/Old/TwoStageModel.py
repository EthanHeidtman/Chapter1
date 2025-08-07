# =============================================================================
# Script Name:    TwoStageModel.py
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-07-28
# Last Updated:   2025-07-28
# Description:    Implements a two-stage modeling approach that first classifies
#                 the likelihood of salinity exceeding a lower threshold, then 
#                 uses various tail distributions to fit the conditional probability
#                 of exceeding the 1.0 salinity threshold. 
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestClassifier, RandomForestRegressor, GradientBoostingRegressor
from sklearn.linear_model import Ridge
from sklearn.metrics import mean_squared_error, r2_score, mean_absolute_error, log_loss, roc_auc_score, accuracy_score, precision_score, recall_score, f1_score
from sklearn.model_selection import train_test_split
from sklearn.model_selection import TimeSeriesSplit
from types import SimpleNamespace
from TailDistributions import Burr, GPD, GenGamma, Lognormal, Loglogistic, Gamma  
import json
import warnings
warnings.filterwarnings('ignore')

# class Config:
#     data_csv = 'Data/Tidied/Final/CleanFinalModelData.csv'
#     predictors_json = "Outputs/Experiments/LinearModeling/LinearPredictors.json"
#     salinity_col = "Salinity"
#     base_threshold = 0.2
#     target_threshold = 0.8
#     log_transform_excess = False
#     random_state = 42
#     test_size = 0.2
#     tail_distribution = 'lognormal'  # options: 'burr', 'gpd', 'gengamma', 'lognormal'
#     group_window_days = 7
#     param_regression_method = 'rf'  # 'rf', 'gbr', 'ridge'
#     min_exceedances_per_group = 10  # Increased from 5
#     rolling_window_approach = True  
#     param_smoothing = True          # Apply smoothing to parameter time series

def dict_to_namespace(d):
    if not isinstance(d, dict):
        return d
    return SimpleNamespace(**{k: dict_to_namespace(v) for k,v in d.items()})

def load_data(config):
    """Load and prepare data with error handling"""
    try:
        data = pd.read_csv(config.data_csv)
        
        if 'DateTime' in data.columns:
            # Strip whitespace and convert to string
            data['DateTime'] = data['DateTime'].astype(str).str.strip()
            
            # Add midnight if time is missing
            data['DateTime'] = data['DateTime'].apply(
                lambda x: x if ':' in x else x + ' 00:00:00'
            )
            
            # Parse to datetime
            data['DateTime'] = pd.to_datetime(data['DateTime'], errors='coerce')
            
            # Drop rows where DateTime is NaT
            before_drop = len(data)
            data = data.dropna(subset=['DateTime'])
            after_drop = len(data)
            dropped = before_drop - after_drop
            if dropped > 0:
                print(f"Dropped {dropped} rows with invalid or missing DateTime values")
            
            # Set and sort index
            data = data.sort_values('DateTime').reset_index(drop=True)
            
            dupes = data['DateTime'].duplicated().sum()
            if dupes > 0:
                print(f"⚠️ Found {dupes} duplicated DateTime values")
            else:
                print("✅ DateTime values are unique")
        
        all_predictors = json.loads(open("Outputs/Experiments/LinearModeling/LinearPredictors.json", "r").read())
        predictors = all_predictors['predictors']['all_predictors']
        
        print(f"Data loaded: {data.shape[0]} rows, {len(predictors)} predictors")
        print(f"Date range: {data['DateTime'].min()} to {data['DateTime'].max()}")
        
        if 'DateTime' in data.columns:
            dupes = data['DateTime'].duplicated().sum()
            print(f"Duplicate DateTime rows: {dupes}")
        
        return data, predictors
    except Exception as e:
        print(f"Error loading data: {e}")
        raise

# Map tail dist names to classes
TAIL_DISTS = {
    'burr': Burr(),
    'gpd': GPD(),
    'gengamma': GenGamma(),
    'lognormal': Lognormal(),
    'loglogistic' : Loglogistic(),
    'gamma' : Gamma()
}

def fit_tail_params(data, config, tail_dist_obj):
    """
    Improved tail parameter fitting with better grouping strategies
    """
    print(f"Fitting tail parameters using {config.tail_distribution} distribution...")
    
    if config.rolling_window_approach:
        return fit_rolling_params(data, config, tail_dist_obj)
    else:
        return fit_discrete_params(data, config, tail_dist_obj)

def fit_rolling_params(data, config, tail_dist_obj):
    """
    Use rolling windows for parameter estimation - more stable than discrete groups
    """
    window_size = f'{config.group_window_days}D'
    params_list = []
    
    # Create rolling windows every day
    for i in range(len(data)):
        end_date = data.loc[i, 'DateTime']
        start_date = end_date - pd.Timedelta(days=config.group_window_days)
        
        # Get data in window
        window_data = data[(data['DateTime'] >= start_date) & (data['DateTime'] <= end_date)]
        excesses = window_data[config.salinity_col] - config.base_threshold
        excesses = excesses[excesses > 0]
        
        if len(excesses) < config.min_exceedances_per_group:
            continue
            
        try:
            # Fit distribution params
            params = tail_dist_obj.fit_params(excesses.values)
            
            # Ensure params are clean scalars
            clean_params = {}
            for k, v in params.items():
                if isinstance(v, dict):
                    # If nested dict, take first value
                    v = list(v.values())[0] if v else np.nan
                clean_params[k] = float(v) if not pd.isna(v) else np.nan
                
            row = {'timestamp': end_date}
            row.update(clean_params)
            params_list.append(row)
            
        except Exception as e:
            print(f"Warning: Failed to fit parameters for window ending {end_date}: {e}")
            continue
    
    if not params_list:
        raise ValueError("No valid parameter fits found!")
        
    params_df = pd.DataFrame(params_list).set_index('timestamp')
    
    # Apply smoothing if requested
    if config.param_smoothing:
        params_df = params_df.rolling(window=3, center=True).mean()
    
    return params_df.dropna()

def fit_discrete_params(data, config, tail_dist_obj):
    """
    Original discrete grouping approach with improvements
    """
    grouped = data.groupby(pd.Grouper(freq=f'{config.group_window_days}D'))
    params_list = []
    
    for group_time, group_df in grouped:
        if group_time is pd.NaT or group_df.empty:
            continue
            
        excesses = group_df[config.salinity_col] - config.base_threshold
        excesses = excesses[excesses > 0]
        
        if len(excesses) < config.min_exceedances_per_group:
            continue
            
        try:
            params = tail_dist_obj.fit_params(excesses.values)
            
            # Clean parameter extraction
            clean_params = {}
            for k, v in params.items():
                if isinstance(v, dict):
                    v = list(v.values())[0] if v else np.nan
                clean_params[k] = float(v) if not pd.isna(v) else np.nan
                
            row = {'group_time': group_time}
            row.update(clean_params)
            params_list.append(row)
            
        except Exception as e:
            print(f"Warning: Failed to fit parameters for group {group_time}: {e}")
            continue
    
    if not params_list:
        raise ValueError("No valid parameter fits found!")
        
    params_df = pd.DataFrame(params_list).set_index('group_time')
    return params_df.dropna()

def prepare_regression_data(data, params_df, config):
    """
    Prepare data for regression by aligning tail parameters with full dataset timestamps,
    using interpolation for rolling window approach and merge for discrete groups.
    """
    data = data.copy()
    
    # Ensure indexes are datetime and sorted
    if not isinstance(data.index, pd.DatetimeIndex):
        data.index = pd.to_datetime(data.index)
    if not isinstance(params_df.index, pd.DatetimeIndex):
        params_df.index = pd.to_datetime(params_df.index)
    
    data = data.sort_index()
    params_df = params_df.sort_index()
    params_df = params_df[~params_df.index.duplicated()]
    
    if config.rolling_window_approach:
        combined = data.copy()
        param_cols = params_df.columns.tolist()
        
        data_times = data.index.astype(np.int64)
        param_times = params_df.index.astype(np.int64)
        
        for param_col in param_cols:
            combined[param_col] = np.interp(
                data_times,
                param_times,
                params_df[param_col].values
            )
    else:
        data['group_time'] = data.index.to_series().dt.floor(f'{config.group_window_days}D')
        combined = data.merge(params_df, left_on='group_time', right_index=True, how='left')
        combined = combined.drop(columns=['group_time'])
        param_cols = params_df.columns.tolist()
    
    combined[param_cols] = combined[param_cols].fillna(method='ffill').fillna(method='bfill')
    combined = combined.dropna(subset=param_cols)
    
    return combined

def train_param_regressors(X, y, method='rf', random_state=42):
    """
    Improved parameter regression with cross-validation and better hyperparameters
    """
    print(f"Training parameter regressors using {method}...")
    
    # Drop rows with NaNs in X or y
    valid_mask = X.notna().all(axis=1) & y.notna().all(axis=1)
    X = X.loc[valid_mask]
    y = y.loc[valid_mask]
    
    regressors = {}
    regressor_metrics = {}
    
    # Split data for validation
    X_train, X_val, y_train, y_val = train_test_split(
        X, y, test_size=0.2, random_state=random_state
    )
    
    for param in y.columns:
        print(f"  Training regressor for parameter: {param}")
        
        if method == 'rf':
            reg = RandomForestRegressor(
                n_estimators=100,
                max_depth=10,
                min_samples_split=5,
                random_state=random_state,
                n_jobs=-1
            )
        elif method == 'gbr':
            reg = GradientBoostingRegressor(
                n_estimators=100,
                max_depth=6,
                learning_rate=0.1,
                random_state=random_state
            )
        elif method == 'ridge':
            reg = Ridge(alpha=1.0)
        else:
            raise ValueError(f"Unknown regression method: {method}")
        
        # Fit and evaluate
        reg.fit(X_train, y_train[param])
        y_pred = reg.predict(X_val)
        
        # Calculate metrics
        metrics = {
            'rmse': np.sqrt(mean_squared_error(y_val[param], y_pred)),
            'r2': r2_score(y_val[param], y_pred),
            'mae': mean_absolute_error(y_val[param], y_pred)
        }
        
        print(f"    {param} - RMSE: {metrics['rmse']:.4f}, R²: {metrics['r2']:.4f}")
        
        regressors[param] = reg
        regressor_metrics[param] = metrics
    
    return regressors, regressor_metrics

def predict_params(regressors, X):
    """
    Improved parameter prediction with bounds checking
    """
    print("Predicting tail distribution parameters...")
    
    preds = {}
    for param, model in regressors.items():
        y_pred = model.predict(X)
        
        # Ensure predictions are positive (most distribution parameters must be > 0)
        if param in ['scale', 'shape', 'c', 'a']:  # Common positive parameters
            y_pred = np.maximum(y_pred, 1e-6)
            
        preds[param] = y_pred
    
    pred_df = pd.DataFrame(preds, index=X.index)
    
    print(f"Parameter predictions shape: {pred_df.shape}")
    print("Parameter prediction summary:")
    print(pred_df.describe())
    
    return pred_df

def calculate_tail_probabilities(y_reg_pred, tail_dist_obj, excess_target, config):
    """
    Improved tail probability calculation with error handling
    """
    print("Calculating tail probabilities...")
    
    tail_probs = []
    failed_calculations = 0
    
    for idx in y_reg_pred.index:
        try:
            params = y_reg_pred.loc[idx].to_dict()
            
            # Check for invalid parameters
            if any(pd.isna(v) or v <= 0 for v in params.values()):
                tail_probs.append(0.0)
                failed_calculations += 1
                continue
                
            # Calculate CDF
            cdf_val = tail_dist_obj.cdf(excess_target, params)
            
            # Ensure valid probability
            if pd.isna(cdf_val) or cdf_val < 0 or cdf_val > 1:
                tail_probs.append(0.0)
                failed_calculations += 1
                continue
                
            tail_prob = 1 - cdf_val
            tail_probs.append(max(0.0, min(1.0, tail_prob)))  # Bound between 0 and 1
            
        except Exception as e:
            tail_probs.append(0.0)
            failed_calculations += 1
    
    if failed_calculations > 0:
        print(f"Warning: {failed_calculations} tail probability calculations failed")
    
    return np.array(tail_probs)

def evaluate_classification(X, y, model, n_splits=5, gap=24):
    """
    Evaluate classifier using time-series cross-validation.

    Parameters:
        X: Feature DataFrame (must have datetime index)
        y: Binary target Series (aligned with X)
        model: Scikit-learn classifier with fit/predict_proba
        n_splits: Number of time-series folds
        gap: Optional gap (in rows) between train/test splits

    Returns:
        dict with mean/stdev of accuracy, AUC, precision, recall, F1
    """
    tscv = TimeSeriesSplit(n_splits=n_splits, gap=gap)
    
    acc_scores = []
    auc_scores = []
    prec_scores = []
    recall_scores = []
    f1_scores = []
    
    fold = 0
    for train_idx, test_idx in tscv.split(X):
        fold += 1
        X_train, X_test = X.iloc[train_idx], X.iloc[test_idx]
        y_train, y_test = y.iloc[train_idx], y.iloc[test_idx]
        
        model.fit(X_train, y_train)
        y_pred_proba = model.predict_proba(X_test)[:, 1]
        y_pred = (y_pred_proba > 0.5).astype(int)

        acc = accuracy_score(y_test, y_pred)
        try:
            auc = roc_auc_score(y_test, y_pred_proba)
        except ValueError:
            auc = np.nan
        
        prec = precision_score(y_test, y_pred, zero_division=0)
        rec = recall_score(y_test, y_pred, zero_division=0)
        f1 = f1_score(y_test, y_pred, zero_division=0)
        
        acc_scores.append(acc)
        auc_scores.append(auc)
        prec_scores.append(prec)
        recall_scores.append(rec)
        f1_scores.append(f1)
        
        print(f"Fold {fold} | Train: {X_train.index.min()}–{X_train.index.max()}, "
              f"Test: {X_test.index.min()}–{X_test.index.max()}, "
              f"Acc: {acc:.3f}, AUC: {auc:.3f}, "
              f"Prec: {prec:.3f}, Recall: {rec:.3f}, F1: {f1:.3f}")
    
    return {
        "accuracy": np.nanmean(acc_scores),
        "accuracy_std": np.nanstd(acc_scores),
        "auc": np.nanmean(auc_scores),
        "auc_std": np.nanstd(auc_scores),
        "precision": np.nanmean(prec_scores),
        "precision_std": np.nanstd(prec_scores),
        "recall": np.nanmean(recall_scores),
        "recall_std": np.nanstd(recall_scores),
        "f1": np.nanmean(f1_scores),
        "f1_std": np.nanstd(f1_scores)
    }
    
from sklearn.metrics import accuracy_score, roc_auc_score, precision_score, recall_score, f1_score

def evaluate_predictions(y_true, y_pred_proba):
    y_true = np.asarray(y_true)
    y_pred_proba = np.asarray(y_pred_proba)
    y_pred_class = (y_pred_proba >= 0.5).astype(int)

    high_mask = y_true == 1
    low_mask = y_true == 0

    def metrics(y_true_part, y_pred_part):
        return {
            "rmse": np.sqrt(mean_squared_error(y_true_part, y_pred_part)),
            "mae": mean_absolute_error(y_true_part, y_pred_part),
            "r2": r2_score(y_true_part, y_pred_part),
        }

    results = {
        "accuracy": accuracy_score(y_true, y_pred_class),
        "auc": roc_auc_score(y_true, y_pred_proba),
        "precision": precision_score(y_true, y_pred_class, zero_division=0),
        "recall": recall_score(y_true, y_pred_class, zero_division=0),
        "f1": f1_score(y_true, y_pred_class, zero_division=0),
        "overall": metrics(y_true, y_pred_proba),
        "high_sal": metrics(y_true[high_mask], y_pred_proba[high_mask]),
        "low_sal": metrics(y_true[low_mask], y_pred_proba[low_mask]),
    }

    return results

def print_eval_metrics(name, metrics):
    print(f"\n{name} Metrics:")
    print(f"  Accuracy: {metrics['accuracy']:.4f}")
    print(f"  AUC:      {metrics['auc']:.4f}")
    
    for group in ['overall', 'high_sal', 'low_sal']:
        m = metrics[group]
        print(f"  {group.capitalize()} – RMSE: {m['rmse']:.4f}, MAE: {m['mae']:.4f}, R²: {m['r2']:.4f}")

def run_model(config):
    """
    Main modeling pipeline with improvements
    """
    if isinstance(config, dict):
        config = dict_to_namespace(config)
        
    print("=== TWO STAGE POT MODEL ===")
    
    # Load data
    data, predictors = load_data(config)
    tail_dist_obj = TAIL_DISTS[config.tail_distribution]
    
    # Create target variables
    data['exceed_base'] = (data[config.salinity_col] > config.base_threshold).astype(int)
    data['exceed_target'] = (data[config.salinity_col] > config.target_threshold).astype(int)
    
    print(f"Base threshold ({config.base_threshold}) exceedance rate: {data['exceed_base'].mean():.3f}")
    print(f"Target threshold ({config.target_threshold}) exceedance rate: {data['exceed_target'].mean():.3f}")
    
    # Step 1: Train RF classifier for base threshold exceedance
    print("\n=== STEP 1: CLASSIFICATION STAGE ===")
    X_clf = data[predictors].dropna()
    y_clf = data['exceed_base'].reindex(X_clf.index)
    
    rf_clf = RandomForestClassifier(
        n_estimators=100,
        max_depth=15,
        min_samples_split=5,
        random_state=config.random_state,
        n_jobs=-1
    )
    
    clf_metrics = evaluate_classification(X = X_clf, y = y_clf, model = rf_clf, n_splits=5, gap=24)
    print(f"TimeSeries CV - Accuracy: {clf_metrics['accuracy']:.4f} ± {clf_metrics['accuracy_std']:.4f}, "
      f"AUC: {clf_metrics['auc']:.4f} ± {clf_metrics['auc_std']:.4f}")
    
    # Step 2: Fit tail distribution parameters
    print(f"\n=== STEP 2: TAIL MODELING STAGE ===")
    try:
        params_df = fit_tail_params(data, config, tail_dist_obj)
        print(f"Successfully fitted parameters for {len(params_df)} time periods")
    except Exception as e:
        print(f"Error in tail parameter fitting: {e}")
        return
    
    # print(params_df.head())
    # print(params_df['a'].unique())
    
    data = data.set_index('DateTime').sort_index()
    print(f"Data index range after fix: {data.index.min()} to {data.index.max()}")

    # Step 3: Train parameter regressors
    print(f"\n=== STEP 3: PARAMETER REGRESSION STAGE ===")
    try:
        combined = prepare_regression_data(data, params_df, config)
        X_reg = combined[predictors]
        param_names = tail_dist_obj.param_names()
        y_reg = combined[param_names]
        
        regressors, reg_metrics = train_param_regressors(
            X_reg, y_reg, method=config.param_regression_method, random_state=config.random_state
        )
        
    except Exception as e:
        print(f"Error in parameter regression: {e}")
        return
    
    # Step 4: Generate final predictions
    print(f"\n=== STEP 4: FINAL PREDICTIONS ===")
    try:
        # Extract predictors and target
        X_full = combined[predictors]
        y_target = combined['exceed_target']
        
        # Drop rows with any NaNs in predictors or target
        valid_idx = X_full.join(y_target).dropna().index
        X_full_clean = X_full.loc[valid_idx]
        y_target_actual = y_target.loc[valid_idx]
        
        # Predict parameters using trained regressors
        y_reg_pred = predict_params(regressors, X_full_clean)
        
        # Get base exceedance probabilities from the classifier
        exceed_prob = rf_clf.predict_proba(X_full_clean)[:, 1]
        
        # Calculate tail probabilities using predicted parameters
        excess_target = config.target_threshold - config.base_threshold
        tail_probs = calculate_tail_probabilities(
            y_reg_pred, tail_dist_obj, excess_target, config
        )
        
        # Final hybrid probability = P(base exceedance) * P(tail exceedance | base)
        hybrid_prob = exceed_prob * tail_probs
        
        # Evaluate model
        hybrid_metrics = evaluate_predictions(y_target_actual, hybrid_prob)
        print(f"Hybrid model metrics - Accuracy: {hybrid_metrics['accuracy']:.4f}, AUC: {hybrid_metrics['auc']:.4f}, Precision: {hybrid_metrics['precision']:.4f}, Recall: {hybrid_metrics['recall']:.4f}")
        
        # Compare to direct classification on target exceedance
        rf_direct = RandomForestClassifier(random_state=config.random_state, n_jobs=-1)
        rf_direct.fit(X_full_clean, y_target_actual)
        direct_pred = rf_direct.predict_proba(X_full_clean)[:, 1]
        direct_metrics = evaluate_predictions(y_target_actual, direct_pred)
        print(f"Direct classification baseline - Accuracy: {direct_metrics['accuracy']:.4f}, AUC: {direct_metrics['auc']:.4f}, Precision: {direct_metrics['precision']:.4f}, Recall: {direct_metrics['recall']:.4f}")
        
        # Summary statistics
        print(f"\n=== SUMMARY ===")
        print(f"Hybrid probability range: [{hybrid_prob.min():.4f}, {hybrid_prob.max():.4f}]")
        print(f"Mean hybrid probability: {hybrid_prob.mean():.4f}")
        print(f"Actual exceedance rate: {y_target_actual.mean():.4f}")
        print_eval_metrics("Hybrid", hybrid_metrics)
        print_eval_metrics("Direct", direct_metrics)
        
        # Format hybrid_prob into a DataFrame for export
        if isinstance(hybrid_prob, np.ndarray):
            hybrid_prob = pd.Series(hybrid_prob, index=valid_idx, name="hybrid_prob")
    
        if isinstance(hybrid_prob, pd.Series):
            hybrid_prob_df = hybrid_prob.reset_index().rename(columns={"index": "DateTime"})
        elif isinstance(hybrid_prob, pd.DataFrame):
            hybrid_prob_df = hybrid_prob.reset_index()
            if "index" in hybrid_prob_df.columns:
                hybrid_prob_df = hybrid_prob_df.rename(columns={"index": "DateTime"})
        else:
            raise TypeError("Unexpected hybrid_prob type")
    
        # Optional: convert tail_probs to series if needed elsewhere
        if isinstance(tail_probs, np.ndarray):
            tail_probs = pd.Series(tail_probs, index=valid_idx, name="tail_prob")
        
        return {
            'rf_classifier': rf_clf,
            'param_regressors': regressors,
            'tail_distribution': tail_dist_obj,
            'hybrid_predictions': hybrid_prob_df,
            'metrics': {
                'classification': clf_metrics,
                'hybrid': hybrid_metrics,
                'direct_baseline': direct_metrics,
                'parameter_regression': reg_metrics
            }
        }
    
    except Exception as e:
        print(f"Error in final prediction stage: {e}")
        raise

# if __name__ == "__main__":
#     # Run with improved configuration
#     config = Config()
#     results = run_model(config)

