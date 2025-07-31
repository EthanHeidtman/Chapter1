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
from ngboost import NGBRegressor
from ngboost.distns import Normal, LogNormal, Gamma
#from ngboost.scores import LogScore, CRPS
from ngboost.learners import default_tree_learner
from sklearn.model_selection import ParameterGrid
from sklearn.ensemble import RandomForestRegressor
from sklearn.linear_model import LinearRegression
import joblib
import os
import json
from datetime import datetime
import warnings

class NGBoostModelTrainer:
    """Handle NGBoost model training and hyperparameter optimization"""
    
    def __init__(self, distributions, base_params, parallel_config):  # Removed scoring_functions
        """
        Initialize with config parameters
        
        Parameters:
        -----------
        distributions : dict
            Distribution classes (from Config_NG.DISTRIBUTIONS)
        base_params : dict
            Base model parameters (from Config_NG.BASE_PARAMS)
        parallel_config : dict
            Parallel processing config (from Config_NG.PARALLEL_CONFIG)
        """
        self.distributions = distributions
        self.base_params = base_params
        self.parallel_config = parallel_config
        self.best_params = None
        self.best_score = None
        self.trained_models = {}
    
    def create_ngboost_model(self, distribution, hyperparams):  # Removed scoring parameter
        """Create NGBoost model with specified parameters"""
        
        # Get distribution class
        if isinstance(distribution, str):
            if distribution not in self.distributions:
                raise ValueError(f"Unknown distribution: {distribution}. Available: {list(self.distributions.keys())}")
            distribution_class = self.distributions[distribution]
        else:
            distribution_class = distribution
        
        # Combine base params with hyperparams
        model_params = {**self.base_params, **hyperparams}
        
        # Set up base learner with parallel processing
        n_jobs = self.parallel_config.get('ngboost_n_jobs', 1)
        base_learner = default_tree_learner
        
        print(f"Creating NGBoost model: {distribution} distribution")
        print(f"Parameters: n_estimators={model_params['n_estimators']}, "
              f"learning_rate={model_params['learning_rate']}")
        
        # Create model (NGBoost will use default LogScore)
        model = NGBRegressor(
            Dist=distribution_class,
            Base=base_learner,
            n_estimators=model_params['n_estimators'],
            learning_rate=model_params['learning_rate'],
            minibatch_frac=model_params.get('minibatch_frac', 1.0),
            col_sample=model_params.get('col_sample', 1.0),
            verbose=model_params.get('verbose', False),
            random_state=model_params.get('random_state', 42),
            tol=model_params.get('tol', 1e-5)
        )
        
        return model
    
    def train_single_model(self, model, X_train, y_train, X_val=None, y_val=None):
        """Train a single NGBoost model"""
        
        try:
            print(f"Training model on {len(X_train)} samples...")
            
            # Train the model
            if X_val is not None and y_val is not None:
                print(f"Using validation set with {len(X_val)} samples")
                model.fit(X_train, y_train, X_val=X_val, Y_val=y_val)
            else:
                model.fit(X_train, y_train)
            
            print("Training completed successfully")
            return model, True, None
            
        except Exception as e:
            print(f"Training failed: {str(e)}")
            return None, False, str(e)
    
    def predict_with_uncertainty(self, model, X):
        """Make predictions with uncertainty estimates"""
        
        try:
            print(f"Making predictions for {len(X)} samples...")
            
            # Get distributional predictions
            y_dists = model.pred_dist(X)
            
            # Extract statistics based on distribution type
            y_pred = y_dists.mean()
            
            # Handle different distribution types for scale parameter
            if hasattr(y_dists, 'scale'):
                y_std = y_dists.scale
            elif hasattr(y_dists, 'var'):
                y_std = np.sqrt(y_dists.var())
            else:
                # Fallback - estimate from quantiles
                y_std = (y_dists.ppf(0.84) - y_dists.ppf(0.16)) / 2
            
            # Get prediction intervals
            y_lower_50 = y_dists.ppf(0.25)  # 50% interval
            y_upper_50 = y_dists.ppf(0.75)
            y_lower_90 = y_dists.ppf(0.05)  # 90% interval
            y_upper_90 = y_dists.ppf(0.95)
            y_lower_95 = y_dists.ppf(0.025) # 95% interval
            y_upper_95 = y_dists.ppf(0.975)
            
            predictions = {
                'mean': y_pred,
                'std': y_std,
                'lower_50': y_lower_50,
                'upper_50': y_upper_50,
                'lower_90': y_lower_90,
                'upper_90': y_upper_90,
                'lower_95': y_lower_95,
                'upper_95': y_upper_95,
                'distributions': y_dists  # Keep full distributions for post-hoc scoring
            }
            
            print("Predictions completed successfully")
            return predictions
            
        except Exception as e:
            print(f"Prediction error: {e}")
            return None

class NGBoostCrossValidator:
    """Handle cross-validation for NGBoost models"""
    
    def __init__(self, cv_splitter, data_processor, salinity_thresholds):
        """
        Initialize cross-validator
        
        Parameters:
        -----------
        cv_splitter : SalinityTimeSeriesCV
            Cross-validation splitter
        data_processor : SalinityDataProcessor
            Data preprocessing handler
        salinity_thresholds : dict
            Salinity thresholds for metrics (from Config_NG.SALINITY_THRESHOLDS)
        """
        self.cv_splitter = cv_splitter
        self.data_processor = data_processor
        self.salinity_thresholds = salinity_thresholds
    
    def cross_validate_model(self, trainer, distribution, hyperparams, X, y):  # Removed scoring parameter
        """Perform cross-validation for a single model configuration"""
        
        from DataUtils_NG import calculate_salinity_metrics
        
        cv_results = {
            'fold_results': [],
            'mean_metrics': {},
            'std_metrics': {},
            'model_config': {
                'distribution': distribution,
                'hyperparams': hyperparams
            }
        }
        
        print(f"\nStarting CV for {distribution} distribution")
        print(f"Hyperparameters: {hyperparams}")
        
        successful_folds = 0
        
        for fold_idx, (train_idx, test_idx) in enumerate(self.cv_splitter.split(X, y)):
            
            print(f"  Fold {fold_idx + 1}/{self.cv_splitter.cv_config['n_splits']}")
            
            try:
                # Split data
                X_train, X_test = X.iloc[train_idx], X.iloc[test_idx] 
                y_train, y_test = y.iloc[train_idx], y.iloc[test_idx]
                
                # Scale features
                X_train_scaled, X_test_scaled = self.data_processor.scale_features(X_train, X_test)
                
                # Create and train model
                model = trainer.create_ngboost_model(distribution, hyperparams)
                trained_model, success, error = trainer.train_single_model(model, X_train_scaled, y_train)
                
                if not success:
                    print(f"    Training failed: {error}")
                    continue
                
                # Make predictions
                predictions = trainer.predict_with_uncertainty(trained_model, X_test_scaled)
                
                if predictions is None:
                    print(f"    Prediction failed")
                    continue
                
                # Calculate metrics with distribution objects for accurate scoring
                fold_metrics = calculate_salinity_metrics(
                    y_test.values, 
                    predictions['mean'], 
                    predictions['std'],
                    y_pred_dist=predictions['distributions'],  # Pass full distributions
                    salinity_thresholds=self.salinity_thresholds
                )
                
                # Get fold info
                fold_info = self.cv_splitter.get_fold_info(X, fold_idx, train_idx, test_idx)
                
                # Store fold results
                fold_result = {
                    'fold': fold_idx,
                    'metrics': fold_metrics,
                    'fold_info': fold_info,
                    'predictions_sample': {
                        'y_true': y_test.values[:10].tolist(),  # First 10 for inspection
                        'y_pred': predictions['mean'][:10].tolist(),
                        'y_std': predictions['std'][:10].tolist()
                    }
                }
                
                cv_results['fold_results'].append(fold_result)
                successful_folds += 1
                
                print(f"    R²: {fold_metrics['r2']:.4f}, RMSE: {fold_metrics['rmse']:.4f}, "
                      f"LogLik: {fold_metrics.get('log_likelihood', 'N/A')}, "
                      f"CRPS: {fold_metrics.get('crps_score', 'N/A')}")
                
            except Exception as e:
                print(f"    Fold {fold_idx + 1} failed: {str(e)}")
                continue
        
        print(f"  Completed {successful_folds}/{self.cv_splitter.cv_config['n_splits']} folds successfully")
        
        # Calculate mean and std of metrics across folds
        if cv_results['fold_results']:
            all_metrics = [fold['metrics'] for fold in cv_results['fold_results']]
            metric_names = all_metrics[0].keys()
            
            for metric in metric_names:
                values = [m[metric] for m in all_metrics if not (np.isnan(m[metric]) if isinstance(m[metric], float) else False)]
                if values:
                    cv_results['mean_metrics'][metric] = np.mean(values)
                    cv_results['std_metrics'][metric] = np.std(values)
                else:
                    cv_results['mean_metrics'][metric] = np.nan
                    cv_results['std_metrics'][metric] = np.nan
        
        return cv_results
    
class NGBoostHyperparameterOptimizer:
    """Optimize hyperparameters for NGBoost models"""
    
    def __init__(self, distributions, base_params, parallel_config, salinity_thresholds): 
        self.distributions = distributions
        self.base_params = base_params
        self.parallel_config = parallel_config
        self.salinity_thresholds = salinity_thresholds
        self.results = []
        self.best_params = None
        self.best_score = None
    
    def optimize_hyperparameters(self, experiment_config, X, y, cv_splitter, data_processor):
        """Run hyperparameter optimization for a specific experiment"""
        
        # Get experiment configuration
        hyperparam_grid = experiment_config['hyperparameter_grid']
        distributions = experiment_config['distributions']
        
        # Create trainer (no scoring functions needed)
        trainer = NGBoostModelTrainer(
            self.distributions, 
            self.base_params, 
            self.parallel_config
        )
        
        # Create parameter combinations
        all_combinations = []
        
        for distribution in distributions:
            if isinstance(hyperparam_grid, dict):
                for hyperparams in ParameterGrid(hyperparam_grid):
                    combination = {
                        'distribution': distribution,
                        'hyperparams': hyperparams
                    }
                    all_combinations.append(combination)
            else:
                # Handle special case like 'best_params'
                combination = {
                    'distribution': distribution,
                    'hyperparams': hyperparam_grid
                }
                all_combinations.append(combination)
        
        print(f"Testing {len(all_combinations)} hyperparameter combinations")
        
        # Cross-validate each combination
        cv_validator = NGBoostCrossValidator(cv_splitter, data_processor, self.salinity_thresholds)
        
        for i, combination in enumerate(all_combinations):
            print(f"\n{'='*60}")
            print(f"Combination {i+1}/{len(all_combinations)}")
            print(f"Distribution: {combination['distribution']}")
            print(f"Hyperparams: {combination['hyperparams']}")
            print(f"{'='*60}")
            
            cv_results = cv_validator.cross_validate_model(
                trainer,
                combination['distribution'],
                combination['hyperparams'],
                X, y
            )
            cv_results['combination_id'] = i
            
            self.results.append(cv_results)
            
            # Print summary for this combination
            if cv_results['mean_metrics']:
                print(f"\nCombination {i+1} Results:")
                print(f"  R²: {cv_results['mean_metrics'].get('r2', 'N/A'):.4f} "
                      f"(±{cv_results['std_metrics'].get('r2', 0):.4f})")
                print(f"  RMSE: {cv_results['mean_metrics'].get('rmse', 'N/A'):.4f} "
                      f"(±{cv_results['std_metrics'].get('rmse', 0):.4f})")
                print(f"  LogLik: {cv_results['mean_metrics'].get('log_likelihood', 'N/A')}")
                print(f"  CRPS: {cv_results['mean_metrics'].get('crps_score', 'N/A')}")
        
        # Find best combination
        self._find_best_combination()
        
        return self.results
    
    def _find_best_combination(self, primary_metric='r2', secondary_metric='high_sal_r2'):
        """Find the best hyperparameter combination based on CV results"""
        
        if not self.results:
            print("No results available for optimization")
            return
        
        # Filter valid results
        valid_results = [r for r in self.results if r['mean_metrics'] and 
                        not np.isnan(r['mean_metrics'].get(primary_metric, np.nan))]
        
        if not valid_results:
            print("No valid results found")
            return
        
        # Find best based on primary metric (R²)
        best_result = max(valid_results, key=lambda x: x['mean_metrics'].get(primary_metric, -np.inf))
        
        self.best_params = best_result['model_config']
        self.best_score = best_result['mean_metrics'][primary_metric]
        
        print(f"\n{'='*60}")
        print("BEST COMBINATION FOUND:")
        print(f"{'='*60}")
        print(f"Primary metric ({primary_metric}): {self.best_score:.4f}")
        if secondary_metric in best_result['mean_metrics']:
            print(f"Secondary metric ({secondary_metric}): {best_result['mean_metrics'][secondary_metric]:.4f}")
        print(f"Distribution: {self.best_params['distribution']}")
        print(f"Scoring: {self.best_params['scoring']}")
        print(f"Hyperparameters: {self.best_params['hyperparams']}")
        print(f"{'='*60}")
        
    def get_best_params(self):
        """Get the best hyperparameters found"""
        return self.best_params, self.best_score
    
    def save_results(self, experiment_paths, experiment_name):
        """Save optimization results using experiment directory structure"""
        
        # Create timestamped filename
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        # Save full results as pickle
        full_results_path = os.path.join(
            experiment_paths['results'], 
            f"{experiment_name}_hyperopt_full_{timestamp}.pkl"
        )
        joblib.dump(self.results, full_results_path)
        print(f"Full results saved to: {full_results_path}")
        
        # Save summary as JSON
        serializable_results = []
        for result in self.results:
            serializable_result = {
                'combination_id': result['combination_id'],
                'model_config': result['model_config'],
                'mean_metrics': result['mean_metrics'],
                'std_metrics': result['std_metrics'],
                'n_successful_folds': len(result['fold_results'])
            }
            serializable_results.append(serializable_result)
        
        summary_path = os.path.join(
            experiment_paths['results'],
            f"{experiment_name}_hyperopt_summary_{timestamp}.json"
        )
        
        with open(summary_path, 'w') as f:
            json.dump({
                'experiment_name': experiment_name,
                'timestamp': timestamp,
                'best_params': self.best_params,
                'best_score': self.best_score,
                'all_results': serializable_results
            }, f, indent=2, default=str)
        
        print(f"Summary saved to: {summary_path}")
        
        return full_results_path, summary_path

class BaselineModelComparator:
    """Compare NGBoost against baseline models"""
    
    def __init__(self, baseline_models):
        """
        Initialize with baseline model types
        
        Parameters:
        -----------
        baseline_models : list
            List of baseline model names (from Config_NG.BASELINE_MODELS)
        """
        self.baseline_models = baseline_models
        self.trained_baselines = {}
    
    def create_baseline_model(self, model_type, random_state=42):
        """Create baseline model of specified type"""
        
        if model_type == 'linear':
            return LinearRegression()
        elif model_type == 'random_forest':
            return RandomForestRegressor(
                n_estimators=100,
                random_state=random_state,
                n_jobs=-1
            )
        else:
            raise ValueError(f"Unknown baseline model type: {model_type}")
    
    def compare_models(self, X, y, cv_splitter, data_processor, salinity_thresholds):
        """Compare baseline models using the same CV framework"""
        
        from DataUtils_NG import calculate_salinity_metrics
        
        baseline_results = {}
        
        for model_type in self.baseline_models:
            print(f"\nTesting baseline model: {model_type}")
            
            model_results = {
                'fold_results': [],
                'mean_metrics': {},
                'std_metrics': {}
            }
            
            for fold_idx, (train_idx, test_idx) in enumerate(cv_splitter.split(X, y)):
                
                print(f"  Fold {fold_idx + 1}")
                
                try:
                    # Split data
                    X_train, X_test = X.iloc[train_idx], X.iloc[test_idx]
                    y_train, y_test = y.iloc[train_idx], y.iloc[test_idx]
                    
                    # Scale features  
                    X_train_scaled, X_test_scaled = data_processor.scale_features(X_train, X_test)
                    
                    # Create and train baseline model
                    model = self.create_baseline_model(model_type)
                    model.fit(X_train_scaled, y_train)
                    
                    # Make predictions
                    y_pred = model.predict(X_test_scaled)
                    
                    # Calculate metrics (no uncertainty for baseline models)
                    fold_metrics = calculate_salinity_metrics(
                        y_test.values, 
                        y_pred, 
                        y_pred_std=None,  # No uncertainty
                        salinity_thresholds=salinity_thresholds
                    )
                    
                    model_results['fold_results'].append({
                        'fold': fold_idx,
                        'metrics': fold_metrics
                    })
                    
                    print(f"    R²: {fold_metrics['r2']:.4f}, RMSE: {fold_metrics['rmse']:.4f}")
                    
                except Exception as e:
                    print(f"    Fold {fold_idx + 1} failed: {str(e)}")
                    continue
            
            # Calculate mean metrics
            if model_results['fold_results']:
                all_metrics = [fold['metrics'] for fold in model_results['fold_results']]
                metric_names = all_metrics[0].keys()
                
                for metric in metric_names:
                    values = [m[metric] for m in all_metrics if not (np.isnan(m[metric]) if isinstance(m[metric], float) else False)]
                    if values:
                        model_results['mean_metrics'][metric] = np.mean(values)
                        model_results['std_metrics'][metric] = np.std(values)
            
            baseline_results[model_type] = model_results
        
        return baseline_results
