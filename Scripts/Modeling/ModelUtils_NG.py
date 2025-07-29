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
from ngboost import NGBoost
from ngboost.distns import Normal, LogNormal, Gamma
from ngboost.scores import LogScore, CRPS
from ngboost.learners import default_tree_learner
from sklearn.model_selection import ParameterGrid
import joblib
import os
from datetime import datetime
import warnings

class NGBoostModelTrainer:
    """Handle NGBoost model training and hyperparameter optimization"""
    
    def __init__(self, config):
        self.config = config
        self.best_params = None
        self.best_score = None
        self.trained_models = {}
    
    def create_ngboost_model(self, distribution, scoring, hyperparams):
        """Create NGBoost model with specified parameters"""
        
        # Get distribution class
        if isinstance(distribution, str):
            distribution = self.config['distributions'][distribution]
        
        # Get scoring function
        if isinstance(scoring, str):
            scoring = self.config['scoring_functions'][scoring]
        
        # Combine base params with hyperparams
        model_params = {**self.config['base_params'], **hyperparams}
        
        # Set up base learner
        n_jobs = self.config['parallel_config'].get('ngboost_n_jobs', 1)
        base_learner = default_tree_learner
        
        # Create model
        model = NGBoost(
            Dist=distribution,
            Score=scoring,
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
            # Train the model
            if X_val is not None and y_val is not None:
                model.fit(X_train, y_train, X_val=X_val, Y_val=y_val)
            else:
                model.fit(X_train, y_train)
            
            return model, True, None
            
        except Exception as e:
            return None, False, str(e)
    
    def predict_with_uncertainty(self, model, X):
        """Make predictions with uncertainty estimates"""
        
        try:
            # Get distributional predictions
            y_dists = model.pred_dist(X)
            
            # Extract mean and std
            y_pred = y_dists.mean()
            y_std = y_dists.scale  # or y_dists.var()**0.5 depending on distribution
            
            # Get prediction intervals (5th and 95th percentiles)
            y_lower = y_dists.ppf(0.05)
            y_upper = y_dists.ppf(0.95)
            
            return {
                'mean': y_pred,
                'std': y_std,
                'lower_90': y_lower,
                'upper_90': y_upper,
                'distributions': y_dists
            }
            
        except Exception as e:
            print(f"Prediction error: {e}")
            return None

class NGBoostCrossValidator:
    """Handle cross-validation for NGBoost models"""
    
    def __init__(self, cv_splitter, data_processor):
        self.cv_splitter = cv_splitter
        self.data_processor = data_processor
        
    def cross_validate_model(self, model_config, X, y, metrics_calculator):
        """Perform cross-validation for a single model configuration"""
        
        from DataUtils_NG import calculate_salinity_metrics
        
        cv_results = {
            'fold_results': [],
            'mean_metrics': {},
            'std_metrics': {},
            'model_config': model_config
        }
        
        print(f"Starting CV for {model_config}")
        
        for fold_idx, (train_idx, test_idx) in enumerate(self.cv_splitter.split(X, y)):
            
            print(f"  Fold {fold_idx + 1}/{self.cv_splitter.cv_config['n_splits']}")
            
            # Split data
            X_train, X_test = X.iloc[train_idx], X.iloc[test_idx] 
            y_train, y_test = y.iloc[train_idx], y.iloc[test_idx]
            
            # Scale features
            X_train_scaled, X_test_scaled = self.data_processor.scale_features(X_train, X_test)
            
            # Create and train model
            trainer = NGBoostModelTrainer(model_config)
            model = trainer.create_ngboost_model(
                distribution=model_config['distribution'],
                scoring=model_config['scoring'],
                hyperparams=model_config['hyperparams']
            )
            
            # Train model
            trained_model, success, error = trainer.train_single_model(model, X_train_scaled, y_train)
            
            if not success:
                print(f"    Training failed: {error}")
                continue
            
            # Make predictions
            predictions = trainer.predict_with_uncertainty(trained_model, X_test_scaled)
            
            if predictions is None:
                print(f"    Prediction failed")
                continue
            
            # Calculate metrics
            fold_metrics = calculate_salinity_metrics(
                y_test.values, 
                predictions['mean'], 
                predictions['std'],
                thresholds=model_config.get('salinity_thresholds', {'moderate': 0.3, 'high': 0.5, 'extreme': 1.0})
            )
            
            # Store fold results
            fold_result = {
                'fold': fold_idx,
                'metrics': fold_metrics,
                'train_size': len(train_idx),
                'test_size': len(test_idx)
            }
            
            cv_results['fold_results'].append(fold_result)
            
            print(f"    R²: {fold_metrics['r2']:.4f}, RMSE: {fold_metrics['rmse']:.4f}")
        
        # Calculate mean and std of metrics across folds
        if cv_results['fold_results']:
            all_metrics = [fold['metrics'] for fold in cv_results['fold_results']]
            metric_names = all_metrics[0].keys()
            
            for metric in metric_names:
                values = [m[metric] for m in all_metrics if not np.isnan(m[metric])]
                cv_results['mean_metrics'][metric] = np.mean(values)
                cv_results['std_metrics'][metric] = np.std(values)
        
        return cv_results

class NGBoostHyperparameterOptimizer:
    """Optimize hyperparameters for NGBoost models"""
    
    def __init__(self, config):
        self.config = config
        self.results = []
    
    def optimize_hyperparameters(self, X, y, cv_splitter, data_processor):
        """Run hyperparameter optimization"""
        
        # Get hyperparameter grid
        hyperparam_grid = self.config['hyperparameter_grid']
        distributions = self.config['distributions']
        scoring_functions = self.config.get('scoring_functions', ['LogScore'])
        
        # Create parameter combinations
        all_combinations = []
        
        for distribution in distributions:
            for scoring in scoring_functions:
                for hyperparams in ParameterGrid(hyperparam_grid):
                    combination = {
                        'distribution': distribution,
                        'scoring': scoring,
                        'hyperparams': hyperparams,
                        'salinity_thresholds': self.config.get('salinity_thresholds', {}),
                        **self.config  # Include other config items
                    }
                    all_combinations.append(combination)
        
        print(f"Testing {len(all_combinations)} hyperparameter combinations")
        
        # Cross-validate each combination
        cv_validator = NGBoostCrossValidator(cv_splitter, data_processor)
        
        for i, combination in enumerate(all_combinations):
            print(f"\nCombination {i+1}/{len(all_combinations)}")
            print(f"Distribution: {combination['distribution']}")
            print(f"Hyperparams: {combination['hyperparams']}")
            
            cv_results = cv_validator.cross_validate_model(combination, X, y, None)
            cv_results['combination_id'] = i
            
            self.results.append(cv_results)
        
        # Find best combination
        self._find_best_combination()
        
        return self.results
    
    def _find_best_combination(self):
        """Find the best hyperparameter combination based on CV results"""
        
        if not self.results:
            return
        
        # Score based on R² (could be made configurable)
        valid_results = [r for r in self.results if r['mean_metrics']]
        
        if not valid_results:
            print("No valid results found")
            return
        
        # Find best based on R² 
        best_result = max(valid_results, key=lambda x: x['mean_metrics'].get('r2', -np.inf))
        
        self.best_params = best_result['model_config']
        self.best_score = best_result['mean_metrics']['r2']
        
        print(f"\nBest combination found:")
        print(f"R² = {self.best_score:.4f}")
        print(f"Distribution: {self.best_params['distribution']}")
        print(f"Hyperparams: {self.best_params['hyperparams']}")
    
    def get_best_params(self):
        """Get the best hyperparameters found"""
        return self.best_params, self.best_score
    
    def save_results(self, filepath):
        """Save optimization results to file"""
        
        # Convert results to serializable format
        serializable_results = []
        for result in self.results:
            serializable_result = {
                'combination_id': result['combination_id'],
                'model_config': result['model_config'],
                'mean_metrics': result['mean_metrics'],
                'std_metrics': result['std_metrics'],
                'n_folds': len(result['fold_results'])
            }
            serializable_results.append(serializable_result)
        
        # Save as pickle for full results, JSON for summary
        joblib.dump(self.results, filepath.replace('.json', '_full.pkl'))
        
        import json
        with open(filepath, 'w') as f:
            json.dump(serializable_results, f, indent=2, default=str)
