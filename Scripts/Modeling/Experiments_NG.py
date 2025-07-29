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
import os
import json
import pandas as pd
from datetime import datetime
import argparse

# Import your modules
from Config_NG import *
from DataUtils_NG import *
from ModelUtils_NG import *

class NGBoostExperimentRunner:
    """Manage and run NGBoost experiments"""
    
    def __init__(self, config_dict):
        self.config = config_dict
        self.results_dir = config_dict.get('results_dir', RESULTS_DIR)
        self.models_dir = config_dict.get('models_dir', MODELS_DIR)
        
        # Create experiment-specific directories
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        self.experiment_dir = os.path.join(self.results_dir, f"{timestamp}_{config_dict['name']}")
        os.makedirs(self.experiment_dir, exist_ok=True)
        
        # Initialize components
        self.data_processor = None
        self.cv_splitter = None
        self.optimizer = None
        
    def setup_experiment(self):
        """Initialize data processor and CV splitter"""
        
        print(f"Setting up experiment: {self.config['name']}")
        print(f"Results will be saved to: {self.experiment_dir}")
        
        # Setup data processor
        data_config = {
            'data_path': DATA_PATH,
            'data_config': DATA_CONFIG,
            'holdout_events': HOLDOUT_EVENTS if self.config.get('include_2016_holdout', False) else {}
        }
        
        self.data_processor = SalinityDataProcessor(data_config)
        
        # Setup CV splitter  
        self.cv_splitter = SalinityTimeSeriesCV(CV_CONFIG)
        
        # Load and prepare data
        self.data_processor.load_data()
        
    def run_experiment(self):
        """Execute the full experiment"""
        
        self.setup_experiment()
        
        # Prepare features and target
        X, y = self.data_processor.prepare_features_target(SELECTED_PREDICTORS, TARGET_VARIABLE)
        
        # Create holdout split if specified
        X_train, y_train, X_holdout, y_holdout = self.data_processor.create_holdout_split(X, y)
        
        # Run hyperparameter optimization
        print("\nStarting hyperparameter optimization...")
        
        optimizer_config = {
            'hyperparameter_grid': self.config['hyperparameter_grid'],
            'distributions': self.config['distributions'],
            'scoring_functions': SCORING_FUNCTIONS,
            'salinity_thresholds': SALINITY_THRESHOLDS,
            **BASE_PARAMS,
            **PARALLEL_CONFIG
        }
        
        self.optimizer = NGBoostHyperparameterOptimizer(optimizer_config)
        results = self.optimizer.optimize_hyperparameters(X_train, y_train, self.cv_splitter, self.data_processor)
        
        # Save optimization results
        results_file = os.path.join(self.experiment_dir, 'hyperparameter_results.json')
        self.optimizer.save_results(results_file)
        
        # Train final model with best parameters if holdout exists
        if X_holdout is not None:
            print("\nTraining final model and evaluating on holdout...")
            self._evaluate_on_holdout(X_train, y_train, X_holdout, y_holdout)
        
        # Save experiment summary
        self._save_experiment_summary()
        
        print(f"\nExperiment completed. Results saved to: {self.experiment_dir}")
        
        return results
    
    def _evaluate_on_holdout(self, X_train, y_train, X_holdout, y_holdout):
        """Evaluate best model on holdout data (2016 event)"""
        
        from DataUtils_NG import calculate_salinity_metrics
        
        # Get best parameters
        best_params, best_score = self.optimizer.get_best_params()
        
        if best_params is None:
            print("No best parameters found, skipping holdout evaluation")
            return
        
        # Scale all training data
        X_train_scaled, _ = self.data_processor.scale_features(X_train)
        X_holdout_scaled, _ = self.data_processor.scale_features(X_holdout)
        
        # Train final model on all training data
        trainer = NGBoostModelTrainer(best_params)
        final_model = trainer.create_ngboost_model(
            distribution=best_params['distribution'],
            scoring=best_params['scoring'], 
            hyperparams=best_params['hyperparams']
        )
        
        print("Training final model on full training set...")
        trained_model, success, error = trainer.train_single_model(final_model, X_train_scaled, y_train)
        
        if not success:
            print(f"Final model training failed: {error}")
            return
        
        # Predict on holdout
        print("Evaluating on holdout data (2016 event)...")
        predictions = trainer.predict_with_uncertainty(trained_model, X_holdout_scaled)
        
        if predictions is None:
            print("Holdout prediction failed")
            return
        
        # Calculate holdout metrics
        holdout_metrics = calculate_salinity_metrics(
            y_holdout.values,
            predictions['mean'],
            predictions['std'],
            thresholds=SALINITY_THRESHOLDS
        )
        
        # Save holdout results
        holdout_results = {
            'metrics': holdout_metrics,
            'best_params': best_params,
            'holdout_size': len(y_holdout),
            'max_salinity_observed': float(y_holdout.max()),
            'max_salinity_predicted': float(predictions['mean'].max())
        }
        
        holdout_file = os.path.join(self.experiment_dir, 'holdout_evaluation.json')
        with open(holdout_file, 'w') as f:
            json.dump(holdout_results, f, indent=2, default=str)
        
        # Save predictions for analysis
        predictions_df = pd.DataFrame({
            'DateTime': X_holdout.index,
            'Observed': y_holdout.values,
            'Predicted_Mean': predictions['mean'],
            'Predicted_Std': predictions['std'],
            'Lower_90': predictions['lower_90'],
            'Upper_90': predictions['upper_90']
        })
        
        predictions_file = os.path.join(self.experiment_dir, 'holdout_predictions.csv')
        predictions_df.to_csv(predictions_file, index=False)
        
        print(f"Holdout evaluation complete:")
        print(f"  R² = {holdout_metrics['r2']:.4f}")
        print(f"  RMSE = {holdout_metrics['rmse']:.4f}")
        print(f"  High salinity precision = {holdout_metrics['high_salinity_precision']:.4f}")
        print(f"  High salinity recall = {holdout_metrics['high_salinity_recall']:.4f}")
        
        # Save the final trained model
        import joblib
        model_file = os.path.join(self.experiment_dir, 'final_model.pkl')
        joblib.dump(trained_model, model_file)
        print(f"Final model saved to: {model_file}")
    
    def _save_experiment_summary(self):
        """Save a summary of the experiment configuration and results"""
        
        best_params, best_score = self.optimizer.get_best_params() if self.optimizer else (None, None)
        
        summary = {
            'experiment_name': self.config['name'],
            'experiment_type': self.config.get('description', ''),
            'timestamp': datetime.now().isoformat(),
            'configuration': {
                'predictors': SELECTED_PREDICTORS,
                'target': TARGET_VARIABLE,
                'hyperparameter_grid': self.config['hyperparameter_grid'],
                'distributions_tested': self.config['distributions'],
                'cv_config': CV_CONFIG,
                'data_config': DATA_CONFIG
            },
            'results': {
                'best_cv_score': best_score,
                'best_parameters': best_params,
                'total_combinations_tested': len(self.optimizer.results) if self.optimizer else 0
            }
        }
        
        summary_file = os.path.join(self.experiment_dir, 'experiment_summary.json')
        with open(summary_file, 'w') as f:
            json.dump(summary, f, indent=2, default=str)

def get_experiment_config(experiment_name):
    """Get configuration for a specific experiment"""
    if experiment_name not in EXPERIMENTS:
        raise ValueError(f"Unknown experiment: {experiment_name}")
    
    config = EXPERIMENTS[experiment_name].copy()
    config['name'] = experiment_name
    
    # Add shared configurations
    config.update({
        'base_params': BASE_PARAMS,
        'cv_config': CV_CONFIG,
        'metrics': CORE_METRICS,
        'parallel_config': PARALLEL_CONFIG,
        'data_config': DATA_CONFIG,
        'results_config': RESULTS_CONFIG,
        'salinity_thresholds': SALINITY_THRESHOLDS
    })
    
    return config

def run_single_experiment(experiment_name):
    """Run a single experiment by name"""
    
    print(f"\n{'='*60}")
    print(f"Running Experiment: {experiment_name}")
    print(f"{'='*60}")
    
    # Get experiment configuration
    config = get_experiment_config(experiment_name)
    
    # Create and run experiment
    runner = NGBoostExperimentRunner(config)
    results = runner.run_experiment()
    
    return runner, results

def run_workflow(workflow=None):
    """Run a sequence of experiments"""
    
    if workflow is None:
        workflow = RECOMMENDED_WORKFLOW
    
    print(f"\n{'='*60}")
    print(f"Running Experiment Workflow")
    print(f"{'='*60}")
    print(f"Experiments to run: {workflow}")
    
    all_results = {}
    
    for experiment_name in workflow:
        try:
            runner, results = run_single_experiment(experiment_name)
            all_results[experiment_name] = {
                'runner': runner,
                'results': results,
                'status': 'completed'
            }
            
        except Exception as e:
            print(f"ERROR in experiment {experiment_name}: {str(e)}")
            all_results[experiment_name] = {
                'runner': None,
                'results': None,
                'status': 'failed',
                'error': str(e)
            }
            
            # Ask user if they want to continue
            response = input(f"Continue with remaining experiments? (y/n): ")
            if response.lower() != 'y':
                break
    
    # Print workflow summary
    print(f"\n{'='*60}")
    print("Workflow Summary")
    print(f"{'='*60}")
    
    for exp_name, result in all_results.items():
        status = result['status']
        if status == 'completed':
            best_score = result['runner'].optimizer.best_score if result['runner'].optimizer else 'N/A'
            print(f"{exp_name}: {status} (Best CV R² = {best_score})")
        else:
            print(f"{exp_name}: {status}")
    
    return all_results

def print_experiment_info(experiment_name):
    """Print information about an experiment"""
    if experiment_name not in EXPERIMENTS:
        print(f"Unknown experiment: {experiment_name}")
        return
    
    exp = EXPERIMENTS[experiment_name]
    print(f"\nExperiment: {experiment_name}")
    print(f"Description: {exp['description']}")
    print(f"Distributions: {exp['distributions']}")
    print(f"Number of runs: {exp['n_runs']}")
    
    if isinstance(exp['hyperparameter_grid'], dict):
        n_combinations = 1
        for param, values in exp['hyperparameter_grid'].items():
            n_combinations *= len(values)
        print(f"Hyperparameter combinations: {n_combinations}")
        
        estimated_models = n_combinations * len(exp['distributions']) * exp['n_runs'] * CV_CONFIG['n_splits']
        print(f"Estimated models to train: {estimated_models}")

def list_available_experiments():
    """List all available experiments"""
    print("Available experiments:")
    for name, config in EXPERIMENTS.items():
        print(f"  {name}: {config['description']}")

def print_recommended_workflow():
    """Print the recommended experimental workflow"""
    print("Recommended workflow:")
    for i, experiment in enumerate(RECOMMENDED_WORKFLOW, 1):
        desc = EXPERIMENTS[experiment]['description']
        print(f"{i}. {experiment}: {desc}")

# # Command line interface
# def main():
#     """Command line interface for running experiments"""
#     
#     parser = argparse.ArgumentParser(description='Run NGBoost salinity prediction experiments')
#     parser.add_argument('command', choices=['list', 'info', 'run', 'workflow'], 
#                        help='Command to execute')
#     parser.add_argument('--experiment', '-e', type=str, 
#                        help='Experiment name (for run/info commands)')
#     parser.add_argument('--workflow', '-w', nargs='+', 
#                        help='Custom workflow (list of experiment names)')
#     
#     args = parser.parse_args()
#     
#     if args.command == 'list':
#         list_available_experiments()
#         
#     elif args.command == 'info':
#         if not args.experiment:
#             print("Please specify experiment name with --experiment")
#             return
#         print_experiment_info(args.experiment)
#         
#     elif args.command == 'run':
#         if not args.experiment:
#             print("Please specify experiment name with --experiment")
#             return
#         run_single_experiment(args.experiment)
#         
#     elif args.command == 'workflow':
#         workflow = args.workflow if args.workflow else RECOMMENDED_WORKFLOW
#         run_workflow(workflow)
# 
# if __name__ == "__main__":
#     main()
