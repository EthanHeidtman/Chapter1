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
import numpy as np
from datetime import datetime
import argparse
import warnings
import joblib

# Import your modules
from Config_NG import *
from DataUtils_NG import *
from ModelUtils_NG import *

class NGBoostExperimentRunner:
    """Manage and run NGBoost experiments using Config_NG structure"""
    
    def __init__(self, experiment_name):
        """
        Initialize experiment runner
        
        Parameters:
        -----------
        experiment_name : str
            Name of experiment from Config_NG.EXPERIMENTS
        """
        self.experiment_name = experiment_name
        
        if experiment_name not in EXPERIMENTS:
            raise ValueError(f"Unknown experiment: {experiment_name}. Available: {list(EXPERIMENTS.keys())}")
        
        self.experiment_config = EXPERIMENTS[experiment_name]
        
        # Create experiment directory structure
        self.experiment_paths = get_experiment_paths(experiment_name)
        
        # Initialize components
        self.data_processor = None
        self.cv_splitter = None
        self.optimizer = None
        self.baseline_comparator = None
        
        print(f"Initialized experiment: {experiment_name}")
        print(f"Description: {self.experiment_config['description']}")
        print(f"Experiment directory: {self.experiment_paths['base']}")
    
    def setup_experiment(self):
        """Initialize data processor, CV splitter, and other components"""
        
        print(f"\nSetting up experiment: {self.experiment_name}")
        
        # Setup data processor with config parameters
        self.data_processor = SalinityDataProcessor(
            data_path=DATA_PATH,
            selected_predictors=SELECTED_PREDICTORS,
            target_variable=TARGET_VARIABLE,
            data_config=DATA_CONFIG,
            holdout_events=HOLDOUT_EVENTS if self.experiment_config.get('include_2016_holdout', False) else None
        )
        
        # Setup CV splitter  
        self.cv_splitter = SalinityTimeSeriesCV(CV_CONFIG)
        
        # Setup optimizer (removed scoring_functions parameter)
        self.optimizer = NGBoostHyperparameterOptimizer(
            distributions=DISTRIBUTIONS,
            base_params=BASE_PARAMS,
            parallel_config=PARALLEL_CONFIG,
            salinity_thresholds=SALINITY_THRESHOLDS
        )
        
        # Setup baseline comparator if specified
        if BASELINE_MODELS:
            self.baseline_comparator = BaselineModelComparator(BASELINE_MODELS)
        
        print("✓ All components initialized")
        
    def load_and_prepare_data(self):
        """Load and prepare data for modeling"""
        
        print(f"\nLoading and preparing data...")
        
        # Load data
        data = self.data_processor.load_data()
        
        # Data quality check
        quality_report = check_data_quality(data, SELECTED_PREDICTORS, TARGET_VARIABLE)
        print(f"Data quality report:")
        print(f"  Total rows: {quality_report['total_rows']}")
        print(f"  Target range: {quality_report['target_stats']['min']:.3f} to {quality_report['target_stats']['max']:.3f}")
        print(f"  Missing data points: {sum([info['count'] for info in quality_report['missing_data'].values()])}")
        
        # Prepare features and target
        X, y = self.data_processor.prepare_features_target()
        
        # Create holdout split if specified
        X_train, y_train, X_holdout, y_holdout = self.data_processor.create_holdout_split(X, y)
        
        # Save data quality report
        quality_file = os.path.join(self.experiment_paths['results'], f'data_quality_report.json')
        with open(quality_file, 'w') as f:
            json.dump(quality_report, f, indent=2, default=str)
        
        return X_train, y_train, X_holdout, y_holdout
    
    def run_hyperparameter_optimization(self, X_train, y_train):
        """Run hyperparameter optimization phase"""
        
        print(f"\n{'='*60}")
        print("HYPERPARAMETER OPTIMIZATION")
        print(f"{'='*60}")
        
        # Run optimization
        results = self.optimizer.optimize_hyperparameters(
            experiment_config=self.experiment_config,
            X=X_train, 
            y=y_train,
            cv_splitter=self.cv_splitter,
            data_processor=self.data_processor
        )
        
        # Save optimization results
        self.optimizer.save_results(self.experiment_paths, self.experiment_name)
        
        return results
    
    def run_baseline_comparison(self, X_train, y_train):
        """Compare against baseline models"""
        
        if self.baseline_comparator is None:
            print("No baseline models specified, skipping comparison")
            return None
            
        print(f"\n{'='*60}")
        print("BASELINE MODEL COMPARISON")
        print(f"{'='*60}")
        
        baseline_results = self.baseline_comparator.compare_models(
            X=X_train,
            y=y_train,
            cv_splitter=self.cv_splitter,
            data_processor=self.data_processor,
            salinity_thresholds=SALINITY_THRESHOLDS
        )
        
        # Save baseline results
        baseline_file = os.path.join(self.experiment_paths['results'], 'baseline_comparison.json')
        
        # Convert to serializable format
        serializable_baselines = {}
        for model_type, results in baseline_results.items():
            serializable_baselines[model_type] = {
                'mean_metrics': results['mean_metrics'],
                'std_metrics': results['std_metrics'],
                'n_folds': len(results['fold_results'])
            }
        
        with open(baseline_file, 'w') as f:
            json.dump(serializable_baselines, f, indent=2, default=str)
        
        # Print comparison summary
        print(f"\nBaseline Comparison Summary:")
        best_params, ngboost_score = self.optimizer.get_best_params()
        print(f"NGBoost (best):     R² = {ngboost_score:.4f}")
        
        for model_type, results in baseline_results.items():
            r2_score = results['mean_metrics'].get('r2', 0)
            print(f"{model_type.title():15} R² = {r2_score:.4f}")
        
        return baseline_results
    
    def evaluate_on_holdout(self, X_train, y_train, X_holdout, y_holdout):
        """Evaluate best model on holdout data (2016 extreme event)"""
        
        if X_holdout is None or len(X_holdout) == 0:
            print("No holdout data available, skipping holdout evaluation")
            return None
            
        print(f"\n{'='*60}")
        print("HOLDOUT EVALUATION (2016 EXTREME EVENT)")
        print(f"{'='*60}")
        
        # Get best parameters
        best_params, best_score = self.optimizer.get_best_params()
        
        if best_params is None:
            print("No best parameters found, cannot evaluate holdout")
            return None
        
        # Create trainer (removed scoring_functions parameter)
        trainer = NGBoostModelTrainer(
            distributions=DISTRIBUTIONS,
            base_params=BASE_PARAMS,
            parallel_config=PARALLEL_CONFIG
        )
        
        # Scale training data
        X_train_scaled, _ = self.data_processor.scale_features(X_train)
        
        # Train final model on all training data (removed scoring parameter)
        final_model = trainer.create_ngboost_model(
            distribution=best_params['distribution'],
            hyperparams=best_params['hyperparams']
        )
        
        print("Training final model on full training set...")
        trained_model, success, error = trainer.train_single_model(final_model, X_train_scaled, y_train)
        
        if not success:
            print(f"Final model training failed: {error}")
            return None
        
        # Scale holdout data using same scaler
        X_holdout_scaled = pd.DataFrame(
            self.data_processor.scaler.transform(X_holdout),
            index=X_holdout.index,
            columns=X_holdout.columns
        )
        
        # Predict on holdout
        print(f"Evaluating on holdout data ({len(X_holdout)} observations)...")
        predictions = trainer.predict_with_uncertainty(trained_model, X_holdout_scaled)
        
        if predictions is None:
            print("Holdout prediction failed")
            return None
        
        # Calculate holdout metrics with full distributions for accurate scoring
        holdout_metrics = calculate_salinity_metrics(
            y_holdout.values,
            predictions['mean'],
            predictions['std'],
            y_pred_dist=predictions['distributions'],  # Pass full distributions
            salinity_thresholds=SALINITY_THRESHOLDS
        )
        
        # Calculate probabilistic metrics for risk thresholds
        threshold_probabilities = {}
        if 'distributions' in predictions:
            threshold_probabilities = calculate_threshold_probabilities(
                predictions['distributions'], 
                SALINITY_THRESHOLDS
            )
        
        # Prepare holdout results
        holdout_results = {
            'experiment_name': self.experiment_name,
            'evaluation_timestamp': datetime.now().isoformat(),
            'holdout_period': list(HOLDOUT_EVENTS.keys())[0] if HOLDOUT_EVENTS else 'unknown',
            'best_model_params': best_params,
            'holdout_metrics': holdout_metrics,
            'threshold_probabilities': threshold_probabilities,
            'data_summary': {
                'holdout_size': len(y_holdout),
                'max_salinity_observed': float(y_holdout.max()),
                'max_salinity_predicted': float(predictions['mean'].max()),
                'mean_salinity_observed': float(y_holdout.mean()),
                'mean_salinity_predicted': float(predictions['mean'].mean())
            }
        }
        
        # Save holdout results
        holdout_file = os.path.join(self.experiment_paths['results'], 'holdout_evaluation.json')
        with open(holdout_file, 'w') as f:
            json.dump(holdout_results, f, indent=2, default=str)
        
        # Save detailed predictions for R plotting
        predictions_df = pd.DataFrame({
            'DateTime': X_holdout.index,
            'Observed': y_holdout.values,
            'Predicted_Mean': predictions['mean'],
            'Predicted_Std': predictions['std'],
            'Lower_50': predictions['lower_50'],
            'Upper_50': predictions['upper_50'],
            'Lower_90': predictions['lower_90'],
            'Upper_90': predictions['upper_90'],  
            'Lower_95': predictions['lower_95'],
            'Upper_95': predictions['upper_95']
        })
        
        predictions_file = os.path.join(self.experiment_paths['plot_data'], 'holdout_predictions.csv')
        predictions_df.to_csv(predictions_file, index=False)
        
        # Print holdout summary
        print(f"\nHoldout Evaluation Results:")
        print(f"  R² = {holdout_metrics['r2']:.4f}")
        print(f"  RMSE = {holdout_metrics['rmse']:.4f}")
        print(f"  Log-likelihood = {holdout_metrics.get('log_likelihood', 'N/A')}")
        print(f"  CRPS = {holdout_metrics.get('crps_score', 'N/A')}")
        print(f"  High salinity R² = {holdout_metrics.get('high_sal_r2', 'N/A')}")
        print(f"  High salinity precision = {holdout_metrics['high_salinity_precision']:.4f}")
        print(f"  High salinity recall = {holdout_metrics['high_salinity_recall']:.4f}")
        print(f"  Max observed salinity = {holdout_results['data_summary']['max_salinity_observed']:.3f}")
        print(f"  Max predicted salinity = {holdout_results['data_summary']['max_salinity_predicted']:.3f}")
        
        # Save the final trained model
        model_file = os.path.join(self.experiment_paths['models'], 'final_model.pkl')
        joblib.dump(trained_model, model_file)
        print(f"Final model saved to: {model_file}")
        
        return holdout_results
    
    def export_plot_data(self, optimization_results, baseline_results=None, holdout_results=None):
        """Export data for R plotting according to R_PLOT_CONFIG"""
        
        if not R_PLOT_CONFIG.get('save_plot_data', True):
            return
            
        print(f"\nExporting plot data for R visualization...")
        
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        
        # Export types from config
        export_types = R_PLOT_CONFIG.get('export_types', {})
        
        # 1. Cross-validation results
        if export_types.get('cross_validation_results', True):
            cv_plot_data = []
            for result in optimization_results:
                for fold_result in result['fold_results']:
                    cv_plot_data.append({
                        'experiment': self.experiment_name,
                        'combination_id': result['combination_id'],
                        'distribution': result['model_config']['distribution'],
                        'scoring': result['model_config']['scoring'],
                        'fold': fold_result['fold'],
                        'r2': fold_result['metrics']['r2'],
                        'rmse': fold_result['metrics']['rmse'],
                        'mae': fold_result['metrics']['mae'],
                        'high_sal_r2': fold_result['metrics'].get('high_sal_r2', np.nan),
                        'high_salinity_precision': fold_result['metrics']['high_salinity_precision'],
                        'high_salinity_recall': fold_result['metrics']['high_salinity_recall']
                    })
            
            cv_df = pd.DataFrame(cv_plot_data)
            cv_file = os.path.join(self.experiment_paths['plot_data'], f'cv_results_{timestamp}.csv')
            cv_df.to_csv(cv_file, index=False)
        
        # 2. Hyperparameter optimization summary
        if export_types.get('hyperparameter_optimization', True):
            hyperparam_data = []
            for result in optimization_results:
                hyperparam_data.append({
                    'experiment': self.experiment_name,
                    'combination_id': result['combination_id'],
                    'distribution': result['model_config']['distribution'],
                    'scoring': result['model_config']['scoring'],
                    'n_estimators': result['model_config']['hyperparams'].get('n_estimators', ''),
                    'learning_rate': result['model_config']['hyperparams'].get('learning_rate', ''),
                    'minibatch_frac': result['model_config']['hyperparams'].get('minibatch_frac', ''),
                    'col_sample': result['model_config']['hyperparams'].get('col_sample', ''),
                    'mean_r2': result['mean_metrics'].get('r2', np.nan),
                    'mean_rmse': result['mean_metrics'].get('rmse', np.nan),
                    'mean_high_sal_r2': result['mean_metrics'].get('high_sal_r2', np.nan),
                    'std_r2': result['std_metrics'].get('r2', np.nan)
                })
            
            hyperparam_df = pd.DataFrame(hyperparam_data)
            hyperparam_file = os.path.join(self.experiment_paths['plot_data'], f'hyperparameter_optimization_{timestamp}.csv')
            hyperparam_df.to_csv(hyperparam_file, index=False)
        
        # 3. Model comparison (NGBoost vs baselines)
        if export_types.get('distribution_comparisons', True) and baseline_results:
            comparison_data = []
            
            # Add NGBoost best result
            best_params, best_score = self.optimizer.get_best_params()
            comparison_data.append({
                'model_type': 'NGBoost',
                'distribution': best_params['distribution'] if best_params else 'unknown',
                'mean_r2': best_score if best_score else np.nan,
                'mean_rmse': np.nan,  # Would need to extract from results
                'mean_high_sal_r2': np.nan
            })
            
            # Add baseline results
            for model_type, results in baseline_results.items():
                comparison_data.append({
                    'model_type': model_type,
                    'distribution': 'N/A',
                    'mean_r2': results['mean_metrics'].get('r2', np.nan),
                    'mean_rmse': results['mean_metrics'].get('rmse', np.nan),
                    'mean_high_sal_r2': results['mean_metrics'].get('high_sal_r2', np.nan)
                })
            
            comparison_df = pd.DataFrame(comparison_data)
            comparison_file = os.path.join(self.experiment_paths['plot_data'], f'model_comparison_{timestamp}.csv')
            comparison_df.to_csv(comparison_file, index=False)
        
        # 4. Experiment metadata
        if R_PLOT_CONFIG.get('include_metadata', True):
            metadata = {
                'experiment_name': self.experiment_name,
                'experiment_description': self.experiment_config['description'],
                'timestamp': timestamp,
                'data_config': DATA_CONFIG,
                'cv_config': CV_CONFIG,
                'salinity_thresholds': SALINITY_THRESHOLDS,
                'selected_predictors': SELECTED_PREDICTORS,
                'target_variable': TARGET_VARIABLE
            }
            
            metadata_file = os.path.join(self.experiment_paths['plot_data'], f'experiment_metadata_{timestamp}.json')
            with open(metadata_file, 'w') as f:
                json.dump(metadata, f, indent=2, default=str)
        
        print(f"✓ Plot data exported to: {self.experiment_paths['plot_data']}")
    
    def save_experiment_summary(self, optimization_results, baseline_results=None, holdout_results=None):
        """Save comprehensive experiment summary"""
        
        best_params, best_score = self.optimizer.get_best_params()
        
        summary = {
            'experiment_info': {
                'name': self.experiment_name,
                'description': self.experiment_config['description'],
                'timestamp': datetime.now().isoformat(),
                'experiment_directory': self.experiment_paths['base']
            },
            'configuration': {
                'predictors': SELECTED_PREDICTORS,
                'target': TARGET_VARIABLE,
                'hyperparameter_grid': self.experiment_config['hyperparameter_grid'],
                'distributions_tested': self.experiment_config['distributions'],
                'cv_config': CV_CONFIG,
                'data_config': DATA_CONFIG,
                'salinity_thresholds': SALINITY_THRESHOLDS
            },
            'optimization_results': {
                'best_cv_score': best_score,
                'best_parameters': best_params,
                'total_combinations_tested': len(optimization_results),
                'successful_combinations': len([r for r in optimization_results if r['mean_metrics']])
            },
            'baseline_comparison': baseline_results is not None,
            'holdout_evaluation': holdout_results is not None
        }
        
        # Add baseline summary if available
        if baseline_results:
            summary['baseline_results'] = {
                model_type: {
                    'r2': results['mean_metrics'].get('r2', np.nan),
                    'rmse': results['mean_metrics'].get('rmse', np.nan)
                }
                for model_type, results in baseline_results.items()
            }
        
        # Add holdout summary if available
        if holdout_results:
            summary['holdout_results'] = {
                'r2': holdout_results['holdout_metrics']['r2'],
                'rmse': holdout_results['holdout_metrics']['rmse'],
                'high_salinity_precision': holdout_results['holdout_metrics']['high_salinity_precision'],
                'high_salinity_recall': holdout_results['holdout_metrics']['high_salinity_recall'],
                'max_observed_salinity': holdout_results['data_summary']['max_salinity_observed']
            }
        
        summary_file = os.path.join(self.experiment_paths['results'], 'experiment_summary.json')
        with open(summary_file, 'w') as f:
            json.dump(summary, f, indent=2, default=str)
        
        return summary
    
    def run_experiment(self):
        """Execute the full experiment pipeline"""
        
        print(f"\n{'='*80}")
        print(f"STARTING EXPERIMENT: {self.experiment_name}")
        print(f"{'='*80}")
        
        try:
            # Setup
            self.setup_experiment()
            
            # Load and prepare data
            X_train, y_train, X_holdout, y_holdout = self.load_and_prepare_data()
            
            # Phase 1: Hyperparameter optimization
            optimization_results = self.run_hyperparameter_optimization(X_train, y_train)
            
            # Phase 2: Baseline comparison (if enabled)
            baseline_results = None
            if BASELINE_MODELS:
                baseline_results = self.run_baseline_comparison(X_train, y_train)
            
            # Phase 3: Holdout evaluation (if 2016 data available)
            holdout_results = None
            if X_holdout is not None:
                holdout_results = self.evaluate_on_holdout(X_train, y_train, X_holdout, y_holdout)
            
            # Phase 4: Export plot data for R
            self.export_plot_data(optimization_results, baseline_results, holdout_results)
            
            # Phase 5: Save comprehensive summary
            summary = self.save_experiment_summary(optimization_results, baseline_results, holdout_results)
            
            print(f"\n{'='*80}")
            print(f"EXPERIMENT COMPLETED: {self.experiment_name}")
            print(f"{'='*80}")
            print(f"Results directory: {self.experiment_paths['base']}")
            
            if self.optimizer.best_score:
                print(f"Best CV R²: {self.optimizer.best_score:.4f}")
            
            return {
                'experiment_name': self.experiment_name,
                'experiment_paths': self.experiment_paths,
                'optimization_results': optimization_results,
                'baseline_results': baseline_results,
                'holdout_results': holdout_results,
                'summary': summary,
                'status': 'completed'
            }
            
        except Exception as e:
            print(f"\n{'='*80}")
            print(f"EXPERIMENT FAILED: {self.experiment_name}")
            print(f"Error: {str(e)}")
            print(f"{'='*80}")
            
            # Save error information
            error_info = {
                'experiment_name': self.experiment_name,
                'error_timestamp': datetime.now().isoformat(),
                'error_message': str(e),
                'error_type': type(e).__name__
            }
            
            error_file = os.path.join(self.experiment_paths['results'], 'experiment_error.json')
            with open(error_file, 'w') as f:
                json.dump(error_info, f, indent=2, default=str)
            
            return {
                'experiment_name': self.experiment_name,
                'status': 'failed',
                'error': str(e)
            }

def run_single_experiment(experiment_name):
    """Run a single experiment by name"""
    
    if experiment_name not in EXPERIMENTS:
        print(f"Error: Unknown experiment '{experiment_name}'")
        print(f"Available experiments: {list(EXPERIMENTS.keys())}")
        return None
    
    # Create and run experiment
    runner = NGBoostExperimentRunner(experiment_name)
    results = runner.run_experiment()
    
    return results

def run_experiment_workflow(workflow_experiments=None):
    """Run a sequence of experiments"""
    
    if workflow_experiments is None:
        # Use recommended workflow from experiments
        workflow_experiments = ['quick_test', 'distribution_comparison', 'optimize_learning', 'optimize_regularization', 'final_model']
    
    print(f"\n{'='*80}")
    print(f"RUNNING EXPERIMENT WORKFLOW")
    print(f"{'='*80}")
    print(f"Experiments to run: {workflow_experiments}")
    
    all_results = {}
    
    for experiment_name in workflow_experiments:
        print(f"\n\nStarting workflow step: {experiment_name}")
        
        try:
            results = run_single_experiment(experiment_name)
            all_results[experiment_name] = results
            
            if results['status'] == 'completed':
                print(f"✓ {experiment_name} completed successfully")
            else:
                print(f"✗ {experiment_name} failed: {results.get('error', 'Unknown error')}")
                
                # Ask user if they want to continue
                response = input(f"Continue with remaining experiments? (y/n): ")
                if response.lower() != 'y':
                    print("Workflow interrupted by user")
                    break
                    
        except KeyboardInterrupt:
            print(f"\nWorkflow interrupted by user at {experiment_name}")
            break
        except Exception as e:
            error_msg = f"Unexpected error in {experiment_name}: {str(e)}"
            print(f"✗ {error_msg}")
            all_results[experiment_name] = {
                'experiment_name': experiment_name,
                'status': 'failed',
                'error': error_msg
            }
    
    # Print workflow summary
    print(f"\n{'='*80}")
    print("WORKFLOW SUMMARY")
    print(f"{'='*80}")
    
    completed = 0
    failed = 0
    
    for exp_name, result in all_results.items():
        status = result['status']
        if status == 'completed':
            completed += 1
            print(f"✓ {exp_name}: {status}")
        else:
            failed += 1
            print(f"✗ {exp_name}: {status}")
    
    print(f"\nTotal experiments: {len(all_results)}")
    print(f"Completed: {completed}")
    print(f"Failed: {failed}")
    
    return all_results

def print_experiment_info(experiment_name):
    """Print detailed information about an experiment"""
    
    if experiment_name not in EXPERIMENTS:
        print(f"Unknown experiment: {experiment_name}")
        print(f"Available experiments: {list(EXPERIMENTS.keys())}")
        return
    
    exp = EXPERIMENTS[experiment_name]
    
    print(f"\n{'='*60}")
    print(f"EXPERIMENT: {experiment_name}")
    print(f"{'='*60}")
    print(f"Description: {exp['description']}")
    print(f"Distributions to test: {exp['distributions']}")
    print(f"Number of runs: {exp['n_runs']}")
    
    if isinstance(exp['hyperparameter_grid'], dict):
        print(f"\nHyperparameter Grid:")
        for param, values in exp['hyperparameter_grid'].items():
            print(f"  {param}: {values}")
        
        n_combinations = 1
        for param, values in exp['hyperparameter_grid'].items():
            n_combinations *= len(values)
        print(f"\nTotal hyperparameter combinations: {n_combinations}")
        
        estimated_models = n_combinations * len(exp['distributions']) * CV_CONFIG['n_splits']
        print(f"Estimated models to train: {estimated_models}")
        
        # Rough time estimate (very approximate)
        est_minutes = estimated_models * 0.5  # Assume ~30 seconds per model
        print(f"Estimated runtime: {est_minutes:.0f} minutes ({est_minutes/60:.1f} hours)")
    
    print(f"\nSpecial features:")
    if exp.get('include_2016_holdout', False):
        print("  ✓ Includes 2016 extreme event holdout evaluation")
    print("  ✓ Uses NGBoost default LogScore for training, calculates CRPS post-hoc")

def list_available_experiments():
    """List all available experiments with descriptions"""
    
    print(f"\n{'='*60}")
    print("AVAILABLE EXPERIMENTS")
    print(f"{'='*60}")
    
    for name, config in EXPERIMENTS.items():
        print(f"{name:25} : {config['description']}")
    
    print(f"\nTo run an experiment: run_single_experiment('experiment_name')")
    print(f"For details: print_experiment_info('experiment_name')")

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
#         workflow = args.workflow if args.workflow else None
#         run_experiment_workflow(workflow)
# 
# if __name__ == "__main__":
#     main()
