# =============================================================================
# Script Name:    Main_NG.py
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-07-28
# Last Updated:   2025-07-28
# Description:    Runs the full modeling pipeline from data import to fitting
#                 and then validation and final results.
# =============================================================================


"""
NGBoost Salinity Prediction - Main Interface

This is the main entry point for running NGBoost experiments for salinity prediction 
at the mouth of the Susquehanna River. The system predicts saltwater intrusion events
to support optimized dam release policies.

Quick Start:
    from Main_NG import *
    
    # List available experiments
    list_experiments()
    
    # Run a single experiment
    run_experiment('quick_test')
    
    # Run the full recommended workflow
    run_full_workflow()
    
    # Check system status
    check_system_status()

Command Line Usage:
    python Main_NG.py                    # Interactive mode
    python Main_NG.py quick_test         # Run specific experiment
    python Main_NG.py --workflow         # Run full workflow
    python Main_NG.py --status           # Check system status
"""

import sys
import os
import logging
import warnings
from datetime import datetime
from pathlib import Path

# Suppress non-critical warnings for cleaner output
warnings.filterwarnings('ignore', category=FutureWarning)
warnings.filterwarnings('ignore', category=UserWarning, module='sklearn')

# Add current directory to path for imports
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

# =============================================================================
# IMPORT PROJECT MODULES
# =============================================================================
try:
    from Config_NG import *
    from DataUtils_NG import SalinityDataProcessor, SalinityTimeSeriesCV
    from ModelUtils_NG import NGBoostModelTrainer, NGBoostHyperparameterOptimizer, BaselineModelComparator
    from Experiments_NG import NGBoostExperimentRunner, print_experiment_info, list_available_experiments, run_single_experiment
    
    print("✓ All modules imported successfully")
    
except ImportError as e:
    print(f"✗ Import error: {e}")
    print("Please ensure all required modules are in the same directory:")
    print("  - Config_NG.py")
    print("  - DataUtils_NG.py") 
    print("  - ModelUtils_NG.py")
    print("  - Experiments_NG.py")
    sys.exit(1)

# =============================================================================
# LOGGING SETUP
# =============================================================================
def setup_logging(experiment_name=None):
    """Set up logging for the main interface"""
    
    # Create logs directory if it doesn't exist
    os.makedirs(LOGS_DIR, exist_ok=True)
    
    # Create log filename
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    if experiment_name:
        log_file = os.path.join(LOGS_DIR, f"main_{experiment_name}_{timestamp}.log")
    else:
        log_file = os.path.join(LOGS_DIR, f"main_interface_{timestamp}.log")
    
    # Configure logging
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        handlers=[
            logging.FileHandler(log_file),
            logging.StreamHandler(sys.stdout)
        ]
    )
    
    return logging.getLogger(__name__)

# =============================================================================
# SYSTEM STATUS AND VALIDATION
# =============================================================================
def check_system_status():
    """Check if the system is properly configured and ready"""
    
    print("\n" + "="*60)
    print("NGBoost Salinity Prediction - System Status Check")
    print("="*60)
    
    status_checks = []
    
    # 1. Check data file exists
    data_exists = os.path.exists(DATA_PATH)
    status_checks.append(("Data file exists", data_exists, DATA_PATH))
    
    # 2. Check output directories
    dirs_exist = all(os.path.exists(d) for d in [BASE_EXPERIMENTS_DIR, LOGS_DIR])
    status_checks.append(("Output directories", dirs_exist, f"{BASE_EXPERIMENTS_DIR}, {LOGS_DIR}"))
    
    # 3. Check predictor configuration
    predictors_valid = len(SELECTED_PREDICTORS) > 0
    status_checks.append(("Selected predictors", predictors_valid, f"{len(SELECTED_PREDICTORS)} predictors"))
    
    # 4. Check if we can load data
    try:
        processor = SalinityDataProcessor(
                        data_path=DATA_PATH,
                        selected_predictors=SELECTED_PREDICTORS,
                        target_variable=TARGET_VARIABLE,
                        data_config=DATA_CONFIG,
                        holdout_events=HOLDOUT_EVENTS  # optional
                    )
        data_loadable = True
        data_shape = processor.load_data().shape
    except Exception as e:
        data_loadable = False
        data_shape = f"Error: {str(e)}"
    
    status_checks.append(("Data loading", data_loadable, str(data_shape)))
    
    # 5. Check NGBoost dependencies
    try:
        from ngboost import NGBoost
        from ngboost.distns import LogNormal, Gamma
        #from ngboost.scores import LogScore, CRPS
        ngboost_available = True
    except ImportError as e:
        ngboost_available = False
        
    status_checks.append(("NGBoost dependencies", ngboost_available, "NGBoost, distributions, scores"))
    
    # Print status
    all_good = True
    for check_name, status, details in status_checks:
        status_symbol = "✓" if status else "✗"
        print(f"{status_symbol} {check_name:<25}: {details}")
        if not status:
            all_good = False
    
    print("\n" + "-"*60)
    if all_good:
        print("✓ System is ready for experiments")
        
        # Show quick stats
        try:
            processor = SalinityDataProcessor(
                        data_path=DATA_PATH,
                        selected_predictors=SELECTED_PREDICTORS,
                        target_variable=TARGET_VARIABLE,
                        data_config=DATA_CONFIG,
                        holdout_events=HOLDOUT_EVENTS  # optional
                    )
            data = processor.load_data()
            salinity_stats = data[TARGET_VARIABLE].describe()
            
            print(f"\nData Summary:")
            print(f"  Total observations: {len(data):,}")
            print(f"  Date range: {data.index.min()} to {data.index.max()}")
            print(f"  Salinity range: {salinity_stats['min']:.3f} - {salinity_stats['max']:.3f}")
            print(f"  Mean salinity: {salinity_stats['mean']:.3f}")
            print(f"  High salinity events (>0.5): {sum(data[TARGET_VARIABLE] > 0.5)}")
            
        except Exception as e:
            print(f"Could not load data summary: {e}")
            
    else:
        print("✗ System has issues that need to be resolved")
        print("\nTroubleshooting:")
        print("1. Ensure data file exists at specified path")
        print("2. Check that all Python dependencies are installed")
        print("3. Verify directory permissions for output folders")
    
    print("="*60)
    return all_good

# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

def list_experiments():
    """List all available experiments with details"""
    print("\n" + "="*60)
    print("NGBoost Salinity Prediction - Available Experiments")
    print("="*60)
    
    # Show system info
    print(f"Data: {os.path.basename(DATA_PATH)}")
    print(f"Predictors: {len(SELECTED_PREDICTORS)}")
    print(f"Target: {TARGET_VARIABLE}")
    print(f"Max cores: {MAX_CORES}")
    
    print("\n" + "-"*40)
    list_available_experiments()
    
    print("\nRecommended workflow:")
    #print_recommended_workflow()
    
    print(f"\nExperiment outputs will be saved to: {BASE_EXPERIMENTS_DIR}")

def run_experiment(experiment_name, skip_confirmation=False):
    """
    Run a single experiment
    
    Parameters:
    -----------
    experiment_name : str
        Name of experiment to run
    skip_confirmation : bool
        Skip user confirmation (useful for batch runs)
    """
    
    if experiment_name not in EXPERIMENTS:
        print(f"✗ Unknown experiment: {experiment_name}")
        print("Available experiments:")
        for name in EXPERIMENTS.keys():
            print(f"  - {name}")
        return None, None
    
    # Set up logging for this experiment
    logger = setup_logging(experiment_name)
    logger.info(f"Starting experiment: {experiment_name}")
    
    print(f"\n" + "="*60)
    print(f"Running NGBoost Experiment: {experiment_name}")
    print("="*60)
    
    # Show experiment details
    print_experiment_info(experiment_name)
    
    # Get user confirmation unless skipped
    if not skip_confirmation:
        print(f"\nExperiment will be saved to:")
        experiment_paths = get_experiment_paths(experiment_name)
        print(f"  {experiment_paths['base']}")
        
        response = input(f"\nProceed with experiment '{experiment_name}'? (y/n): ")
        if response.lower() not in ['y', 'yes']:
            print("Experiment cancelled.")
            return None, None
    
    # Run the experiment
    try:
        print(f"\n{'='*40}")
        print("Starting experiment execution...")
        print(f"{'='*40}")
        
        # Create experiment runner
        runner = NGBoostExperimentRunner(experiment_name)
        
        # Run the experiment
        results = runner.run_experiment()
        
        print(f"\n{'='*40}")
        print(f"✓ Experiment '{experiment_name}' completed successfully!")
        print(f"{'='*40}")
        
        # Show key results if available
        if results and 'best_model_performance' in results:
            perf = results['best_model_performance']
            print(f"\nKey Results:")
            print(f"  Best CV R²: {perf.get('cv_r2_mean', 'N/A'):.4f}")
            if 'cv_r2_std' in perf:
                print(f"  CV R² std: ±{perf['cv_r2_std']:.4f}")
            print(f"  Best distribution: {results.get('best_distribution', 'N/A')}")
            print(f"  Best parameters: {results.get('best_params', 'N/A')}")
        
        # Show output locations
        experiment_paths = get_experiment_paths(experiment_name)
        print(f"\nOutputs saved to: {experiment_paths['base']}")
        print(f"  Models: {experiment_paths['models']}")
        print(f"  Results: {experiment_paths['results']}")
        print(f"  Plot data: {experiment_paths['plot_data']}")
        
        logger.info(f"Experiment {experiment_name} completed successfully")
        
        return runner, results
        
    except Exception as e:
        print(f"\n✗ Experiment failed with error:")
        print(f"  {str(e)}")
        logger.error(f"Experiment {experiment_name} failed: {str(e)}", exc_info=True)
        
        print(f"\nCheck log file for details: {LOGS_DIR}")
        return None, None

def run_full_workflow(skip_confirmation=False):
    """
    Run the complete recommended workflow
    
    This runs all experiments in the recommended sequence:
    1. quick_test - Verify system works
    2. distribution_comparison - Find best distribution  
    3. optimize_learning - Find best learning parameters
    4. optimize_regularization - Add regularization
    5. final_model - Complete model with holdout validation
    """
    
    logger = setup_logging("full_workflow")
    
    print("\n" + "="*60)
    print("NGBoost Salinity Prediction - Full Workflow")
    print("="*60)
    
    # Define workflow sequence
    workflow_experiments = [
        'quick_test',
        'distribution_comparison', 
        'optimize_learning',
        'optimize_regularization',
        'final_model'
    ]
    
    print("This will run all experiments in sequence:")
    for i, exp_name in enumerate(workflow_experiments, 1):
        exp_info = EXPERIMENTS[exp_name]
        print(f"  {i}. {exp_name}: {exp_info['description']}")
    
    print(f"\nEstimated total time: 2-6 hours depending on data size")
    print(f"All outputs will be saved to: {BASE_EXPERIMENTS_DIR}")
    
    if not skip_confirmation:
        response = input(f"\nProceed with full workflow? (y/n): ")
        if response.lower() not in ['y', 'yes']:
            print("Workflow cancelled.")
            return None
    
    # Run workflow
    workflow_results = {}
    failed_experiments = []
    
    print(f"\n{'='*60}")
    print("Starting Full Workflow Execution")
    print(f"{'='*60}")
    
    for i, exp_name in enumerate(workflow_experiments, 1):
        print(f"\n{'-'*40}")
        print(f"Workflow Step {i}/{len(workflow_experiments)}: {exp_name}")
        print(f"{'-'*40}")
        
        try:
            runner, results = run_experiment(exp_name, skip_confirmation=True)
            
            if results is not None:
                workflow_results[exp_name] = results
                print(f"✓ Step {i} completed successfully")
            else:
                failed_experiments.append(exp_name)
                print(f"✗ Step {i} failed")
                
                # Ask if should continue
                if i < len(workflow_experiments):  # Not the last experiment
                    continue_response = input(f"Continue with remaining experiments? (y/n): ")
                    if continue_response.lower() not in ['y', 'yes']:
                        print("Workflow stopped by user.")
                        break
                        
        except KeyboardInterrupt:
            print(f"\n\nWorkflow interrupted by user at step {i}")
            break
        except Exception as e:
            print(f"✗ Step {i} failed with error: {e}")
            failed_experiments.append(exp_name)
            logger.error(f"Step {i} ({exp_name}) failed: {str(e)}", exc_info=True)
    
    # Workflow summary
    print(f"\n{'='*60}")
    print("Full Workflow Summary")
    print(f"{'='*60}")
    
    completed = len(workflow_results)
    total = len(workflow_experiments)
    failed = len(failed_experiments)
    
    print(f"Completed: {completed}/{total} experiments")
    
    if workflow_results:
        print(f"\n✓ Successful experiments:")
        for exp_name in workflow_results.keys():
            print(f"  - {exp_name}")
    
    if failed_experiments:
        print(f"\n✗ Failed experiments:")
        for exp_name in failed_experiments:
            print(f"  - {exp_name}")
    
    print(f"\nAll results saved to: {BASE_EXPERIMENTS_DIR}")
    print(f"Logs saved to: {LOGS_DIR}")
    
    logger.info(f"Full workflow completed: {completed}/{total} successful")
    
    return workflow_results

def quick_test():
    """Run just the quick test experiment to verify setup"""
    print("\n" + "="*50)
    print("Running Quick Test")
    print("="*50)
    print("This will verify that your system is properly configured")
    print("and run a fast test with a small parameter grid.")
    
    return run_experiment('quick_test')

def run_custom_experiment(name, config):
    """
    Run a custom experiment with user-defined configuration
    
    Parameters:
    -----------
    name : str
        Custom experiment name
    config : dict
        Experiment configuration matching EXPERIMENTS format
    """
    
    # Temporarily add to experiments
    original_experiments = EXPERIMENTS.copy()
    EXPERIMENTS[name] = config
    
    try:
        runner, results = run_experiment(name, skip_confirmation=True)
        return runner, results
    finally:
        # Restore original experiments
        EXPERIMENTS.clear()
        EXPERIMENTS.update(original_experiments)

def interactive_mode():
    """Interactive mode for selecting and running experiments"""
    
    while True:
        print("\n" + "="*60)
        print("NGBoost Salinity Prediction - Interactive Mode")
        print("="*60)
        print("1. Check system status")
        print("2. List available experiments")
        print("3. Run single experiment")
        print("4. Run full workflow")
        print("5. Quick test")
        print("6. View experiment results")
        print("7. Exit")
        
        choice = input("\nSelect option (1-7): ").strip()
        
        if choice == '1':
            check_system_status()
            
        elif choice == '2':
            list_experiments()
            
        elif choice == '3':
            list_experiments()
            exp_name = input("\nEnter experiment name: ").strip()
            if exp_name in EXPERIMENTS:
                run_experiment(exp_name)
            else:
                print(f"Unknown experiment: {exp_name}")
                print("Use option 2 to see available experiments.")
                
        elif choice == '4':
            run_full_workflow()
            
        elif choice == '5':
            quick_test()
            
        elif choice == '6':
            view_experiment_results()
            
        elif choice == '7':
            print("Goodbye!")
            break
            
        else:
            print("Invalid choice. Please select 1-7.")

def view_experiment_results():
    """View results from completed experiments"""
    
    print("\n" + "="*50)
    print("Experiment Results")
    print("="*50)
    
    if not os.path.exists(BASE_EXPERIMENTS_DIR):
        print("No experiment results found.")
        return
    
    # Find completed experiments
    experiment_dirs = [d for d in os.listdir(BASE_EXPERIMENTS_DIR) 
                      if os.path.isdir(os.path.join(BASE_EXPERIMENTS_DIR, d))]
    
    if not experiment_dirs:
        print("No experiment results found.")
        return
    
    print("Completed experiments:")
    for i, exp_dir in enumerate(experiment_dirs, 1):
        exp_path = os.path.join(BASE_EXPERIMENTS_DIR, exp_dir)
        results_path = os.path.join(exp_path, 'results')
        
        # Check if has results
        has_results = os.path.exists(results_path) and os.listdir(results_path)
        status = "✓" if has_results else "○"
        
        print(f"  {i}. {status} {exp_dir}")
    
    # Let user select experiment to view
    while True:
        try:
            choice = input(f"\nSelect experiment to view (1-{len(experiment_dirs)}) or 'q' to quit: ").strip()
            if choice.lower() == 'q':
                break
                
            idx = int(choice) - 1
            if 0 <= idx < len(experiment_dirs):
                exp_name = experiment_dirs[idx]
                show_experiment_summary(exp_name)
                break
            else:
                print(f"Please enter a number between 1 and {len(experiment_dirs)}")
                
        except ValueError:
            print("Please enter a valid number or 'q'")

def show_experiment_summary(experiment_name):
    """Show summary of a completed experiment"""
    
    exp_path = os.path.join(BASE_EXPERIMENTS_DIR, experiment_name)
    results_path = os.path.join(exp_path, 'results')
    
    print(f"\n" + "="*50)
    print(f"Experiment: {experiment_name}")
    print("="*50)
    
    if not os.path.exists(results_path):
        print("No results found for this experiment.")
        return
    
    # List result files
    result_files = os.listdir(results_path)
    
    print(f"Results directory: {results_path}")
    print(f"Files found: {len(result_files)}")
    
    for file in sorted(result_files):
        file_path = os.path.join(results_path, file)
        file_size = os.path.getsize(file_path)
        print(f"  - {file} ({file_size:,} bytes)")
    
    # Try to load and show summary statistics
    try:
        import json
        summary_file = os.path.join(results_path, f"{experiment_name}_summary.json")
        if os.path.exists(summary_file):
            with open(summary_file, 'r') as f:
                summary = json.load(f)
            
            print(f"\nExperiment Summary:")
            if 'best_model_performance' in summary:
                perf = summary['best_model_performance']
                for metric, value in perf.items():
                    if isinstance(value, float):
                        print(f"  {metric}: {value:.4f}")
                    else:
                        print(f"  {metric}: {value}")
                        
    except Exception as e:
        print(f"Could not load experiment summary: {e}")

# =============================================================================
# COMMAND LINE INTERFACE
# =============================================================================

def print_usage():
    """Print command line usage information"""
    print(__doc__)
    print("\nAdditional Options:")
    print("  --status     Check system status")
    print("  --list       List available experiments")
    print("  --quick      Run quick test")
    print("  --help, -h   Show this help message")

def main():
    """Main function for command line usage"""
    
    # Print header
    print("NGBoost Salinity Prediction System")
    print("Susquehanna River Saltwater Intrusion Modeling")
    print("=" * 50)
    
    # Basic system info
    print(f"Data file: {os.path.basename(DATA_PATH) if os.path.exists(DATA_PATH) else 'NOT FOUND'}")
    print(f"Predictors: {len(SELECTED_PREDICTORS)}")
    print(f"Target variable: {TARGET_VARIABLE}")
    print(f"Available cores: {MAX_CORES}")
    
    # Handle command line arguments
    if len(sys.argv) == 1:
        # No arguments - run interactive mode
        print("\nStarting interactive mode...")
        interactive_mode()
        
    elif len(sys.argv) == 2:
        arg = sys.argv[1]
        
        if arg in ['--help', '-h']:
            print_usage()
            
        elif arg == '--list':
            list_experiments()
            
        elif arg == '--status':
            check_system_status()
            
        elif arg == '--workflow':
            run_full_workflow()
            
        elif arg == '--quick':
            quick_test()
            
        elif arg in EXPERIMENTS:
            run_experiment(arg)
            
        else:
            print(f"\n✗ Unknown argument: {arg}")
            print("Use --help for usage information")
            sys.exit(1)
            
    else:
        print("\n✗ Too many arguments.")
        print("Use --help for usage information")
        sys.exit(1)

# =============================================================================
# MODULE EXPORTS FOR INTERACTIVE USE
# =============================================================================

# Make key functions available at module level for easy import
__all__ = [
    'run_experiment',
    'run_full_workflow', 
    'list_experiments',
    'quick_test',
    'interactive_mode',
    'check_system_status',
    'run_custom_experiment',
    'view_experiment_results'
]

# =============================================================================
# ENTRY POINT
# =============================================================================

if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("\n\nProgram interrupted by user. Goodbye!")
        sys.exit(0)
    except Exception as e:
        print(f"\n✗ Unexpected error: {e}")
        print("Check log files for details.")
        sys.exit(1)
