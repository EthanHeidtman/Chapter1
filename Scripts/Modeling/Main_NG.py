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

This is the main entry point for running NGBoost experiments for salinity prediction.

Quick Start:
    from Main_NG import *
    
    # List available experiments
    list_experiments()
    
    # Run a single experiment
    run_experiment('quick_test')
    
    # Run the full recommended workflow
    run_full_workflow()

Command Line Usage:
    python Main_NG.py                    # Interactive mode
    python Main_NG.py quick_test         # Run specific experiment
    python Main_NG.py --workflow         # Run full workflow
"""

import sys
import os

# Add current directory to path for imports
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

# Import all the modules
from Config_NG import *
from Experiments_NG import *

# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

def list_experiments():
    """List all available experiments"""
    print("\n" + "="*50)
    print("NGBoost Salinity Prediction Experiments")
    print("="*50)
    list_available_experiments()
    print("\nRecommended workflow:")
    print_recommended_workflow()

def run_experiment(experiment_name):
    """Run a single experiment"""
    print(f"\nRunning experiment: {experiment_name}")
    
    # Show experiment info first
    print_experiment_info(experiment_name)
    
    # Confirm with user
    response = input(f"\nProceed with experiment '{experiment_name}'? (y/n): ")
    if response.lower() != 'y':
        print("Experiment cancelled.")
        return None
    
    # Run the experiment
    runner, results = run_single_experiment(experiment_name)
    
    print(f"\nExperiment '{experiment_name}' completed!")
    if runner.optimizer and runner.optimizer.best_score:
        print(f"Best CV R² achieved: {runner.optimizer.best_score:.4f}")
    
    return runner, results

def run_full_workflow():
    """Run the complete recommended workflow"""
    print("\n" + "="*50)
    print("Running Full NGBoost Workflow")
    print("="*50)
    
    print("This will run all recommended experiments in sequence:")
    print_recommended_workflow()
    
    response = input(f"\nThis may take several hours. Proceed? (y/n): ")
    if response.lower() != 'y':
        print("Workflow cancelled.")
        return None
    
    results = run_workflow()
    
    print("\n" + "="*50)
    print("Full Workflow Completed!")
    print("="*50)
    
    return results

def quick_test():
    """Run just the quick test experiment"""
    print("Running quick test to verify setup...")
    return run_experiment('quick_test')

def interactive_mode():
    """Interactive mode for selecting experiments"""
    
    while True:
        print("\n" + "="*50)
        print("NGBoost Salinity Prediction - Interactive Mode")
        print("="*50)
        print("1. List available experiments")
        print("2. Run single experiment")
        print("3. Run full workflow")
        print("4. Quick test")
        print("5. Exit")
        
        choice = input("\nSelect option (1-5): ").strip()
        
        if choice == '1':
            list_experiments()
            
        elif choice == '2':
            list_experiments()
            exp_name = input("\nEnter experiment name: ").strip()
            if exp_name in EXPERIMENTS:
                run_experiment(exp_name)
            else:
                print(f"Unknown experiment: {exp_name}")
                
        elif choice == '3':
            run_full_workflow()
            
        elif choice == '4':
            quick_test()
            
        elif choice == '5':
            print("Goodbye!")
            break
            
        else:
            print("Invalid choice. Please select 1-5.")

# =============================================================================
# COMMAND LINE INTERFACE
# =============================================================================

def main():
    """Main function for command line usage"""
    
    # Print header
    print("NGBoost Salinity Prediction")
    print("=" * 30)
    print(f"Data: {DATA_PATH}")
    print(f"Predictors: {len(SELECTED_PREDICTORS)}")
    print(f"Target: {TARGET_VARIABLE}")
    print(f"Max cores: {MAX_CORES}")
    
    # Handle command line arguments
    if len(sys.argv) == 1:
        # No arguments - interactive mode
        interactive_mode()
        
    elif len(sys.argv) == 2:
        arg = sys.argv[1]
        
        if arg == '--help' or arg == '-h':
            print(__doc__)
            
        elif arg == '--list':
            list_experiments()
            
        elif arg == '--workflow':
            run_full_workflow()
            
        elif arg == '--quick':
            quick_test()
            
        elif arg in EXPERIMENTS:
            run_experiment(arg)
            
        else:
            print(f"Unknown argument: {arg}")
            print("Use --help for usage information")
            
    else:
        print("Too many arguments. Use --help for usage information")

if __name__ == "__main__":
    main()

# =============================================================================
# FOR JUPYTER/INTERACTIVE USE
# =============================================================================

# Make key functions available at module level for easy import
__all__ = [
    'run_experiment',
    'run_full_workflow', 
    'list_experiments',
    'quick_test',
    'interactive_mode'
]
