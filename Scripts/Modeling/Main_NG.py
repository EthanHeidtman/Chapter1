# =============================================================================
# Script Name:    Main_NG.py
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-07-28
# Last Updated:   2025-07-28
# Description:    Runs the full modeling pipeline from data import to fitting
#                 and then validation and final results.
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
import pandas as pd
import numpy as np
import json
from datetime import datetime
import warnings
warnings.filterwarnings('ignore')

# Import our modules
from Config_NG import *
from DataUtils_NG import *
from ModelUtils_NG import *

def main():
    """
    Main execution function for NGBoost modeling pipeline.
    """
    print("="*60)
    print("NGBoost Salinity Prediction Model")
    print("="*60)
    
    # Step 1: Load and examine data
    print("\n1. Loading data...")
    data = load_model_data()
    if data is None:
        print("Failed to load data. Exiting.")
        return
    
    # Step 2: Data quality check
    print("\n2. Checking data quality...")
    quality_report = check_data_quality(data, SELECTED_PREDICTORS, TARGET_VARIABLE)
    
    # Step 3: Prepare modeling data
    print("\n3. Preparing modeling data...")
    X, y, scaler = prepare_modeling_data(
        data, 
        SELECTED_PREDICTORS, 
        TARGET_VARIABLE,
        scaler_type='none',  
        handle_missing='drop'
    )
    
    if len(X) == 0:
        print("No data remaining after preprocessing. Exiting.")
        return
    
    # Step 4: Create cross-validation splits
    print("\n4. Creating time series cross-validation splits...")
    cv_splits = create_time_series_splits(X, n_splits=CV_PARAMS['n_splits'])
    
    # Step 5: Test different NGBoost configurations
    print("\n5. Testing NGBoost configurations...")
    
    # Test different distributions
    distribution_results = {}
    for dist_name, dist_class in DISTRIBUTIONS.items():
        print(f"\n  Testing {dist_name} distribution...")
        try:
            model_params = {'distribution': dist_class}
            cv_summary, cv_details = cross_validate_ngboost(
                X, y, model_params, cv_splits
            )
            distribution_results[dist_name] = cv_summary
            print(f"    CV R²: {cv_summary['r2_mean']:.4f} ± {cv_summary['r2_std']:.4f}")
        except Exception as e:
            print(f"    Error with {dist_name}: {e}")
            distribution_results[dist_name] = None
    
    # Find best distribution
    best_dist = None
    best_dist_score = -np.inf
    for dist_name, results in distribution_results.items():
        if results and results['r2_mean'] > best_dist_score:
            best_dist_score = results['r2_mean']
            best_dist = dist_name
    
    print(f"\n  Best distribution: {best_dist} (R² = {best_dist_score:.4f})")
    
    # Step 6: Hyperparameter tuning with best distribution
    print("\n6. Hyperparameter tuning...")
    
    param_grid = {
        'n_estimators': [250, 500, 750],
        'learning_rate': [0.005, 0.01, 0.02],
        'distribution': [DISTRIBUTIONS[best_dist]]
    }
    
    tuning_results = grid_search_ngboost(X, y, param_grid, cv_splits)
    best_params = tuning_results['best_params']
    
    print(f"\nBest hyperparameters: {best_params}")
    print(f"Best CV score: {tuning_results['best_score']:.4f}")
    
    # Step 7: Train final model with best parameters
    print("\n7. Training final model...")
    final_model = NGBoostModel(**best_params)
    final_model.fit(X, y)
    
    # Make predictions on full dataset for analysis
    y_pred_mean, y_pred_std = final_model.predict(X, return_std=True)
    quantile_preds = final_model.predict_quantiles(X)
    
    # Calculate final metrics
    extreme_mask = get_extreme_events_mask(y)
    final_metrics = calculate_metrics(y.values, y_pred_mean, extreme_mask)
    
    print("\nFinal model performance:")
    for metric, value in final_metrics.items():
        print(f"  {metric}: {value:.4f}")
    
    # Step 8: Model comparison
    print("\n8. Comparing with baseline models...")
    comparison_results = compare_models(X, y, cv_splits)
    print("\nModel comparison:")
    print(comparison_results.to_string(index=False))
    
    # Step 9: Feature importance
    print("\n9. Feature importance...")
    if CALCULATE_FEATURE_IMPORTANCE:
        feature_importance = final_model.get_feature_importance()
        if feature_importance is not None:
            print("\nTop 10 most important features:")
            print(feature_importance.head(10).to_string())
    
    # Step 10: Save results
    print("\n10. Saving results...")
    
    # Prepare results dictionary
    results = {
        'timestamp': datetime.now().isoformat(),
        'data_info': {
            'n_samples': len(X),
            'n_features': len(X.columns),
            'target_range': [float(y.min()), float(y.max())],
            'extreme_events': int(extreme_mask.sum())
        },
        'best_distribution': best_dist,
        'best_parameters': best_params,
        'cv_score': tuning_results['best_score'],
        'final_metrics': final_metrics,
        'distribution_comparison': distribution_results,
        'model_comparison': comparison_results.to_dict('records'),
        'feature_importance': feature_importance.to_dict() if feature_importance is not None else None
    }
    
    # Save results to JSON
    results_file = f"{RESULTS_DIR}/{EXPERIMENT_PREFIX}_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
    with open(results_file, 'w') as f:
        json.dump(results, f, indent=2, default=str)
    
    # Save predictions for R analysis
    predictions_df = pd.DataFrame({
        'observed': y.values,
        'predicted_mean': y_pred_mean,
        'predicted_std': y_pred_std,
        'extreme_event': extreme_mask
    })
    
    # Add quantile predictions
    for col in quantile_preds.columns:
        predictions_df[f'predicted_{col}'] = quantile_preds[col].values
    
    predictions_file = f"{RESULTS_DIR}/predictions_{datetime.now().strftime('%Y%m%d_%H%M%S')}.csv"
    predictions_df.to_csv(predictions_file, index=False)
    
    print(f"\nResults saved to: {results_file}")
    print(f"Predictions saved to: {predictions_file}")
    
    # Step 11: Summary
    print("\n" + "="*60)
    print("MODELING SUMMARY")
    print("="*60)
    print(f"Best model: NGBoost with {best_dist} distribution")
    print(f"Cross-validation R²: {tuning_results['best_score']:.4f}")
    print(f"Final model R²: {final_metrics['r2']:.4f}")
    print(f"Extreme events R²: {final_metrics.get('r2_extreme', 'N/A')}")
    print(f"RMSE: {final_metrics['rmse']:.4f}")
    print(f"MAE: {final_metrics['mae']:.4f}")
    
    if feature_importance is not None:
        print(f"\nTop 3 features:")
        for i, (feature, importance) in enumerate(feature_importance.head(3).items()):
            print(f"  {i+1}. {feature}: {importance:.4f}")
    
    print("\nNext steps:")
    print("- Review results in R for detailed analysis")
    print("- Consider additional feature engineering if needed")
    print("- Evaluate temporal autocorrelation in residuals")
    print("- Test model on holdout data if available")
    
    return final_model, results

def run_quick_test():
    """
    Quick test function for development and debugging.
    """
    print("Running quick NGBoost test...")
    
    # Load data
    data = load_model_data()
    if data is None:
        return
    
    # Prepare small subset for testing
    X, y, _ = prepare_modeling_data(data, SELECTED_PREDICTORS, TARGET_VARIABLE)
    
    # Use only first 1000 observations for quick test
    if len(X) > 1000:
        X_test = X.head(1000)
        y_test = y.head(1000)
    else:
        X_test, y_test = X, y
    
    print(f"Testing with {len(X_test)} observations...")
    
    # Quick model test
    model = NGBoostModel(
        distribution=DISTRIBUTIONS['normal'],
        n_estimators=50,  # Reduced for speed
        learning_rate=0.01
    )
    
    model.fit(X_test, y_test)
    y_pred = model.predict(X_test)
    
    r2 = r2_score(y_test, y_pred)
    rmse = np.sqrt(mean_squared_error(y_test, y_pred))
    
    print(f"Quick test results:")
    print(f"  R²: {r2:.4f}")
    print(f"  RMSE: {rmse:.4f}")
    print("Quick test completed successfully!")
# 
# if __name__ == "__main__":
#     # Uncomment the line below for quick testing during development
#     # run_quick_test()
#     
#     # Run full modeling pipeline
#     model, results = main()
#         '
