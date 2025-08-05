# =============================================================================
# Distribution Debug Script (Corrected Version)
# =============================================================================

import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.metrics import r2_score
from ngboost import NGBoost
from ngboost.learners import default_tree_learner
from ngboost.distns import Normal, LogNormal, Exponential
from ngboost.scores import LogScore, CRPS

def debug_distributions():
    """
    Debug distribution issues by testing each one individually.
    """
    print("=" * 60)
    print("NGBoost Distribution Debug")
    print("=" * 60)
    
    # Create synthetic salinity-like data for testing
    np.random.seed(42)
    n_samples = 1000
    
    X = pd.DataFrame({
        'discharge': np.random.exponential(100, n_samples),
        'tide': np.random.normal(0, 1, n_samples),
        'season': np.random.uniform(0, 365, n_samples),
        'lag_flow': np.random.exponential(80, n_samples)
    })
    
    base_salinity = 0.05 + 0.15 * np.exp(-X['discharge'] / 50) + 0.02 * np.abs(X['tide'])
    noise = np.random.gamma(2, 0.01, size=n_samples)
    y = base_salinity + noise
    y = np.clip(y, 0.01, 2.0)
    
    print(f"Generated test data:")
    print(f"  Samples: {len(X)}")
    print(f"  Features: {list(X.columns)}")
    print(f"  Target range: {y.min():.3f} - {y.max():.3f}")
    print(f"  Target mean: {y.mean():.3f}")
    print(f"  Target std: {y.std():.3f}")
    print(f"  % zeros or negative: {(y <= 0).mean()*100:.2f}%")
    
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)

    # Use compatible score functions
    distributions_to_test = {
        'Normal':      (Normal, LogScore),
        'LogNormal':   (LogNormal, CRPS),
        'Exponential': (Exponential, CRPS)
    }

    
    results = {}
    
    for dist_name, (dist_class, score_func) in distributions_to_test.items():
        print(f"\n{'-'*40}")
        print(f"Testing {dist_name} Distribution")
        print(f"{'-'*40}")
        
        try:
            if dist_name == 'LogNormal':
                y_train_trans = y_train + 1e-6
                y_test_trans = y_test + 1e-6
            else:
                y_train_trans = y_train
                y_test_trans = y_test
            
            model = NGBoost(
                Base=default_tree_learner,
                Dist=dist_class,
                Score=score_func,
                n_estimators=50,
                learning_rate=0.01,
                verbose=False,
                random_state=42
            )
            
            print(f"  Model created successfully")
            print(f"  Fitting model...")
            model.fit(X_train, y_train_trans)
            print(f"  Model fitted successfully")
            
            pred_dist = model.pred_dist(X_test)
            y_pred = pred_dist.loc

            if dist_name == 'LogNormal':
                y_pred = np.exp(y_pred) - 1e-6
            
            r2 = r2_score(y_test, y_pred)
            rmse = np.sqrt(np.mean((y_test - y_pred) ** 2))
            
            print(f"  Predictions made successfully")
            print(f"  R²: {r2:.4f}")
            print(f"  RMSE: {rmse:.4f}")
            print(f"  Prediction range: {y_pred.min():.3f} - {y_pred.max():.3f}")
            
            # Test quantiles (no transpose)
            try:
                quantiles = pred_dist.ppf([0.05, 0.5, 0.95])
                print(f"  ✓ Quantile predictions work")
                print(f"    5th percentile range: {quantiles[:, 0].min():.3f} - {quantiles[:, 0].max():.3f}")
                print(f"    95th percentile range: {quantiles[:, 2].min():.3f} - {quantiles[:, 2].max():.3f}")
            except Exception as e:
                print(f"  ✗ Quantile error: {e}")
            
            results[dist_name] = {
                'success': True,
                'r2': r2,
                'rmse': rmse,
                'pred_range': (y_pred.min(), y_pred.max())
            }
        
        except Exception as e:
            print(f"  ERROR: {type(e).__name__}: {e}")
            results[dist_name] = {
                'success': False,
                'error': str(e)
            }
    
    print(f"\n{'='*60}")
    print("DISTRIBUTION TEST SUMMARY")
    print(f"{'='*60}")
    
    for dist_name, result in results.items():
        if result['success']:
            print(f"{dist_name:12}: ✓ SUCCESS - R² = {result['r2']:.4f}, RMSE = {result['rmse']:.4f}")
        else:
            print(f"{dist_name:12}: ✗ FAILED - {result['error']}")
    
    return results

def test_ngboost_version():
    """
    Test NGBoost version and available components.
    """
    print("\nTesting NGBoost Installation:")
    
    try:
        import ngboost
        print(f"  NGBoost version: {ngboost.__version__}")
    except:
        print("  Could not determine NGBoost version")
    
    try:
        from ngboost.distns import Normal, LogNormal, Exponential
        print("  ✓ Distribution imports successful")
        normal_dist = Normal
        print(f"  ✓ Normal distribution: {normal_dist}")
        print(f"    Available methods: {[m for m in dir(normal_dist) if not m.startswith('_')]}")
    except Exception as e:
        print(f"  ✗ Distribution import error: {e}")
    
    try:
        from ngboost.scores import LogScore, CRPS
        print("  ✓ Score function imports successful")
    except Exception as e:
        print(f"  ✗ Score function import error: {e}")

if __name__ == "__main__":
    test_ngboost_version()
    debug_results = debug_distributions()
