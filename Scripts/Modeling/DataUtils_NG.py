import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.metrics import r2_score
from ngboost import NGBoost
from ngboost.learners import default_tree_learner
from ngboost.distns import Normal, LogNormal
from ngboost.scores import LogScore, CRPS

def debug_distributions():
    print("=" * 60)
    print("NGBoost Distribution Debug")
    print("=" * 60)

    # Create synthetic salinity-like data
    np.random.seed(42)
    n_samples = 1000
    X = pd.DataFrame({
        'discharge': np.random.exponential(100, n_samples),
        'tide': np.random.normal(0, 1, n_samples),
        'season': np.random.uniform(0, 365, n_samples),
        'lag_flow': np.random.exponential(80, n_samples)
    })
    base_salinity = 0.05 + 0.15 * np.exp(-X['discharge'] / 50) + 0.02 * np.abs(X['tide'])
    noise = np.random.gamma(2, 0.01)
    y = base_salinity + noise
    y = np.clip(y, 0.01, 2.0)

    print(f"Generated test data:")
    print(f"  Samples: {len(X)}")
    print(f"  Target range: {y.min():.3f} - {y.max():.3f}")
    print(f"  Target mean: {y.mean():.3f}, std: {y.std():.3f}")

    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)

    combos = [
    ("Normal", Normal, LogScore),
    ("Normal (CRPS)", Normal, CRPS),
    ("LogNormal (CRPS)", LogNormal, CRPS),
    ("Exponential (CRPS)", Exponential, CRPS)
]

    results = {}

    for name, Dist, Score in combos:
        print(f"\n{'-'*40}\nTesting: {name}\n{'-'*40}")

        try:
            # Handle log-transform for LogNormal
            if Dist == LogNormal:
                if np.any(y_train <= 0):
                    raise ValueError("LogNormal requires strictly positive target values.")
                y_train_trans = y_train
                y_test_trans = y_test
            else:
                y_train_trans = y_train
                y_test_trans = y_test

            model = NGBoost(
                Dist=Dist,
                Score=Score,
                Base=default_tree_learner,
                n_estimators=50,
                learning_rate=0.01,
                verbose=False,
                random_state=42
            )
            print("  Model created.")
            model.fit(X_train, y_train_trans)
            print("  Model fitted.")

            pred_dist = model.pred_dist(X_test)
            y_pred = pred_dist.loc

            if Dist == LogNormal:
                y_pred = np.exp(y_pred)  # Optional: depends on model behavior

            r2 = r2_score(y_test_trans, y_pred)
            rmse = np.sqrt(np.mean((y_test_trans - y_pred) ** 2))

            print(f"  ✓ Predictions OK — R²: {r2:.4f}, RMSE: {rmse:.4f}")
            print(f"  Prediction range: {y_pred.min():.3f} - {y_pred.max():.3f}")

            try:
                q = pred_dist.ppf([0.05, 0.5, 0.95])  # shape (3, n_samples)
                if reverse_transform:
                    q = np.exp(q) - 1e-6
                
                # q is (3, n_samples), so compute min/max along axis=1 for each quantile
                print(f"    5th percentile: {q[0].min():.3f} - {q[0].max():.3f}")
                print(f"    Median       : {q[1].min():.3f} - {q[1].max():.3f}")
                print(f"    95th percentile: {q[2].min():.3f} - {q[2].max():.3f}")
            except Exception as e:
                print(f"  ✗ Quantile error: {e}")

            results[name] = {
                'success': True,
                'r2': r2,
                'rmse': rmse,
                'range': (y_pred.min(), y_pred.max())
            }

        except Exception as e:
            print(f"  ✗ ERROR: {type(e).__name__}: {e}")
            results[name] = {
                'success': False,
                'error': str(e)
            }

    print(f"\n{'='*60}\nSUMMARY\n{'='*60}")
    for name, res in results.items():
        if res['success']:
            print(f"{name:20}: ✓ R²={res['r2']:.4f}, RMSE={res['rmse']:.4f}")
        else:
            print(f"{name:20}: ✗ {res['error']}")

    return results

def test_ngboost_version():
    import ngboost
    print("\nTesting NGBoost Installation:")
    print(f"  NGBoost version: {ngboost.__version__}")
    print("  ✓ Distributions and scoring modules loaded.")

if __name__ == "__main__":
    test_ngboost_version()
    debug_results = debug_distributions()
