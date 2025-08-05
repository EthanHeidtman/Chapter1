import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import train_test_split
from scipy.stats import burr12
from sklearn.ensemble import RandomForestRegressor

data = pd.read_csv('Data/Tidied/Final/CleanFinalModelData.csv')
data['DateTime'] =pd.to_datetime(data['DateTime'], format="%Y-%m-%d %H:%M:%S", errors='coerce')
data['IsLowInflow'] = data['IsLowInflow'].astype(bool)
data['DayOfYear_sin'] = np.sin(2 * np.pi * data_model['DayOfYear'] / 365)
data['DayOfYear_cos'] = np.cos(2 * np.pi * data_model['DayOfYear'] / 365)

# with pd.option_context('display.max_columns', None):
#     print(data.head(10))
    
predictors = [
    'Norm_TideRange24',
    'Norm_PowLagDischarge72',
    'Norm_RollingPowDischarge14',
    'Norm_PowLagInflows96',
    'Norm_RollingPowInflows10',
    'IsLowInflow',
    'Norm_CumulativeInflowDeficit30',
    'DayOfYear_sin',
    'DayOfYear_cos'
]

data_model = data.dropna(subset=predictors + ['Salinity']).copy()

data_model['exceed_0_5'] = (data_model['Salinity'] > 0.5).astype(int)

X = data_model[predictors]
y = data_model['exceed_0_5']

# --- Step 3: Train binary classifier ---
clf = RandomForestClassifier(n_estimators=100, random_state=42)
clf.fit(X, y)

# Predict probability P(Salinity > 0.5)
data_model['p_gt_0_5'] = clf.predict_proba(X)[:, 1]

# --- Step 2: Prepare exceedance subset for Burr fitting ---
exceed_df = data_model[data_model['Salinity'] > 0.5].copy()
exceed_df['excess'] = exceed_df['Salinity'] - 0.5

# Fill missing discharge predictor for binning
exceed_df['Norm_PowLagDischarge72'] = exceed_df['Norm_PowLagDischarge72'].fillna(exceed_df['Norm_PowLagDischarge72'].median())

# --- Step 3: Bin exceedances by discharge quantiles ---
exceed_df['discharge_bin'] = pd.qcut(exceed_df['Norm_PowLagDischarge72'], q=20, duplicates='drop')

# --- Step 4: Fit Burr parameters per bin ---
params_list = []
bin_centers = []

for bin_name, group in exceed_df.groupby('discharge_bin'):
    excess_vals = group['excess']
    if len(excess_vals) > 10:  # Minimum data to fit
        c, d, loc, scale = burr12.fit(excess_vals, floc=0)  # fix loc=0 for stability
        params_list.append({'c': c, 'd': d, 'scale': scale})
        bin_centers.append(group['Norm_PowLagDischarge72'].median())

params_df = pd.DataFrame(params_list)
params_df['discharge_median'] = bin_centers

# --- Step 5: Train regressors for Burr params ---
reg_c = RandomForestRegressor(random_state=42)
reg_d = RandomForestRegressor(random_state=42)
reg_scale = RandomForestRegressor(random_state=42)

X_reg = params_df[['discharge_median']]
reg_c.fit(X_reg, params_df['c'])
reg_d.fit(X_reg, params_df['d'])
reg_scale.fit(X_reg, params_df['scale'])

# --- Step 6: Predict Burr params dynamically for all data points ---
# Fill missing discharge predictor in full data
data_model['Norm_PowLagDischarge72'] = data_model['Norm_PowLagDischarge72'].fillna(data_model['Norm_PowLagDischarge72'].median())
X_pred = data_model[['Norm_PowLagDischarge72']].rename(columns={'Norm_PowLagDischarge72': 'discharge_median'})

c_pred = reg_c.predict(X_pred)
d_pred = reg_d.predict(X_pred)
scale_pred = reg_scale.predict(X_pred)

# --- Step 7: Compute dynamic tail exceedance probability ---
excess_threshold = 1.0 - 0.5  # excess for threshold 1.0

p_tail_dynamic = burr12.sf(excess_threshold, c_pred, d_pred, loc=0, scale=scale_pred)

# --- Step 8: Combine classification and tail probability ---
data_model['p_gt_1_0_dynamic'] = data_model['p_gt_0_5'] * p_tail_dynamic

# --- Inspect results ---
print(data_model[['Salinity', 'p_gt_0_5', 'p_gt_1_0_dynamic']].sort_values('p_gt_1_0_dynamic', ascending=False).head(10))





## Analysis:

print("Summary of P(Salinity > 0.5):")
print(data_model['p_gt_0_5'].describe())

print("\nSummary of P(Salinity > 1.0) - dynamic tail:")
print(data_model['p_gt_1_0_dynamic'].describe())


import matplotlib.pyplot as plt
import numpy as np

# Bin predicted probabilities into 10 bins
bins = np.linspace(0, 1, 11)
data_model['p_gt_0_5_bin'] = pd.cut(data_model['p_gt_0_5'], bins)

# Calculate observed frequency of exceedance per bin
calibration = data_model.groupby('p_gt_0_5_bin').apply(
    lambda df: pd.Series({
        'mean_pred_prob': df['p_gt_0_5'].mean(),
        'observed_freq': df['exceed_0_5'].mean(),
        'count': len(df)
    })
).reset_index()

plt.figure(figsize=(8, 6))
plt.plot(calibration['mean_pred_prob'], calibration['observed_freq'], 'o-', label='Observed')
plt.plot([0, 1], [0, 1], 'k--', label='Perfect calibration')
plt.xlabel('Mean Predicted Probability')
plt.ylabel('Observed Frequency')
plt.title('Calibration plot for P(Salinity > 0.5)')
plt.legend()
plt.grid(True)
plt.show()

from sklearn.metrics import roc_curve, auc

fpr, tpr, thresholds = roc_curve(data_model['exceed_0_5'], data_model['p_gt_0_5'])
roc_auc = auc(fpr, tpr)

plt.figure(figsize=(8,6))
plt.plot(fpr, tpr, label=f'ROC curve (AUC = {roc_auc:.3f})')
plt.plot([0, 1], [0, 1], 'k--')
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC Curve for Salinity > 0.5 Classification')
plt.legend(loc='lower right')
plt.grid(True)
plt.show()


plt.figure(figsize=(8,6))
plt.hist(data_model['p_gt_1_0_dynamic'], bins=50, alpha=0.7)
plt.xlabel('Predicted Probability of Salinity > 1.0')
plt.ylabel('Count')
plt.title('Distribution of Dynamic Tail Exceedance Probabilities')
plt.show()


extreme_df = data_model[data_model['Salinity'] > 1.0]

plt.figure(figsize=(8,6))
plt.scatter(range(len(extreme_df)), extreme_df['p_gt_1_0_dynamic'], alpha=0.6)
plt.xlabel('Extreme Event Index')
plt.ylabel('Predicted P(Salinity > 1.0)')
plt.title('Predicted Tail Probabilities on Extreme Salinity Events')
plt.show()

from sklearn.metrics import brier_score_loss

brier = brier_score_loss(data_model['exceed_0_5'], data_model['p_gt_0_5'])
print(f"Brier score for P(Salinity > 0.5): {brier:.4f}")

