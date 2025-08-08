# =============================================================================
# Script Name:    ModelMetrics.py
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-08-08
# Last Updated:   2025-08-08
# Description:    Defines a suite of functions that compute relevant metrics at
#                 different points throughout the Copula/POT modeling workflow.
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
import numpy as np
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
from sklearn.metrics import roc_auc_score, average_precision_score, brier_score_loss


class ModelMetrics:
    """
    Simplified, reusable static methods to evaluate POT-copula model components.
    """

    @staticmethod
    def pot_parameter_metrics(true_params, predicted_params):
        """
        Evaluate prediction quality of POT tail parameters.
        Returns RMSE, MAE, R2, and MAPE (for scale-like parameters).
        """
        metrics = {}
        common_params = set(true_params.columns).intersection(predicted_params.columns)
        
        for param in common_params:
            true_vals = true_params[param].dropna()
            pred_vals = predicted_params[param].dropna()
            idx = true_vals.index.intersection(pred_vals.index)
            if len(idx) == 0:
                continue
            true_aligned = true_vals.loc[idx]
            pred_aligned = pred_vals.loc[idx]

            metrics[f'{param}_rmse'] = np.sqrt(mean_squared_error(true_aligned, pred_aligned))
            metrics[f'{param}_mae'] = mean_absolute_error(true_aligned, pred_aligned)
            metrics[f'{param}_r2'] = r2_score(true_aligned, pred_aligned)

            if param in ['scale', 'sigma', 'beta']:
                rel_error = np.abs((pred_aligned - true_aligned) / (true_aligned + 1e-8))
                metrics[f'{param}_mape'] = 100 * np.mean(rel_error)

        return metrics

    @staticmethod
    def exceedance_prediction_metrics(true_exceedances, predicted_probabilities):
        """
        Evaluate exceedance probability predictions.
        Includes AUC-ROC, AUC-PR, Brier score, and log-likelihood.
        """
        valid_mask = ~(np.isnan(true_exceedances) | np.isnan(predicted_probabilities))
        true_vals = true_exceedances[valid_mask]
        pred_probs = np.clip(predicted_probabilities[valid_mask], 1e-8, 1-1e-8)
        if len(true_vals) == 0:
            return {'error': 'No valid data'}

        metrics = {}

        # Binary classification metrics only if true labels are binary
        if set(np.unique(true_vals)) <= {0,1}:
            metrics['auc_roc'] = roc_auc_score(true_vals, pred_probs)
            metrics['auc_pr'] = average_precision_score(true_vals, pred_probs)
            metrics['brier_score'] = brier_score_loss(true_vals, pred_probs)

        # Log-likelihood as mean log-probability
        metrics['log_likelihood'] = np.mean(
            true_vals * np.log(pred_probs) + (1 - true_vals) * np.log(1 - pred_probs)
        )

        return metrics

    @staticmethod
    def copula_fit_quality_metrics(uniform_data, copula_model):
        """
        Evaluate copula fit quality via log-likelihood, AIC, BIC,
        and goodness-of-fit tests using Rosenblatt transform KS statistics.
        """
        metrics = {}
        try:
            ll = copula_model.log_likelihood(uniform_data)
            n, d = uniform_data.shape
            k = copula_model.n_params()
            metrics['log_likelihood'] = ll
            metrics['aic'] = -2 * ll + 2 * k
            metrics['bic'] = -2 * ll + np.log(n) * k

            if hasattr(copula_model, 'rosenblatt_transform'):
                residuals = copula_model.rosenblatt_transform(uniform_data)
                from scipy.stats import kstest
                ks_stats = [kstest(residuals[:, i], 'uniform')[0] for i in range(residuals.shape[1])]
                metrics['max_ks_statistic'] = max(ks_stats)

        except Exception as e:
            metrics['error'] = str(e)

        return metrics

    @staticmethod
    def flow_requirement_metrics(estimated_flows, actual_flows, exceedance_events):
        """
        Evaluate flow requirement effectiveness.
        Metrics include shortage frequency, failure rate, false alarm rate,
        mean excess flow, and overall protection rate.
        """
        idx = estimated_flows.index.intersection(actual_flows.index).intersection(exceedance_events.index)
        if len(idx) == 0:
            return {'error': 'No overlapping time periods'}

        est = estimated_flows.loc[idx]
        act = actual_flows.loc[idx]
        exc = exceedance_events.loc[idx]

        shortage = act < est

        metrics = {
            'shortage_frequency': shortage.mean(),
            'failure_rate_given_shortage': exc[shortage].mean() if shortage.sum() > 0 else np.nan,
            'false_alarm_rate': exc[~shortage].mean() if (~shortage).sum() > 0 else np.nan,
            'mean_excess_flow': np.maximum(0, est - act).mean(),
            'overall_protection_rate': ((~exc) | (~shortage)).mean()
        }

        return metrics
