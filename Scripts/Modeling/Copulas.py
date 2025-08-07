# =============================================================================
# Script Name:    Copulas.py
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-08-07
# Last Updated:   2025-08-07
# Description:    Defines various copulas for the copula POT model
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
import numpy as np
from scipy import stats
from scipy.optimize import minimize
#from copulae import NormalCopula, StudentCopula

class CopulaModel:
    def __init__(self):
        self.fitted = False

    def fit(self, u, v):
        raise NotImplementedError

    def cdf(self, u, v):
        raise NotImplementedError

    def conditional_cdf(self, u, v_target):
        raise NotImplementedError

class MultivariateGaussianCopula:
    """Gaussian copula implementation for multivariate modeling"""
    
    def __init__(self):
        self.correlation_matrix = None
        self.fitted = False
    
    def fit(self, uniform_data):
        """Fit Gaussian copula to uniform marginal data"""
        # Convert uniform margins to normal margins
        normal_data = norm.ppf(np.clip(uniform_data, 1e-6, 1-1e-6))
        
        # Estimate correlation matrix
        self.correlation_matrix = np.corrcoef(normal_data.T)
        
        # Ensure positive definite
        eigenvals, eigenvecs = np.linalg.eigh(self.correlation_matrix)
        eigenvals = np.maximum(eigenvals, 1e-8)
        self.correlation_matrix = eigenvecs @ np.diag(eigenvals) @ eigenvecs.T
        
        self.fitted = True
        return self.correlation_matrix
    
    def pdf(self, uniform_data):
        """Compute copula density"""
        if not self.fitted:
            raise ValueError("Copula must be fitted first")
        
        normal_data = norm.ppf(np.clip(uniform_data, 1e-6, 1-1e-6))
        
        # Multivariate normal density
        inv_corr = np.linalg.inv(self.correlation_matrix)
        det_corr = np.linalg.det(self.correlation_matrix)
        
        density = np.exp(-0.5 * np.sum((normal_data @ inv_corr) * normal_data, axis=1))
        density = density / np.sqrt(det_corr)
        
        # Adjust for marginal densities
        marginal_densities = np.prod(norm.pdf(normal_data), axis=1)
        
        return density / marginal_densities
    
    def conditional_distribution(self, conditioning_variables, conditioned_variable_idx):
        """Get conditional distribution parameters"""
        if not self.fitted:
            raise ValueError("Copula must be fitted first")
        
        # Gaussian copula conditional distribution is also Gaussian
        n_vars = self.correlation_matrix.shape[0]
        conditioning_idx = [i for i in range(n_vars) if i != conditioned_variable_idx]
        
        sigma_11 = self.correlation_matrix[conditioned_variable_idx, conditioned_variable_idx]
        sigma_12 = self.correlation_matrix[conditioned_variable_idx, conditioning_idx]
        sigma_22 = self.correlation_matrix[np.ix_(conditioning_idx, conditioning_idx)]
        
        # Conditional mean and variance
        sigma_22_inv = np.linalg.inv(sigma_22)
        conditional_mean_coef = sigma_12 @ sigma_22_inv
        conditional_var = sigma_11 - sigma_12 @ sigma_22_inv @ sigma_12.T
        
        return {
            'mean_coefficients': conditional_mean_coef,
            'conditional_variance': conditional_var
        }

class MultivariateStudentTCopula:
    """Student-t copula implementation for multivariate modeling"""
    
    def __init__(self):
        self.correlation_matrix = None
        self.degrees_of_freedom = None
        self.fitted = False
    
    def fit(self, uniform_data):
        """Fit Student-t copula to uniform marginal data"""
        # Initial estimate using Gaussian copula
        normal_data = norm.ppf(np.clip(uniform_data, 1e-6, 1-1e-6))
        initial_corr = np.corrcoef(normal_data.T)
        
        # Fit degrees of freedom using MLE
        def neg_log_likelihood(params):
            df = params[0]
            if df <= 2:
                return np.inf
            
            t_data = t_dist.ppf(np.clip(uniform_data, 1e-6, 1-1e-6), df)
            corr = np.corrcoef(t_data.T)
            
            # Ensure positive definite
            eigenvals = np.linalg.eigvals(corr)
            if np.min(eigenvals) <= 0:
                return np.inf
            
            try:
                log_lik = np.sum(t_dist.logpdf(t_data, df).sum(axis=1))
                return -log_lik
            except:
                return np.inf
        
        # Optimize degrees of freedom
        result = minimize(neg_log_likelihood, x0=[5.0], bounds=[(2.1, 50)], method='L-BFGS-B')
        
        self.degrees_of_freedom = result.x[0] if result.success else 5.0
        
        # Final correlation matrix estimate
        t_data = t_dist.ppf(np.clip(uniform_data, 1e-6, 1-1e-6), self.degrees_of_freedom)
        self.correlation_matrix = np.corrcoef(t_data.T)
        
        # Ensure positive definite
        eigenvals, eigenvecs = np.linalg.eigh(self.correlation_matrix)
        eigenvals = np.maximum(eigenvals, 1e-8)
        self.correlation_matrix = eigenvecs @ np.diag(eigenvals) @ eigenvecs.T
        
        self.fitted = True
        return {'correlation': self.correlation_matrix, 'df': self.degrees_of_freedom}
