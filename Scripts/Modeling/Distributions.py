# =============================================================================
# Script Name:    Distributions.py
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-07-28
# Last Updated:   2025-07-28
# Description:    Implements a two-stage modeling approach that first classifies
#                 the likelihood of salinity exceeding a lower threshold, then 
#                 uses various tail distributions to fit the conditional probability
#                 of exceeding the 1.0 salinity threshold. 
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
import numpy as np
from scipy.stats import burr12, genpareto, gengamma, lognorm, fisk, gamma

class Distribution:
    def fit_params(self, excesses):
        raise NotImplementedError

    def pdf(self, x, params):
        raise NotImplementedError

    def cdf(self, x, params):
        raise NotImplementedError

    def param_names(self):
        raise NotImplementedError


class Burr(Distribution):
    def fit_params(self, excesses):
        c, d, loc, scale = burr12.fit(excesses, floc=0)
        return {'c': c, 'd': d, 'scale': scale}

    def pdf(self, x, params):
        return burr12.pdf(x, params['c'], params['d'], loc=0, scale=params['scale'])

    def cdf(self, x, params):
        return burr12.cdf(x, params['c'], params['d'], loc=0, scale=params['scale'])

    def param_names(self):
        return ['c', 'd', 'scale']


class GPD(Distribution):
    def fit_params(self, excesses):
        c, loc, scale = genpareto.fit(excesses, floc=0)
        return {'c': c, 'scale': scale}

    def pdf(self, x, params):
        return genpareto.pdf(x, params['c'], loc=0, scale=params['scale'])

    def cdf(self, x, params):
        return genpareto.cdf(x, params['c'], loc=0, scale=params['scale'])

    def param_names(self):
        return ['c', 'scale']


class GenGamma(Distribution):
    def fit_params(self, excesses):
        a, b, loc, scale = gengamma.fit(excesses, floc=0)
        return {'a': a, 'b': b, 'scale': scale}

    def pdf(self, x, params):
        return gengamma.pdf(x, params['a'], params['b'], loc=0, scale=params['scale'])

    def cdf(self, x, params):
        return gengamma.cdf(x, params['a'], params['b'], loc=0, scale=params['scale'])

    def param_names(self):
        return ['a', 'b', 'scale']


class Lognormal(Distribution):
    def fit_params(self, excesses):
        shape, loc, scale = lognorm.fit(excesses, floc=0)
        return {'shape': shape, 'scale': scale}

    def pdf(self, x, params):
        return lognorm.pdf(x, params['shape'], loc=0, scale=params['scale'])

    def cdf(self, x, params):
        return lognorm.cdf(x, params['shape'], loc=0, scale=params['scale'])

    def param_names(self):
        return ['shape', 'scale']


class Loglogistic(Distribution):  # fisk = log-logistic
    def fit_params(self, excesses):
        c, loc, scale = fisk.fit(excesses, floc=0)
        return {'c': c, 'scale': scale}

    def pdf(self, x, params):
        return fisk.pdf(x, params['c'], loc=0, scale=params['scale'])

    def cdf(self, x, params):
        return fisk.cdf(x, params['c'], loc=0, scale=params['scale'])

    def param_names(self):
        return ['c', 'scale']


class Gamma(Distribution):
    def fit_params(self, excesses):
        a, loc, scale = gamma.fit(excesses, floc=0)
        return {'a': a, 'scale': scale}

    def pdf(self, x, params):
        return gamma.pdf(x, params['a'], loc=0, scale=params['scale'])

    def cdf(self, x, params):
        return gamma.cdf(x, params['a'], loc=0, scale=params['scale'])

    def param_names(self):
        return ['a', 'scale']
