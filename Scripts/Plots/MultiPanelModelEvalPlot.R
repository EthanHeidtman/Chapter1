# =============================================================================
# Script Name:    MultiPanelModelEvalPlot.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-08-28
# Last Updated:   2025-08-28
# Description:    Takes the output of the rolling window experiments and generates
#                 a plot that contains a panel for the predicted exceedance,
#                 the raw salinity, and each of the predictors
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(purrr)