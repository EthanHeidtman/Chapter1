# =============================================================================
# Script Name:    TidyModelCreation.R
# Project:        Chapter1
# Author:         Ethan Heidtman
# Date Created:   2025-11-25
# Last Updated:   2025-11-25
# Description:    Takes the output of RFInitialScreening.R and creates regularized 
#                 statistical models to further screen predictors and predict
#                 salinity exceedance or raw salinity values.
# =============================================================================

# =============================================================================
# LOAD NECESSARY PACKAGES
# =============================================================================
library(here)
library(dplyr)
library(tidyverse)
library(tidymodels)
