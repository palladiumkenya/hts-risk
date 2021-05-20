## This script runs the entire pipeline from processing NDWH and GIS data, feature generation and 
## dimensionality reduction, missing value imputation, model training and evaluation, and analysis
## of the best performing model.
##
## Author: Yoni Friedman
## Last edited: May 20, 2021

setwd("~/Kenya/NDHW Data/TestData/HTS_App/FHW App/HomaBay")

source('./GIS_Collection.R')
source('./NDWH_Prep.R')
source('./NDWH_GIS_Prep.R')
source('./RF_Model.R')
source('./XGB_Model.R')
source('./Prediction_Analysis.R')