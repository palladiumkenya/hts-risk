## This script runs the entire pipeline from processing NDWH and GIS data, feature generation and 
## dimensionality reduction, missing value imputation, model training and evaluation, analysis
## of the best performing model, and generation of app files.
##
## Author: Yoni Friedman
## Last edited: August 4, 2021

library(raster)
library(sp)
library(rgdal)
library(dplyr)
library(rgeos)
library(ggmap)
library(caret)
library(lubridate)
library(mice)
library(PRROC)
library(xgboost)

setwd("~/Kenya/NDHW Data/TestData/HTS_App/FHW App/HomaBay")

source('./GIS_Collection.R') #gisCollect()
source('./NDWH_Prep.R') # ndwhPrep()
source('./mice_reuse.R')
source('./NDWH_GIS_Prep.R') #ndwhGISPrep()
source('./App_File_Generation.R')

# Read in dataframe that contains columns: Facility.Name, Longitude, Latitude
facilities <- read.csv('KenyaHMIS Facility geocodes.csv', stringsAsFactors = FALSE)
gis_data <- gisCollect(facilities, "HOMA BAY")

# Read in patient-level HTS data
# FinalTestResult, TestType, DOB, TestDate, KeyPopType, MaritalStatus, EntryPoint, TestingStrategy,
# PatientDisabled, EverTestedForHIV, MonthsSinceLastTest, ClientTestedAs, TBScreening, ClientSelfTested,
# Gender, County, Sitecode, FacilityName
hts <- read.csv('~/Kenya/Data/HTS FEB 20212.csv', stringsAsFactors = FALSE)
ndwh_data <- ndwhPrep(hts)

# Combine NDWH and GIS data
model_data <- ndwhGISPrep(ndwh_data, gis_data, "HOMA BAY")

# Generate model and cutoffs
output_files <- prepAppFiles(model_data)

# Save out imputed data, facilities, XGB model, and cutoff list, and read in global.R
saveRDS(model_data, 'hts_data_imputed.rds')
saveRDS(output_files$cutoff_list, 'cutoff_list.rds')
saveRDS(output_files$xgb_model, 'xgb_model.rds')
saveRDS(model_data$facilities, 'facilities.rds')
