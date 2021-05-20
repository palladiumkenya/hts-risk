## This script combines patient-level HTS data from NDWH with geospatial data to prepare a model-ready dataset
##
## Author: Yoni Friedman
## Last edited: May 19, 2021

# Load libraries ----------------
library(caret)
library(dplyr)
library(lubridate)
library(mice)

# Read in HTS and geo data -------------------
setwd("~/Kenya/NDHW Data/TestData/HTS_App/FHW App/HomaBay")

# Read in HTS NDWH data
hts <- readRDS('./htsdata_0519.rds')
# Filter to Homa Bay
hts <- hts %>% filter(County == 'HOMA BAY') %>% select(-County)

# Read in Local Demographic and HIV data
facilities <- readRDS('./facilities_homabay_3km_20210519.rds')
# Filter to facilities included in hts
facilities <- facilities %>%
  filter(Facility.Name %in% unique(hts$FacilityName))
# Convert demographic counts to shares of population
facilities[, 23:36] <- facilities[, 23:36] / facilities[, 22]

# Use PCA to synthesize the GIS data -------------
fac_pca <- facilities %>% select(4:ncol(facilities))
# Remove any varaibles with zero variance as these add no value
no_var <- apply(fac_pca, 2, var)
fac_pca <- fac_pca[, no_var != 0]
fac_pca <- prcomp(fac_pca, center = TRUE,scale. = TRUE) # center and scale data prior to running PCA
summary(fac_pca) # ten components capture 99% the variance
fac_pca_out <- fac_pca$x[, 1:10]

facilities <- cbind.data.frame(facilities[, 1:3], fac_pca_out)

# Join HTS and GIS data -------------------------
# left join so we don't lose any records
hts <- merge(facilities, hts, by.x = "Facility.Name", by.y = "FacilityName", all.y = TRUE)

# Group rare classes -----------------------------------------
# Key Population Type - group into
kp_other <- names(which(prop.table(table(hts$KeyPopulationType)) < .01))
hts$KeyPopulationType <- ifelse(hts$KeyPopulationType %in% kp_other, 'OtherKP', hts$KeyPopulationType)

# If any entry point has <1% of observations, group it in Other
ep_other <- names(which(prop.table(table(hts$EntryPoint)) < .01))
hts$EntryPoint <- ifelse(hts$EntryPoint %in% ep_other, 'Other', hts$EntryPoint)


# Convert to factor variables ---------------
hts$Gender <- factor(hts$Gender, levels = c("Male", "Female"))
hts$PatientDisabled <- factor(hts$PatientDisabled, levels = c("Disabled", "Not Disabled"))
hts$MaritalStatus <- factor(hts$MaritalStatus, levels = c("Married","Minor", "Divorced", "Widowed","Polygamous"))
hts$KeyPopulationType <- factor(hts$KeyPopulationType, levels = c("GP", "SW", "MSM", "OtherKP"))
hts$EverTestedForHIV <- factor(hts$EverTestedForHIV, levels = c("Yes", "No"))
hts$dayofweek <- factor(hts$dayofweek, levels = c('1', '2', '3', '4','5', '6', '7'))
hts$month_of_test <- factor(hts$month_of_test, levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12'))
hts$TBScreening <- factor(hts$TBScreening, levels = c("No Presumed TB", "Presumed TB"))
hts$EntryPoint <- factor(hts$EntryPoint, levels = c("MTC", "OPD", "IPD", "MOBILE", "VCT",  "PEDS", "HB", "Other"))
hts$TestingStrategy <- factor(hts$TestingStrategy, levels = c("VCT","MOBILE", "HB"))
hts$ClientSelfTested <- factor(hts$ClientSelfTested, levels = c("Yes", "No"))
hts$ClientTestedAs <- factor(hts$ClientTestedAs, levels = c("Individual", "Couple"))
hts$FinalTestResult <- factor(hts$FinalTestResult, levels = c("Negative", "Positive"))
hts$MonthsSinceLastTest <- as.numeric(hts$MonthsSinceLastTest)
hts$AgeAtTest <- as.numeric(hts$AgeAtTest)

# Imputation ------------------------------
set.seed(2231)
#partition and create training, evaluation testing data
split <- createDataPartition(y = hts$FinalTestResult,p = 0.6,list = FALSE)

# split into train and test
train <- hts[split, ] 
test_all <- hts[-split, ]

# split train into train and validation
split_test <- createDataPartition(y = test_all$FinalTestResult,p = 0.5,list = FALSE)
val <- test_all[split_test, ]
test <- test_all[-split_test, ]

# MICE Imputation -------------------------------
# Select variables for imputation (no target variable and no id variables)
cols_to_not_impute <- c("Facility.Name", "Longitude", "Latitude", "Sitecode")
train_to_impute <- train[, !names(train) %in% cols_to_not_impute]
test_all_to_impute <- test_all[, !names(test) %in% cols_to_not_impute]

# https://github.com/prockenschaub/Misc/blob/master/R/mice.reuse/mice.reuse_example.md
source('./mice_reuse.R')

hts_impute_mice <- mice(train_to_impute,
                       m = 5,
                       maxit = 5,
                       seed = 2231)
hts_impute_mice_train <- complete(hts_impute_mice)

hts_impute_mice_test_all <- mice.reuse(hts_impute_mice, test_all_to_impute, maxit = 1)

hts_impute_mice_val <- hts_impute_mice_test_all[[2]][split_test, ]
hts_impute_mice_test <- hts_impute_mice_test_all[[2]][-split_test, ]

# Simple Imputation ------------------------------
# For continuous variables, take the mean; for categorical variables, take the mode
Mode <- function(x, na.rm = TRUE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# Impute missing values with mode or mean from train set
hts$MaritalStatus <- if_else(is.na(hts$MaritalStatus), Mode(train$MaritalStatus), hts$MaritalStatus)
hts$EntryPoint <- if_else(is.na(hts$EntryPoint), Mode(train$EntryPoint), hts$EntryPoint)
hts$TestingStrategy <- if_else(is.na(hts$TestingStrategy), Mode(train$TestingStrategy), hts$TestingStrategy)
hts$TBScreening <- if_else(is.na(hts$TBScreening), Mode(train$TBScreening), hts$TBScreening)
hts$ClientSelfTested <- if_else(is.na(hts$ClientSelfTested), Mode(train$ClientSelfTested), hts$ClientSelfTested)
hts$MonthsSinceLastTest <- if_else(is.na(hts$MonthsSinceLastTest), mean(train$MonthsSinceLastTest, na.rm = TRUE), hts$MonthsSinceLastTest)
hts$MonthsSinceLastTest <- if_else(hts$EverTestedForHIV == "No", 0, hts$MonthsSinceLastTest)

# split into train and test
hts_impute_simple_train <- hts[split, ] 
test_all <- hts[-split, ]
hts_impute_simple_val <- test_all[split_test, ]
hts_impute_simple_test <- test_all[-split_test, ]




outlist <- list(
  "sparse" = list(
    "train" = train,
    "val" = val,
    "test" = test
  ),
  "impute_mice" = list(
    "train" = hts_impute_mice_train,
    "val" = hts_impute_mice_val,
    "test" = hts_impute_mice_test
  ),
  "impute_simple" = list(
    "train" = hts_impute_simple_train,
    "val" = hts_impute_simple_val,
    "test" = hts_impute_simple_test
  )
)

saveRDS(outlist, './hts_homabay_imputed_0520.rds')

