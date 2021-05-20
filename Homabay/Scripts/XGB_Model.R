## This script trains and evaluates XGBoost models with datasets with missing values imputed using 
## a simple imputation approach as well as with MICE, plus with sparse data. Models are evaluated using AUC-PR.
##
## Author: Yoni Friedman
## Last edited: May 20, 2021

library(caret)
library(dplyr)
library(PRROC)
library(xgboost)

setwd("~/Kenya/NDHW Data/TestData/HTS_App/FHW App/HomaBay")
hts <- readRDS('./hts_homabay_imputed_0520.rds')


# XGBoost requires only numeric inputs, so one-hot encode all categorical variables
encodeXGBoost <- function(dataset){
  dataset$FinalTestResult <- if_else(dataset$FinalTestResult == "Positive", 1, 0)
  # Need to one-hot encode all the factor variables
  ohe_features <- names(dataset)[ sapply(dataset, is.factor) ]
  
  dmy <- dummyVars("~ Gender + KeyPopulationType + MaritalStatus + PatientDisabled + EverTestedForHIV +
                   ClientTestedAs + EntryPoint + TestingStrategy + TBScreening + ClientSelfTested +
                   month_of_test + dayofweek",
                   data = dataset)
  ohe <- data.frame(predict(dmy, newdata = dataset))
  dataset <- cbind(dataset, ohe)
  
  dataset[, !(names(dataset) %in% ohe_features)]
  
}

hts_sparse_train <- encodeXGBoost(hts$sparse$train) %>%
  select(-Facility.Name, -Longitude, -Latitude, -Sitecode)
hts_impute_mice_train <- encodeXGBoost(hts$impute_mice$train)
hts_impute_simp_train <- encodeXGBoost(hts$impute_simple$train) %>%
  select(-Facility.Name, -Longitude, -Latitude, -Sitecode)
hts_sparse_val <- encodeXGBoost(hts$sparse$val) %>%
  select(-Facility.Name, -Longitude, -Latitude, -Sitecode)
hts_impute_mice_val <- encodeXGBoost(hts$impute_mice$val)
hts_impute_simp_val <- encodeXGBoost(hts$impute_simple$val) %>%
  select(-Facility.Name, -Longitude, -Latitude, -Sitecode)

# Separately, convert target to numeric

# Sparse Model -------------------------------------------------

grid_sparse <- expand.grid(model = "xgboost",
                           sparsity = "sparse",
                           eta = c(0.01, 0.1, 0.3),
                           max_depth = c(6, 8, 10, 12, 14),
                           cs = c(.2, .3, .4, .5))

for (i in 1:nrow(grid_sparse)){
  
  set.seed(2231)
  xgb <- xgboost::xgboost(data = data.matrix(hts_sparse_train[,which(names(hts_sparse_train) != "FinalTestResult")]), 
                          label = hts_sparse_train$FinalTestResult, 
                          eta = grid_sparse[i, 3],
                          max_depth = grid_sparse[i, 4], 
                          colsample_bytree = grid_sparse[i, 5],
                          nrounds = 100,
                          objective = "binary:logistic",
                          metric = 'auc',
                          verbose = 0
  )
  
  val_predict <- predict(xgb,newdata = data.matrix(hts_sparse_val[, which(names(hts_sparse_val) != "FinalTestResult")]))
  fg <- val_predict[hts_sparse_val$FinalTestResult == 1]
  bg <- val_predict[hts_sparse_val$FinalTestResult == 0]
  prc <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
  grid_sparse$val_pr_auc[i] <- prc$auc.integral
  
}

# Simple Imputation ------------------------------------------------

grid_simple <- expand.grid(model = "xgboost",
                           sparsity = "simple",
                           eta = c(0.01, 0.1, 0.3),
                           max_depth = c(6, 8, 10, 12, 14),
                           cs = c(.2, .3, .4, .5))

for (i in 1:nrow(grid_simple)){
  
  set.seed(2231)
  xgb <- xgboost::xgboost(data = data.matrix(hts_impute_simp_train[,which(names(hts_impute_simp_train) != "FinalTestResult")]), 
                          label = hts_impute_simp_train$FinalTestResult, 
                          eta = grid_simple[i, 3],
                          max_depth = grid_simple[i, 4], 
                          colsample_bytree = grid_simple[i, 5],
                          nrounds = 100,
                          objective = "binary:logistic",
                          metric = 'auc',
                          verbose = 0
  )
  
  val_predict <- predict(xgb,newdata = data.matrix(hts_impute_simp_val[, which(names(hts_impute_simp_val) != "FinalTestResult")]))
  fg <- val_predict[hts_impute_simp_val$FinalTestResult == 1]
  bg <- val_predict[hts_impute_simp_val$FinalTestResult == 0]
  prc <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
  grid_simple$val_pr_auc[i] <- prc$auc.integral
  
}



# MICE Imputation ------------------------------------------------

grid_mice <- expand.grid(model = "xgboost",
                           sparsity = "mice",
                         eta = c(0.01, 0.1, 0.3),
                         max_depth = c(6, 8, 10, 12, 14),
                         cs = c(.2, .3, .4, .5))

for (i in 1:nrow(grid_mice)){
  
  set.seed(2231)
  xgb <- xgboost::xgboost(data = data.matrix(hts_impute_mice_train[,which(names(hts_impute_mice_train) != "FinalTestResult")]), 
                          label = hts_impute_mice_train$FinalTestResult, 
                          eta = grid_mice[i, 3],
                          max_depth = grid_mice[i, 4], 
                          colsample_bytree = grid_mice[i, 5],
                          nrounds = 100,
                          objective = "binary:logistic",
                          metric = 'auc',
                          verbose = 0
  )
  
  val_predict <- predict(xgb,newdata = data.matrix(hts_impute_mice_val[, which(names(hts_impute_mice_val) != "FinalTestResult")]))
  fg <- val_predict[hts_impute_mice_val$FinalTestResult == 1]
  bg <- val_predict[hts_impute_mice_val$FinalTestResult == 0]
  prc <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
  grid_mice$val_pr_auc[i] <- prc$auc.integral
  
}

# Save Out ------------------------------------------------------

grid <- rbind(grid_sparse, grid_simple, grid_mice)
saveRDS(grid, './homabay_grid_xgb_0520.rds')

