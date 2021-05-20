## This script trains and evaluates random forest models with datasets with missing values imputed using 
## a simple imputation approach as well as with MICE. Models are evaluated using AUC-PR.
##
## Author: Yoni Friedman
## Last edited: May 20, 2021

library(caret)
library(dplyr)
library(PRROC)
library(randomForest)

setwd("~/Kenya/NDHW Data/TestData/HTS_App/FHW App/HomaBay")
hts <- readRDS('./hts_homabay_imputed_0520.rds')


hts_impute_mice_train <- hts$impute_mice$train 
hts_impute_simp_train <- hts$impute_simple$train %>%
  select(-Facility.Name, -Longitude, -Latitude, -Sitecode)
hts_impute_mice_val <- hts$impute_mice$val 
hts_impute_simp_val <- hts$impute_simple$val %>%
  select(-Facility.Name, -Longitude, -Latitude, -Sitecode)

# Simple Imputation ------------------------------------------------

grid_simple <- expand.grid(model = "randomforest",
                           sparsity = "imputed_simple",
                           mtry = c(4,5,6,7,8),
                           nodesize = c(5,1))

for (i in 1:nrow(grid_simple)){
  
  set.seed(2231)
  print(i)
  rf <- randomForest(
    FinalTestResult ~ .,
    data = hts_impute_simp_train,
    mtry = grid_simple[i, 3],
    nodesize = grid_simple[i, 4],
    importance = TRUE
  )
  
  val_predict = predict(rf, newdata=hts_impute_simp_val, type = "prob")
  fg <- val_predict[hts_impute_simp_val$FinalTestResult == "Positive", 2]
  bg <- val_predict[hts_impute_simp_val$FinalTestResult == "Negative", 2]
  prc <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
  grid_simple$val_pr_auc[i] <- prc$auc.integral
  # print(prc$auc.integral)
  
}
print(grid_simple %>% arrange(desc(val_pr_auc)))


# MICE Imputation ------------------------------------------------

grid_mice <- expand.grid(model = "randomforest",
                         sparsity = "imputed_mice",
                         mtry = c(4,5,6,7,8),
                         nodesize = c(5,1))

for (i in 1:nrow(grid_mice)){
  
  set.seed(2231)
  print(i)
  rf <- randomForest(
    FinalTestResult ~ .,
    data = hts_impute_mice_train,
    mtry = grid_mice[i, 3],
    nodesize = grid_mice[i, 4],
    importance = TRUE
  )
  
  val_predict = predict(rf, newdata=hts_impute_mice_val, type = "prob")
  fg <- val_predict[hts_impute_mice_val$FinalTestResult == "Positive", 2]
  bg <- val_predict[hts_impute_mice_val$FinalTestResult == "Negative", 2]
  prc <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
  grid_mice$val_pr_auc[i] <- prc$auc.integral
  # print(prc$auc.integral)
  
}
print(grid_mice %>% arrange(desc(val_pr_auc)))
# Save Out ------------------------------------------------------

grid <- rbind(grid_simple, grid_mice)
saveRDS(grid, './homabay_grid_rf_0520.rds')