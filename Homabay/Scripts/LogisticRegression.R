## This script trains and evaluates logistic regression with datasets with missing values imputed using 
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


# Logistic Regression with Simple Imputation --------------------------
log_model <- glm(as.factor(FinalTestResult) ~ ., data = hts_impute_simp_train,
                 family = binomial(link="logit"))

summary(log_model)

log_predict <- predict(log_model,newdata = hts_impute_simp_val,type = "response")

fg <- log_predict[hts_impute_simp_val$FinalTestResult == "Positive"]
bg <- log_predict[hts_impute_simp_val$FinalTestResult == "Negative"]
prc <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(prc)

# Logistic Regression with MICE --------------------------
log_model <- glm(as.factor(FinalTestResult) ~ ., data = hts_impute_mice_train,
                 family = binomial(link="logit"))

summary(log_model)

log_predict <- predict(log_model,newdata = hts_impute_mice_val,type = "response")

fg <- log_predict[hts_impute_mice_val$FinalTestResult == "Positive"]
bg <- log_predict[hts_impute_mice_val$FinalTestResult == "Negative"]
prc <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(prc)


