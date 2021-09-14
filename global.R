library(shiny)
library(randomForest)
library(caret)
library(xgboost)
library(dplyr)
library(RSQLite)
library(shinythemes)
library(lubridate)
library(RMariaDB)
library(shinyjs)

#source("alter_database.R")
# Recall, Precision, Threshold
cutoffs <- readRDS('./cutoff_list2021-09-10.rds')

mod <- readRDS('xgb_model2021-09-10.rds')
dat <- readRDS('./hts_data_imputed2021-09-10.rds')
dat <- dat$sparse$train
dat <- dat %>%
  select(-Facility.Name, -Longitude, -Latitude, -Sitecode)
factor_vars <- names(dat)[ sapply(dat, is.factor) ]
factor_vars <- factor_vars[factor_vars!="FinalTestResult"]
dat_unique <- apply(dat[, factor_vars], 2, function(x)unique(x[!is.na(x)]))

facilities <- readRDS('./facilities2021-09-10.rds') %>%
  filter(Facility.Name %in% c ('Mbita Sub-County Hospital','Rangwe Sub-District Hospital','Ogongo Sub-County Hospital','Suba Sub-Couty Hospital'
         ,'Nyandiwa Level IV Hospital','Ndhiwa Sub-District Hospital','Marindi Sub County Referral Hospital','Makongeni Health Centre'
           ,'Homa Bay County Teaching and Referral Hospital','Kendu Sub-District Hospital','Kandiege Sub-District Hospital'
           ,'Kabondo Sub-County  Hospital','Rachuonyo District Hospital','Ogongo Sub-County Hospital','Kabondo Sub-County  Hospital','Kendu Sub-District Hospital')) %>%
  select(-Latitude, -Longitude)



encodeXGBoost <- function(dataset){

  # Need to one-hot encode all the factor variables
  ohe_features <- names(dataset)[ sapply(dataset, is.factor) ]
  
  dmy <- dummyVars("~ Gender +  KeyPopulationType + MaritalStatus + PatientDisabled + EverTestedForHIV +
                   ClientTestedAs + EntryPoint + TestingStrategy + TBScreening + ClientSelfTested +
                   month_of_test + dayofweek",
                   data = dataset)
  ohe <- data.frame(predict(dmy, newdata = dataset))
  dataset <- cbind(dataset, ohe)
  
  dataset[, !(names(dataset) %in% ohe_features)]
  
}
