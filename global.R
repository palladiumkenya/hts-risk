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
cutoffs <- readRDS('./cutoffs_2021-07-14.rds')

mod <- readRDS('homabay_model_20210713.rds')
dat <- readRDS('./hts_homabay_imputed_0525.rds')
dat <- dat$sparse$train
dat <- dat %>%
  select(-Facility.Name, -Longitude, -Latitude, -Sitecode)
factor_vars <- names(dat)[ sapply(dat, is.factor) ]
factor_vars <- factor_vars[factor_vars!="FinalTestResult"]
dat_unique <- apply(dat[, factor_vars], 2, function(x)unique(x[!is.na(x)]))

facilities <- readRDS('./facilities_pca_20210713.rds') %>%
  filter(Facility.Name =='Mbita Sub-County Hospital'| Facility.Name =='Rangwe Sub-District Hospital'| Facility.Name =='Ogongo Sub-County Hospital'| Facility.Name =='Suba Sub-Couty Hospital'|
         Facility.Name =='Nyandiwa Level IV Hospital'| Facility.Name =='Ndhiwa Sub-District Hospital'| Facility.Name =='Marindi Sub County Referral Hospital'| Facility.Name =='Makongeni Health Centre'|
           Facility.Name =='Homa Bay County Teaching and Referral Hospital'| Facility.Name =='Kendu Sub-District Hospital'| Facility.Name =='Kandiege Sub-District Hospital'| 
           Facility.Name =='Kabondo Sub-County  Hospital'| Facility.Name =='Rachuonyo District Hospital') %>%
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
