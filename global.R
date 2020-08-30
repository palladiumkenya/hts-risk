library(shiny)
library(randomForest)
library(dplyr)
library(DBI)
library(RMariaDB)
library(shinythemes)

source("create_database.R")

# Recall, Precision, Threshold
THRESH_75 <- c('13%', '75%', .462)
THRESH_50 <- c('32%', '50%', .19)
THRESH_25 <- c('48%', '25%', .05)

# Declare global variables
KPTYPE <- c("GP", "Other")
MS <- c("Other", "Single", "Married", "Minor", "Divorced", "Widowed", "Married Polygamous")
GENDER <- c("Female", "Male")
PD <- c("Not Disabled", "Disabled")
ETHIV <- c("Yes", "No")
CTA <- c("Couple", "Individual")
EP <- c("MTC", "VCT", "OPD", "MOBILE", "Other", "PEDS")
TS <- c("VCT", "NP", "HP", "MOBILE", "Other", "PNS")
TB <- c("Screened - No Signs", "Not Done", "Screened - Presumed TB")
CST <- c("Yes", "No")
CD <- c("Yes", "No") 
SITECODE <- c("13155", "13165", "13126", "13122", "12935", "13028", "12929", "99999", "12974", "13030", "13245", "24698")

mod <- readRDS('rf_nairobi_20200827.rds')
dat <- readRDS('training_nairobi_20200827.rds') %>% select(-1)