library(shiny)
library(randomForest)
library(dplyr)
library(RSQLite)
library(shinythemes)

# Recall, Precision, Threshold
THRESH_75 <- c('12%', '75%', .47)
THRESH_50 <- c('29%', '50%', .21)
THRESH_25 <- c('47%', '25%', .058)

# Declare global variables
KPTYPE <- c("GP", "Other")
MS <- c("Other", "Single", "Married", "Minor", "Divorced")
GENDER <- c("Female", "Male")
PD <- c("Not Disabled", "Disabled")
ETHIV <- c("Yes", "No")
CTA <- c("Couple", "Individual")
EP <- c("MTC", "VCT", "OPD", "MOBILE", "Other", "PEDS")
TS <- c("VCT", "NP", "HP", "MOBILE", "Other", "PNS")
TB <- c("Screened - No Signs", "Not Done", "Screened - Presumed TB")
CST <- c("Yes", "No")
CD <- c("0", "1") 
SITECODE <- c("13155", "13165", "13126", "13122", "12935", "13028", "12929", "99999", "12974", "13030", "13245", "24698")

mod <- readRDS('rf_nairobi.rds')
dat <- readRDS('training_nairobi.rds') %>% select(-1)