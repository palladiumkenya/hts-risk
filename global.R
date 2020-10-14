library(shiny)
library(randomForest)
library(dplyr)
library(DBI)
library(RMariaDB)
library(shinythemes)

source("create_database.R")

# Recall, Precision, Threshold
THRESH_75 <- c('21%', '75%', .49)
THRESH_50 <- c('36%', '50%', .244)
THRESH_25 <- c('56%', '25%', .082)

# Declare global variables
KPTYPE <- c("GP", "Other")
MS <- c("Other", "Single", "Married", "Minor", "Divorced", "Widowed", "Married Polygamous")
GENDER <- c("Female", "Male")
PD <- c("Not Disabled", "Disabled")
ETHIV <- c("Yes", "No")
CTA <- c("Couple", "Individual")
EP <- c("MTC", "VCT", "OPD", "CCC", "Other", "IPD")
TS <- c("VCT", "NP", "HP", "MOBILE", "Other", "PNS")
TB <- c("Screened - No Signs", "Not Done", "Screened - Presumed TB")
CST <- c("Yes", "No")
SITECODE <- c("14175", "13471", "99999", "13947", "13588", "14091", "13747", "13476", "16792", "13987",
              "13837", "13581", "13496", "13600", "13760", "14085", "14148", "14042", "14080", "16784",
              "14164", "13845", "13840", "14013", "13771", "13834", "14156", "14165", "16789", "13651",
              "16785", "14072", "13735", "14018", "14063")

mod <- readRDS('rf_siaya_20200930.rds')
dat <- readRDS('training_siaya_20200930.rds') %>% select(-1)