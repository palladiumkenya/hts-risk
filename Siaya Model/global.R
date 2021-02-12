library(shiny)
library(randomForest)
library(dplyr)
library(DBI)
library(RMariaDB)
library(shinythemes)
library(lubridate)

source("create_database.R")

# Recall, Precision, Threshold
THRESH_75 <- c('28.6%', '75%', .362)
THRESH_50 <- c('46%', '50%', .192)
THRESH_25 <- c('61%', '25%', .08)

mod <- readRDS('rf_siaya_20201026.rds')
dat <- readRDS('training_siaya_20201026.rds')
# Read in facility index
facilities <- read.csv('./Facility Export Material List.csv', stringsAsFactors = FALSE) %>%
  select(1:2) %>%
  rbind(., data.frame(Code = "99999", Name = "Other")) %>%
  filter(Code %in% unique(dat$Sitecode))

# Read in Population Type
population <- read.csv('./KeyPopulation.csv', stringsAsFactors = FALSE) %>%
  select(1:2) %>%
  filter(PopCode %in% unique(dat$KeyPopulationType))

# Read in Enrty Point
entrypoint <- read.csv('./EntryPoint.csv', stringsAsFactors = FALSE) %>%
  select(1:2) %>%
  filter(ID %in% unique(dat$EntryPoint))

# Read in Testing Strategy
testingstrategy <- read.csv('./Testing Strategy.csv', stringsAsFactors = FALSE) %>%
  select(1:2) %>%
  filter(ID %in% unique(dat$TestingStrategy))