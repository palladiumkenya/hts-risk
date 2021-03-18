library(shiny)
library(randomForest)
library(dplyr)
library(RSQLite)
library(shinythemes)
library(lubridate)
library(RMariaDB)
library(shinyjs)

# Recall, Precision, Threshold
THRESH_75 <- list('21.5%', '75%', .474)
THRESH_50 <- list('44.2%', '50%', .242)
THRESH_25 <- list('68.8%', '25%', .082)

mod <- readRDS('rf_homabay_20210315.rds')
dat <- readRDS('training_homabay_20210315.rds') 
facilities <- readRDS('./facilities_pca_all.rds') %>%
  select(-Latitude, -Longitude)


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

