library(shiny)
library(randomForest)
library(dplyr)
library(RSQLite)
library(shinythemes)
library(lubridate)
library(RMariaDB)

source("create_database.R")

# Recall, Precision, Threshold
THRESH_75 <- list('22.7%', '75%', .446)
THRESH_50 <- list('44.7%', '50%', .232)
THRESH_25 <- list('67.5%', '25%', .068)

mod <- readRDS('rf_homabay_20210210.rds')
dat <- readRDS('training_homabay_20210210.rds')
# sc_info <- dat %>% select(2:76) %>% unique()
# saveRDS(sc_info, './sc_info_homabay.rds')
sc_info <- readRDS('./sc_info_homabay.rds')
# Read in facility index
facilities <- read.csv('./Facility Export Material List.csv', stringsAsFactors = FALSE) %>%
  select(1:2) %>%
  # rbind(., data.frame(Code = "99999", Name = "Other")) %>%
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
