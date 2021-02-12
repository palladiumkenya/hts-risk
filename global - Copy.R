library(shiny)
library(randomForest)
library(dplyr)
library(RSQLite)
library(shinythemes)
library(lubridate)
library(RMariaDB)

# Recall, Precision, Threshold
THRESH_75 <- list('22.8%', '75%', .388)
THRESH_50 <- list('45.5%', '50%', .138)
THRESH_25 <- list('68.7%', '25%', .024)

mod <- readRDS('rf_siaya_20210209.rds')
dat <- readRDS('training_siaya_20210209.rds')
# sc_info <- dat %>% select(2:75) %>% unique() %>% filter(Sitecode != '99999')
# saveRDS(sc_info, './sc_info.rds')
sc_info <- readRDS('./sc_info.rds')
# Read in facility index
facilities <- read.csv('./Facility Export Material List.csv', stringsAsFactors = FALSE) %>%
  select(1:2) %>%
  # rbind(., data.frame(Code = "99999", Name = "Other")) %>%
  filter(Code %in% unique(dat$Sitecode))
