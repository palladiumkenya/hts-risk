## This script processes individual HTS records from the National Data Warehouse including data cleaning,
## feature generation, and imputation of missing fields
##
## Author: Yoni Friedman
## Last edited: May 19, 2021


# Load libraries --------------------------
# R_DEFAULT_SAVE_VERSION = 2
# R_DEFAULT_SERIALIZE_VERSION = 2
library(caret)
library(dplyr)
library(lubridate)
library(mice)

# Read in NDWH data ----------------------------
setwd("~/Kenya/NDHW Data/TestData/HTS_App/FHW App/HomaBay")
# 1,716,253 test results
hts <- read.csv('~/Kenya/Data/HTS FEB 20212.csv', stringsAsFactors = FALSE)
# Add 797 test results from Rangwe
hts_rangwe <- read.csv('Rangwe.csv', stringsAsFactors = FALSE)
hts <- rbind(hts, hts_rangwe)

# Filter to Positive and Negative
hts <- hts %>%
  filter(FinalTestResult %in% c("Positive", "Negative")) %>%
  mutate(FinalTestResult = ifelse(FinalTestResult == 'Positive', 'Positive', 'Negative'))
#1,511,628 remaining

# Limiting to initial tests
hts <- hts %>%
  filter(TestType %in% c('Initial', 'Initial Test'))
#1,367,375 remaining

# Remove observations with unlikely DOB
hts$DOB <- ymd(hts$DOB) # convert DOB to date variable
hts <- hts[hts$DOB > '1920-01-01', ] # limit to DOB within past 100 years
#1,367,195 remaining

# Look at test date range
hts$TestDate <- ymd(hts$TestDate) # convert to date variable
# Drop test dates that are erroneously in the future
hts <- hts[hts$TestDate < '2021-03-12', ]
#1,367,190 remaining 

# drop tests prior to 2019 for data quality reasons
hts <- hts[hts$TestDate >= '2019-01-01', ]
#1,152,085 remaining

# limit to observations where DOB is before or same as test date
hts <- hts[hts$DOB <= hts$TestDate, ]
#1,151,956 remaining

# Calculate age at time of test date in terms of years
hts$AgeAtTest <- floor((hts$TestDate - hts$DOB)/365)
hts$AgeAtTest <- as.numeric(hts$AgeAtTest)

# Get the month and day of week of the test
hts$month_of_test <- month(hts$TestDate)
hts$dayofweek <- wday(hts$TestDate)

# Key Population Type - group as MSM, SW, PWID, GP, and OtherKP (KP type not indicated)
hts$KeyPopulationType <- tolower(hts$KeyPopType)
hts <- hts %>%
  mutate(KeyPopulationType = ifelse(grepl("msm|men", KeyPopulationType), "MSM",
                                    ifelse(grepl("sw|sex", KeyPopulationType), "SW",
                                           ifelse(grepl("pwid|drugs", KeyPopulationType), "PWID",
                                                  ifelse(KeyPopulationType %in% c("", " ", "n/a") | is.na(KeyPopulationType), "GP", "OtherKP")))))
table(hts$KeyPopulationType) # Mostly GP

# Marital Status - group as married, single, minor, divorced, widowed, polygamous, unknown
hts$MaritalStatus <- tolower(hts$MaritalStatus)
hts <- hts %>%
  mutate(MaritalStatus = ifelse(grepl("married|cohabit|partner", MaritalStatus), "Married",
                                ifelse(grepl("single|never", MaritalStatus) & AgeAtTest >= 15, "Single",
                                       ifelse((grepl("single|never|unknown", MaritalStatus) | MaritalStatus == "") & AgeAtTest < 15, "Minor",
                                              ifelse(grepl("divorced|separated", MaritalStatus), "Divorced",
                                                     ifelse(grepl("widow", MaritalStatus), "Widowed",
                                                            ifelse(grepl("poly", MaritalStatus), "Polygamous", NA)))))))
table(hts$MaritalStatus)

# Patient disabled
hts <- hts %>%
  mutate(PatientDisabled = ifelse(PatientDisabled %in% c('<NA>', '', 'No', 'C: Couple (includes polygamous)', 'I: Individual'), 'Not Disabled', 'Disabled'))

# Filter for clients who reported whether they ever tested before for HIV (7,467 are dropped)
hts <- hts %>%
  filter(EverTestedForHIV %in% c('No', 'Yes'))

# For patients who have been tested for HIV, how long has it been since their last test?
hts$MonthsSinceLastTest <- as.numeric(hts$MonthsSinceLastTest)
# Keep only records where range is within past five years or NA (for not tested previously) 
hts <- hts %>% filter((MonthsSinceLastTest>=0 & MonthsSinceLastTest <= 120) | is.na(MonthsSinceLastTest))
# only a few drop

# Set months to zero for never tested
hts <- hts %>%
  mutate(MonthsSinceLastTest = ifelse(EverTestedForHIV == 'No', 0, MonthsSinceLastTest))
# For the remaining NAs, we'll impute

# Client Type
hts$ClientTestedAs <- tolower(hts$ClientTestedAs)
# Group into couple, individual
hts <- hts %>%
  mutate(ClientTestedAs = ifelse(grepl('couple', ClientTestedAs), 'Couple', 'Individual'))

# Group entry points
hts$EntryPoint <- tolower(hts$EntryPoint)
hts <- hts %>%
  mutate(EntryPoint = ifelse(grepl('ccc|comprehensive', EntryPoint), 'CCC',
                             ifelse(grepl('pmtct|pnc|mch|maternity|anc|mother|cwc', EntryPoint), 'MTC',
                                    ifelse(grepl('home|hbtc', EntryPoint), "HB",
                                           ifelse(grepl('inpatient|ipd', EntryPoint), "IPD",
                                                  ifelse(grepl('outpatient|opd|pitc', EntryPoint), "OPD",
                                                         ifelse(grepl('mobile|outreach', EntryPoint), "MOBILE",
                                                                ifelse(grepl('ped|peadiatric', EntryPoint), "PEDS",
                                                                       ifelse(grepl('tb', EntryPoint), "TB",
                                                                              ifelse(grepl('vct|voluntary', EntryPoint), "VCT",
                                                                                     ifelse(grepl('vmmc', EntryPoint), "VMMC", NA)))))))))))

# Group testing strategy
hts$TestingStrategy <- tolower(hts$TestingStrategy)
hts <- hts %>%
  mutate(TestingStrategy = ifelse(grepl('hb|home based', TestingStrategy), "HB",
                                  ifelse(grepl('ipd|hp|opd|mch|maternity', TestingStrategy), "HP",
                                         ifelse(grepl('vct|vi|vs|non provider', TestingStrategy), "VCT",
                                                ifelse(grepl('np', TestingStrategy), "NP",
                                                       ifelse(grepl('mobile', TestingStrategy), "MOBILE",
                                                              ifelse(grepl('pitc', TestingStrategy), "PITC",
                                                                     ifelse(grepl('pns', TestingStrategy), "PNS", NA))))))))


# Group TB Screening
hts$TBScreening <- tolower(hts$TBScreening)
table(hts$TBScreening, useNA = 'always')

# group as presumed TB, no TB, and NA - to impute
hts <- hts %>%
  filter(TBScreening != 'not done') %>%
  mutate(TBScreening = ifelse(grepl('tb rx|tbrx|tb confirmed|prtb|presumed tb|on tb treatment', TBScreening), "Presumed TB",
                              ifelse(grepl('no tb|no signs', TBScreening), "No Presumed TB", NA)))

# if not asked, keep as NA and impute
hts <- hts %>%
  mutate(ClientSelfTested = ifelse(ClientSelfTested %in% c('1', 'Yes'), 'Yes',
                                   ifelse(ClientSelfTested %in% c('0', 'No'), 'No', NA)))


# Take selected variables
hts_out <- hts %>%
  select(FinalTestResult, AgeAtTest, KeyPopulationType, MaritalStatus, Gender, PatientDisabled, EverTestedForHIV,
         MonthsSinceLastTest, ClientTestedAs, EntryPoint, TestingStrategy, TBScreening, ClientSelfTested, County,
         Sitecode, month_of_test, dayofweek, FacilityName)


# Save out -------------------------
saveRDS(hts_out, './htsdata_0519.rds')

