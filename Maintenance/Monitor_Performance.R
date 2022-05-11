setwd("~/Kenya/Deployment")
library(xgboost); library(caret); library(dplyr); library(Matrix); library(r2pmml); library(PRROC)


library(raster)
library(sp)
library(rgdal)
library(dplyr)
library(rgeos)
library(ggmap)
library(caret)
library(lubridate)
library(mice)
library(PRROC)
library(xgboost)

setwd("~/Kenya/Deployment")
# source('~/Kenya/NDHW Data/TestData/HTS_App/FHW App/HomaBay/mice_reuse.R')

# hts <- read.csv('~/Kenya/Data/HTSDataset10012022.csv', stringsAsFactors = FALSE)
hts <- read.csv('~/Kenya/Deployment/HTSJanToMarch2022.csv', stringsAsFactors = FALSE)

# Read in NDWH data ----------------------------
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
hts$DOB <- mdy(hts$DOB) # convert DOB to date variable
hts <- hts[hts$DOB > '1920-01-01', ] # limit to DOB within past 100 years
#1,367,195 remaining

# Look at test date range
hts$TestDate <- mdy(hts$TestDate) # convert to date variable
# Drop test dates that are erroneously in the future
hts <- hts[hts$TestDate < Sys.Date(), ]
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

# Filter for 15 and under
# hts <- hts[hts$AgeAtTest > 15, ]

# Get the month and day of week of the test
hts$month_of_test <- month(hts$TestDate)
hts$dayofweek <- wday(hts$TestDate)

# Homa Bay Only
# hts <- hts[hts$County == "HOMA BAY", ]

# Key Population Type - group as MSM, SW, PWID, GP, and OtherKP (KP type not indicated)
hts <- hts %>%
  mutate(KeyPopulationType = ifelse(KeypopulationType == "NULL", "GP", KeypopulationType))

# # Patient disabled
# hts <- hts %>%
#   mutate(PatientDisabled = ifelse(PatientDisabled %in% c('<NA>', '', 'No', 'C: Couple (includes polygamous)', 'I: Individual'), 'Not Disabled', 'Disabled'))

# Filter for clients who reported whether they ever tested before for HIV (7,467 are dropped)
hts <- hts %>%
  filter(EverTestedForHiv %in% c('No', 'Yes'))

# For patients who have been tested for HIV, how long has it been since their last test?
hts$MonthsSinceLastTest <- as.numeric(hts$MonthsSinceLastTest)
# Keep only records where range is within past five years or NA (for not tested previously) 
hts <- hts %>% filter((MonthsSinceLastTest>=0 & MonthsSinceLastTest <= 120) | is.na(MonthsSinceLastTest))
# only a few drop

# drop cases if months since last test is positive but ever tested for hiv is no - very few of these
# htsa <- hts[!(hts$EverTestedForHiv == "No" & hts$MonthsSinceLastTest > 0), ]
# For the remaining NAs, we'll impute

# Group entry points
hts$EntryPoint <- tolower(hts$EntryPoint)
hts <- hts %>%
  mutate(EntryPoint = ifelse(grepl('ccc|comprehensive', EntryPoint), 'CCC',
                             ifelse(grepl('pmtct|pnc|mch|maternity|anc|mother|cwc|family|fp', EntryPoint), 'MTC',
                                    ifelse(grepl('home|hbtc', EntryPoint), "HB",
                                           ifelse(grepl('inpatient|ipd', EntryPoint), "IPD",
                                                  ifelse(grepl('outpatient|opd|pitc', EntryPoint), "OPD",
                                                         ifelse(grepl('mobile|outreach', EntryPoint), "MOBILE",
                                                                ifelse(grepl('ped|peadiatric', EntryPoint), "PEDS",
                                                                       ifelse(grepl('tb', EntryPoint), "TB",
                                                                              ifelse(grepl('vct|voluntary', EntryPoint), "VCT",
                                                                                     ifelse(grepl('vmmc', EntryPoint), "VMMC",
                                                                                            ifelse(grepl('pns', EntryPoint), "PNS", NA))))))))))))


# Group testing strategy
hts$TestingStrategy <- tolower(hts$TestStrategy)
hts <- hts %>%
  mutate(TestingStrategy = ifelse(grepl('hb|home based', TestingStrategy), "HB",
                                  ifelse(grepl('ipd|hp|opd|mch|maternity', TestingStrategy), "HP",
                                         ifelse(grepl('vct|vi|vs|non provider', TestingStrategy), "VCT",
                                                ifelse(grepl('np', TestingStrategy), "NP",
                                                       ifelse(grepl('mobile', TestingStrategy), "MOBILE",
                                                              ifelse(grepl('pitc', TestingStrategy), "PITC",
                                                                     ifelse(grepl('pns', TestingStrategy), "PNS", NA))))))))


# Group TB Screening
hts$TBScreening <- tolower(hts$tbScreening)

# group as presumed TB, no TB, and NA - to impute
hts <- hts %>%
  mutate(TBScreening = ifelse(grepl('tb rx|tbrx|tb confirmed|prtb|presumed tb|on tb treatment', TBScreening), "Presumed TB",
                              ifelse(grepl('no tb|no signs', TBScreening), "No Presumed TB", NA)))

# marital status
table(hts$MaritalStatus)
hts$MaritalStatus <- tolower(hts$MaritalStatus)
hts <- hts %>%
  mutate(MaritalStatus = ifelse(grepl('poly', MaritalStatus), "polygamous",
                                ifelse(grepl('married|cohabit', MaritalStatus), "married",
                                       ifelse(grepl('divorc|separat', MaritalStatus), "divorced",
                                              ifelse(grepl('single', MaritalStatus), "single",
                                                     ifelse(grepl('widow', MaritalStatus), "widowed", NA))))))


# Take selected variables
hts <- hts %>%
  dplyr::select(FinalTestResult, AgeAtTest, KeyPopulationType, Gender, EverTestedForHiv, MaritalStatus,
                MonthsSinceLastTest,  EntryPoint, TestingStrategy, TBScreening, PatientDisabled,
                month_of_test, dayofweek, FacilityName, County, SiteCode, patientpk)

# Homa Bay Only
# hts <- hts[hts$County == "HOMA BAY", ]
# hts <- hts[!is.na(hts$County), ]

# Join with GIS data ----------------------
gis <- readRDS('../NDHW Data/TestData/National/hb_locationdata_0125.rds')

hts <- merge(hts, gis, by.x = "FacilityName", by.y = "Facility.Name")
prop.table(table(hts$FinalTestResult))

hts_main <- readRDS('../NDHW Data/TestData/National/national_outlist_0322_allages.rds')
hts_main <- hts_main$sparse$train

# Convert to factor variables ---------------
hts$Gender <- factor(hts$Gender, levels = levels(hts_main$Gender))
hts$PatientDisabled <- factor(hts$PatientDisabled, levels = levels(hts_main$PatientDisabled))
hts$MaritalStatus <- factor(hts$MaritalStatus, levels = levels(hts_main$MaritalStatus))
hts$KeyPopulationType <- factor(hts$KeyPopulationType, levels = levels(hts_main$KeyPopulationType))
hts$EverTestedForHiv <- factor(hts$EverTestedForHiv, levels = levels(hts_main$EverTestedForHiv))
hts$dayofweek <- factor(hts$dayofweek, levels = levels(hts_main$dayofweek))
hts$month_of_test <- factor(hts$month_of_test, levels = levels(hts_main$month_of_test))
# hts$TBScreening <- gsub(" ", "", hts$TBScreening)
hts$TBScreening <- factor(hts$TBScreening, levels = levels(hts_main$TBScreening))
hts$EntryPoint <- factor(hts$EntryPoint, levels = levels(hts_main$EntryPoint))
hts$TestingStrategy <- factor(hts$TestingStrategy, levels = levels(hts_main$TestingStrategy))
# hts$ClientSelfTested <- factor(hts$ClientSelfTested, levels = unique(na.omit(hts$ClientSelfTested)))
# hts$ClientTestedAs <- factor(hts$ClientTestedAs, levels = unique(na.omit(hts$ClientTestedAs)))
hts$FinalTestResult <- factor(hts$FinalTestResult, levels = levels(hts_main$FinalTestResult))
# hts$County<- factor(hts$County, levels = unique(na.omit(hts$County)))
hts$MonthsSinceLastTest <- as.numeric(hts$MonthsSinceLastTest)
hts$AgeAtTest <- as.numeric(hts$AgeAtTest)

saveRDS(hts, './janmar_0510.rds')


# Encoding function --------------
encodeXGBoost <- function(dataset){
  dataset$FinalTestResult <- if_else(dataset$FinalTestResult == "Positive", 1, 0)
  # Need to one-hot encode all the factor variables
  ohe_features <- names(dataset)[ sapply(dataset, is.factor) ]
  dmy <- dummyVars("~ Gender + KeyPopulationType + MaritalStatus + PatientDisabled + EverTestedForHiv +
                   EntryPoint + TestingStrategy + TBScreening + 
                   month_of_test + dayofweek",
                   data = dataset)
  ohe <- data.frame(predict(dmy, newdata = dataset))
  dataset <- cbind(dataset, ohe)
  
  dataset[, !(names(dataset) %in% ohe_features)]
  
}

hts <- readRDS('janmar_0510.rds')
hts <- hts %>% dplyr::select(-FacilityName, -Longitude, -Latitude, -SiteCode, -County, -patientpk)#-month_of_test, -dayofweek)#, -starts_with("PC"))
hts <- encodeXGBoost(hts) 

xgb <- readRDS("xgb_0509.rds")

names(hts)[55] <- "TBScreeningNoPresumedTB"
names(hts)[56] <- "TBScreeningPresumedTB"

#hts_main has an extra tbscreening and an extra keypopulationtype.other
hts_predict <- predict(xgb,newdata = data.matrix(hts[, which(names(hts) != "FinalTestResult")]))

hts$risk <- "Low"
hts[hts_predict >= 0.002625179 & hts_predict < 0.0106387809, "risk"] <- "Medium"
hts[hts_predict >= 0.0106387809 & hts_predict < 0.0289241020, "risk"] <- "High"
hts[hts_predict >= 0.0289241020, "risk"] <- "Highest"

hts %>%
  group_by(risk) %>%
  summarize(count = n(),
            positives = sum(FinalTestResult == 1),
            negatives = sum(FinalTestResult == 0))

# risk    count positives negatives
# 
# High    40798       427     40371
# Highest 26177      1071     25106
# Low     46042        90     45952
# Medium  85174       437     84737

hts %>%
  filter(AgeAtTest > 15) %>%
  group_by(risk) %>%
  summarize(count = n(),
            positives = sum(FinalTestResult == 1),
            negatives = sum(FinalTestResult == 0))

# risk    count positives negatives
# High    38564       400     38164
# Highest 25137      1042     24095
# Low     42754        82     42672
# Medium  76173       404     75769

hts %>%
  filter(AgeAtTest <= 15) %>%
  group_by(risk) %>%
  summarize(count = n(),
            positives = sum(FinalTestResult == 1),
            negatives = sum(FinalTestResult == 0))

# risk    count positives negatives
# High     2234        27      2207
# Highest  1040        29      1011
# Low      3288         8      3280
# Medium   9001        33      8968

hts_orig <- readRDS('janmar_0510.rds')
hts <- cbind(hts, "County" = hts_orig$County, "PatientPK" = hts_orig$patientpk)

county_dist <- hts %>%
  group_by(`hts_orig$County`, risk) %>%
  summarize(count = n(),
            positives = sum(FinalTestResult == 1),
            negatives = sum(FinalTestResult == 0),
            pos_rate = positives / count)


hts_low_pos <- hts %>%
  filter(risk == "Low" & FinalTestResult == 1)

table(hts_low_pos$`hts_orig$County`)
hist(hts_low_pos$AgeAtTest)
