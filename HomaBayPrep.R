# R_DEFAULT_SAVE_VERSION = 2
# R_DEFAULT_SERIALIZE_VERSION = 2
library(caret)
library(dplyr)
library(ROCR)
library(PRROC)
library(randomForest)
library(lubridate)

# setwd("~/Kenya/Data")
# hts <- read.csv('./HTS FEB 20212.csv', stringsAsFactors = FALSE)
# # 1,716,253
# # Filter to Positive and Negative (what about inconclusive?)
# hts <- hts %>%
#   filter(FinalTestResult %in% c("Positive", "Negative")) %>%
#   mutate(FinalTestResult = ifelse(FinalTestResult == 'Positive', 'Positive', 'Negative'))
# #1,510,831
# # Per Kenya team's guidance, let's limit to initial tests.
# hts <- hts %>%
#   filter(TestType %in% c('Initial', 'Initial Test'))
# #1,366,649
# # Remove observations with unlikely DOB
# hts$DOB <- ymd(hts$DOB)
# hts <- hts[hts$DOB > '1920-01-01', ]
# #1,366,469
# # convert to date
# hts$TestDate <- ymd(hts$TestDate)
# # Drop test dates that are erroneously in the future
# hts <- hts[hts$TestDate < '2021-02-08', ]
# #1,366,464
# # drop tests prior to 2019 for data quality reasons
# hts <- hts[hts$TestDate >= '2017-01-01', ]
# #1,365,265
# # limit to observations where DOB is before or same as test date
# hts <- hts[hts$DOB <= hts$TestDate, ]
# #1,365,075
# # Calculate age at time of test date in term sof years
# hts$AgeAtTest <- floor((hts$TestDate - hts$DOB)/365)
# 
# hts$month_of_test <- month(hts$TestDate)
# hts$dayofweek <- wday(hts$TestDate)
# 
# hts$KeyPopulationType <- tolower(hts$KeyPopType)
# hts <- hts %>%
#   mutate(KeyPopulationType = ifelse(grepl("msm", KeyPopulationType) | grepl("men", KeyPopulationType), "MSM",
#                                     ifelse(grepl("sw", KeyPopulationType) | grepl("sex", KeyPopulationType), "SW",
#                                            ifelse(grepl("pwid", KeyPopulationType) | grepl("drugs", KeyPopulationType), "PWID",
#                                                   ifelse(KeyPopulationType %in% c("", " ", "n/a"), "GP", "OtherKP")))))
# 
# hts$MaritalStatus <- tolower(hts$MaritalStatus)
# hts <- hts %>%
#   mutate(MaritalStatus = ifelse(grepl("married", MaritalStatus) | grepl("cohabit", MaritalStatus) | grepl("partner", MaritalStatus), "Married",
#                                 ifelse((grepl("single", MaritalStatus) | grepl("never", MaritalStatus)) & AgeAtTest >= 15, "Single",
#                                        ifelse((grepl("single", MaritalStatus) | grepl("never", MaritalStatus)) & AgeAtTest < 15, "Minor",
#                                               ifelse(grepl("divorced", MaritalStatus) | grepl("separated", MaritalStatus), "Divorced",
#                                                      ifelse(grepl("widow", MaritalStatus), "Widowed",
#                                                             ifelse(grepl("poly", MaritalStatus), "Polygamous", "Unknown")))))))
# 
# hts <- hts %>%
#   mutate(PatientDisabled = ifelse(PatientDisabled %in% c('<NA>', '', 'No', 'C: Couple (includes polygamous)', 'I: Individual'), 'Not Disabled', 'Disabled'))
# 
# hts <- hts %>%
#   filter(EverTestedForHIV %in% c('No', 'Yes'))
# 
# hts$MonthsSinceLastTest <- as.numeric(hts$MonthsSinceLastTest)
# 
# hts <- hts %>% filter((MonthsSinceLastTest>=0 & MonthsSinceLastTest <= 120) | is.na(MonthsSinceLastTest))
# 
# # Set months to zero for never tested
# hts <- hts %>%
#   mutate(MonthsSinceLastTest = ifelse(EverTestedForHIV == 'No', 0, MonthsSinceLastTest))
# 
# # Set months to average for tested with missing months value
# MSLT_AVG <- round(mean(filter(hts, EverTestedForHIV == 'Yes')$MonthsSinceLastTest, na.rm = T))
# hts <- hts %>%
#   mutate(MonthsSinceLastTest = ifelse(EverTestedForHIV == 'Yes' & is.na(MonthsSinceLastTest), MSLT_AVG, MonthsSinceLastTest))
# 
# hts$ClientTestedAs <- tolower(hts$ClientTestedAs)
# # Group into couple, individual, other
# hts <- hts %>%
#   mutate(ClientTestedAs = ifelse(grepl('couple', ClientTestedAs), 'Couple', 'Individual'))
# 
# # # EntryPoint
# # hts$EntryPoint <- tolower(hts$EntryPoint)
# # CCC <- c('CCC', 'Comprehensive Care Clinic')
# # MTC <- c('PMTCT', 'PNC', 'MCH', 'Maternity', 'ANC', 'Mother Child Health', 'CWC/HEI')
# # HB <- c('Home Based Testing and Care', 'Home Based Testing', 'HBTC')
# # IPD <- c('Inpatient Adult', 'IPD', 'IPD-Adult', 'IPD-Child', 'In Patient Department(IPD)')
# # OPD <- c('Out Patient Department(OPD)', 'Outpatient Department', 'OPD', 'PITC')
# # MOBILE <- c('Mobile Outreach', 'Outreach')
# # PEDS <- c('Peadiatric Clinic', 'PeD')
# # TB <- c('TB Clinic', 'TB')
# # VCT <- c('Volunatry Counselling Centre', 'VCT WALK IN', 'VCT')
# # VMMC <- c('VMMC')
# 
# hts <- hts %>%
#   mutate(EntryPoint = ifelse(grepl('ccc|comprehensive', EntryPoint), 'CCC',
#                              ifelse(grepl('pmtct|pnc|mch|maternity|anc|mother|cwc', EntryPoint), 'MTC',
#                                     ifelse(grepl('home|hbtc', EntryPoint), "HB",
#                                            ifelse(grepl('inpatient|ipd', EntryPoint), "IPD",
#                                                   ifelse(grepl('outpatient|opd|pitc', EntryPoint), "OPD",
#                                                          ifelse(grepl('mobile|outreach', EntryPoint), "MOBILE",
#                                                                 ifelse(grepl('ped|peadiatric', EntryPoint), "PEDS",
#                                                                        ifelse(grepl('tb', EntryPoint), "TB",
#                                                                               ifelse(grepl('vct|voluntary', EntryPoint), "VCT",
#                                                                                      ifelse(grepl('vmmc', EntryPoint), "VMMC", "Other")))))))))))
# 
# # hts <- hts %>%
# #   mutate(EntryPoint = ifelse(EntryPoint %in% CCC, 'CCC',
# #                              ifelse(EntryPoint %in% MTC, 'MTC',
# #                                     ifelse(EntryPoint %in% HB, 'HB',
# #                                            ifelse(EntryPoint %in% IPD, 'IPD',
# #                                                   ifelse(EntryPoint %in% OPD, 'OPD',
# #                                                          ifelse(EntryPoint %in% MOBILE, 'MOBILE',
# #                                                                 ifelse(EntryPoint %in% PEDS, 'PEDS',
# #                                                                        ifelse(EntryPoint %in% TB, 'TB',
# #                                                                               ifelse(EntryPoint %in% VCT, 'VCT',
# #                                                                                      ifelse(EntryPoint %in% VMMC, 'VMMC', 'Other')))))))))))
# 
# # HB <- c('HB: Home-based', 'HB', 'Home Based Testing', 'HBTC')
# # HP <- c('IPD-Child', 'HP/PITC', 'HP: Health Facility Patients', 'PITC', 'HP', 'IPD-Adult', 'OPD', 'MCH', 'Maternity')
# # VCT <- c('VCT', 'Integrated VCT Center', 'VI: Integrated VCT sites', 'Stand Alone VCT Center', 'VS: Stand-alone VCT sites', 'VI', 'VS', 'Non Provider Initiated Testing')
# # NP <- c('NP', 'NP: Non-Patients')
# # MOBILE <- c('Mobile Outreach HTS', 'MO: Mobile and Outreach')
# # PITC <- c('PITC')
# # PNS <- c('PNS')
# 
# hts$TestingStrategy <- tolower(hts$TestingStrategy)
# hts <- hts %>%
#   mutate(TestingStrategy = ifelse(grepl('hb|home based', TestingStrategy), "HB",
#                                   ifelse(grepl('ipd|hp|opd|mch|maternity', TestingStrategy), "HP",
#                                          ifelse(grepl('vct|vi|vs|non provider', TestingStrategy), "VCT",
#                                                 ifelse(grepl('np', TestingStrategy), "NP",
#                                                        ifelse(grepl('mobile', TestingStrategy), "MOBILE",
#                                                               ifelse(grepl('pitc', TestingStrategy), "PITC",
#                                                                      ifelse(grepl('pns', TestingStrategy), "PNS", "Other"))))))))
# 
# 
# # hts <- hts %>%
# #   mutate(TestingStrategy = ifelse(TestingStrategy %in% HB, 'HB',
# #                                   ifelse(TestingStrategy %in% HP, 'HP',
# #                                          ifelse(TestingStrategy %in% VCT, 'VCT',
# #                                                 ifelse(TestingStrategy %in% NP, 'NP',
# #                                                        ifelse(TestingStrategy %in% MOBILE, 'MOBILE',
# #                                                               ifelse(TestingStrategy %in% PITC, 'PITC',
# #                                                                      ifelse(TestingStrategy %in% PNS, 'PNS', 'Other'))))))))
# 
# hts$TBScreening <- tolower(hts$TBScreening)
# table(hts$TBScreening, useNA = 'always')
# 
# # group as not done, no signs, and done
# hts <- hts %>%
#   filter(TBScreening != 'not done') %>%
#   mutate(TBScreening = ifelse(grepl('yes|tb rx|tbrx|tb confirmed|prtb|presumed tb|on tb treatment', TBScreening), "Presumed TB", "No Presumed TB"))
# # hts <- hts %>%
# #   mutate(TBScreening = ifelse(TBScreening %in% c('tbrx', 'prtb', 'presumed tb', 'on tb treatment', 'tb rx', 'tb confirmed'), "Screened - Presumed TB",
# #                               ifelse(TBScreening == 'not done', 'Not Done', 'Screened - No Signs')))
# 
# # if not asked, set to No, which is dominant class
# hts <- hts %>%
#   mutate(ClientSelfTested = ifelse(ClientSelfTested %in% c('1', 'Yes'), 'Yes', 'No'))
# 
# hts <- hts %>%
#   mutate(FacilityType = ifelse(grepl('Hospital', FacilityName), "Hospital",
#                                ifelse(grepl("Dispensary", FacilityName), "Dispensary", "Health Center")))
# 
# hts_out <- hts %>%
#   select(FinalTestResult, AgeAtTest, KeyPopulationType, MaritalStatus, Gender, PatientDisabled, EverTestedForHIV,
#          MonthsSinceLastTest, ClientTestedAs, EntryPoint, TestingStrategy, TBScreening, ClientSelfTested, County,
#          Sitecode, County, month_of_test, dayofweek, FacilityName, FacilityType)
# 
# # # saveRDS(hts_out, './htsdata_1025.rds')
# saveRDS(hts_out, './htsdata_0210.rds')

setwd("~/Kenya/NDHW Data/TestData/HTS_App/FHW App/HomaBay")
hts <- readRDS('./htsdata_0210.rds')

## Some final data prep
# Filter to Nairobi and drop temporal variables
hts <- hts %>% filter(County == 'HOMA BAY') %>% select(-County) #%>% select(-c(month_of_test, dayofweek))

# Read in Local Demographic and HIV data
facilities <- readRDS('../../../../../facilities_homabay_5km.rds') %>%
  filter(!is.na(hiv_prev)) 

hts <- merge(facilities, hts, by.x = "Facility.Name", by.y = "FacilityName")

# # Read in facility index
# facilities <- read.csv('./Facility Export Material List.csv', stringsAsFactors = FALSE) %>%
#   select(1,5:6, 9, 18:20)
# 
# hts <- merge(hts, facilities, by.x = 'Sitecode', by.y = 'Code')
# 
# hts$Keph.level <- ifelse(hts$Keph.level %in% c("Level 4", "Level 5", "Level 6"), "Level 4 and Up", hts$Keph.level)
# 
# hts$Keph.level <- factor(hts$Keph.level, levels = c("Level 2", "Level 3", "Level 4 and Up"))
# hts$Facility.type <- factor(hts$Facility.type, levels = c("Basic Health Centre", "Primary care hospitals", "Dispensary",
#                                                           "Comprehensive health Centre", "Medical Clinic"))
# hts$Owner.type <- factor(hts$Owner.type, levels = c("Ministry of Health", "Private Practice", "Faith Based Organization"))
# hts$Open_whole_day <- factor(hts$Open_whole_day, levels = c("Yes", "No"))
# hts$Open_public_holidays <- factor(hts$Open_public_holidays, levels = c("Yes", "No"))
# hts$Open_weekends <- factor(hts$Open_weekends, levels = c("Yes", "No"))

# Key Population Type - group into
kp_other <- names(which(prop.table(table(hts$KeyPopulationType, useNA='always')) < .01))
hts$KeyPopulationType <- ifelse(hts$KeyPopulationType %in% kp_other, 'OtherKP', hts$KeyPopulationType)

# If any SiteCode has <.1% of observations, drop it
# sc_other <- names(which(prop.table(table(hts$Sitecode, useNA='always')) < .005))
# # hts <- hts %>% filter(!(Sitecode %in% sc_other))
# hts$Sitecode <- ifelse(hts$Sitecode %in% sc_other, '99999', hts$Sitecode)

## Updating this to .5% of observations, per KeHMIS II request - as no category has less than .5%, skipping this
# If any marital status has <1% of observations, group it in Other
# ms_other <- names(which(prop.table(table(hts$MaritalStatus)) < .005))
# hts$MaritalStatus <- ifelse(hts$MaritalStatus %in% ms_other, 'Other', hts$MaritalStatus)

# If any entry point has <1% of observations, group it in Other
ep_other <- names(which(prop.table(table(hts$EntryPoint, useNA = 'always')) < .01))
hts$EntryPoint <- ifelse(hts$EntryPoint %in% ep_other, 'Other', hts$EntryPoint)

# If any testing strategy has <1% of observations, group it in Other
ts_other <- names(which(prop.table(table(hts$TestingStrategy, useNA = 'always')) < .01))
hts$TestingStrategy <- ifelse(hts$TestingStrategy %in% ts_other, 'Other', hts$TestingStrategy)

hts$AgeAtTest <- as.numeric(hts$AgeAtTest)
hts$FinalTestResult <- factor(hts$FinalTestResult, levels = c("Positive", "Negative"))
hts$KeyPopulationType <- factor(hts$KeyPopulationType, levels = c("GP", "OtherKP", "SW"))
hts$MaritalStatus <- factor(hts$MaritalStatus, levels = c("Unknown","Married","Polygamous", 
                                                          "Widowed", "Divorced"))
hts$Gender <- factor(hts$Gender, levels = c("Female", "Male"))
hts$PatientDisabled <- factor(hts$PatientDisabled, levels = c("Not Disabled", "Disabled"))
hts$EverTestedForHIV <- factor(hts$EverTestedForHIV, levels = c("Yes", "No"))
hts$ClientTestedAs <- factor(hts$ClientTestedAs, levels = c("Individual", "Couple"))
hts$EntryPoint <- factor(hts$EntryPoint, levels = c("MOBILE", "OPD", "VCT", "Other",
                                                    "HB", "PEDS", "MTC", "IPD"))
hts$TestingStrategy <- factor(hts$TestingStrategy, levels = c("MOBILE", "VCT", "HB", "Other"))
hts$TBScreening <- factor(hts$TBScreening, levels = c("No Presumed TB", "Presumed TB"))
hts$ClientSelfTested <- factor(hts$ClientSelfTested, levels = c("Yes", "No"))
hts$FacilityType <- factor(hts$FacilityType, levels = c("Dispensary", "Hospital", "Health Center"))
# hts$Sitecode <- factor(hts$Sitecode)
hts$month_of_test <- factor(hts$month_of_test, levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12'))
hts$dayofweek <- factor(hts$dayofweek, levels = c('1', '2', '3', '4', '5', '6', '7'))
# saveRDS(hts, './training_homabay_20201026.rds')

hts <- hts %>% select(-c(Facility.Name, Longitude, Latitude, nearest_in_set2))
# library(Boruta)
# set.seed(2231)
# boruta_output <- Boruta(FinalTestResult ~ ., doTrace = 2, data=hts, maxRuns = 100)
# boruta_signif <- getSelectedAttributes(boruta_output)
boruta_signif <- c("births", "pregnancies", "literacy", "poverty", "anc", "pnc", "sba", "hiv_prev",
                   "hiv_count", "condom", "intercourse", "in_union", "circumcision", "partner_away",
                   "partner_men", "partner_women", "sti", "fb", "pop", "popm45", "popm40", "popm35",
                   "popm30", "popm25", "popm20", "popm15", "popf45", "popf40", "popf35", "popf30",
                   "popf25", "popf20", "popf15", "bed_prv_pu", "bed_pub_pu", "edu_prv_pu", "edu_pub_pu",
                   "emp_agr_pu", "emp_ind_pu", "emp_ser_pu", "ic_low_pu", "ic_mlw_pu", "tot_pu", "bed_prv_cu",
                   "bed_pub_cu", "edu_prv_cu", "edu_pub_cu", "emp_ind_cu", "emp_ser_cu", "ic_low_cu",
                   "ic_mlw_cu", "tot_cu", "bed_prv_pr", "bed_pub_pr", "edu_prv_pr", "edu_pub_pr", "emp_agr_pr",
                   "emp_ind_pr", "emp_ser_pr", "ic_low_pr", "ic_mlw_pr", "tot_pr", "bed_prv_cr", "bed_pub_cr",
                   "edu_prv_cr", "edu_pub_cr", "emp_ind_cr", "emp_ser_cr", "ic_low_cr", "ic_mlw_cr", "tot_cr",
                   "tot_pob", "tot_val","FacilityType", "AgeAtTest", "MaritalStatus", "Gender", "EverTestedForHIV",
                   "MonthsSinceLastTest", "ClientTestedAs", "TestingStrategy", "ClientSelfTested",
                   "TBScreening", "month_of_test", "dayofweek", "KeyPopulationType")

# plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")
hts_out <- hts[, c("FinalTestResult", "Sitecode", boruta_signif)]
# saveRDS(hts_out, './training_homabay_20210210.rds')
hts <- hts_out %>% select(-Sitecode)

set.seed(2231)

#partition and create training, testing data
split <- createDataPartition(y = hts$FinalTestResult,p = 0.8,list = FALSE)

train_all <- hts[split, ] 
test <- hts[-split, ]

split_val <- createDataPartition(y = train_all$FinalTestResult,p = 0.75,list = FALSE)

train <- train_all[split_val, ]
val <- train_all[-split_val, ]

grid <- expand.grid(mtry = c(7, 9, 11, 13, 15, 17, 19, 21, 23, 25), nodesize = c(5, 1))

for (i in 1:nrow(grid)){
  print(i)
  set.seed(2231)
  
  rf <- randomForest(
    as.factor(FinalTestResult) ~ .,
    data = train,
    mtry = grid[i, 1],
    nodesize = grid[i, 2],
    # sampsize = c(grid[i, 1], grid[i, 2]),
    importance = TRUE
  )
  
  pred_val = predict(rf, newdata=val, type = "prob")
  fg <- pred_val[val$FinalTestResult == "Positive", 1]
  bg <- pred_val[val$FinalTestResult == "Negative", 1]
  prc <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
  grid$val_pr_auc[i] <- prc$auc.integral
  
}

grid %>% arrange(desc(val_pr_auc))

set.seed(2231)
rf <- randomForest(
  as.factor(FinalTestResult) ~ .,
  data = train,
  importance = TRUE,
  mtry = 23,
  nodesize = 1 
  # cutoff = c(.85, .15)
)

rf$importance

saveRDS(rf, './rf_homabay_20210210.rds')
# rf <- readRDS('./rf_siaya_20210209.rds')

pred_val = predict(rf, newdata=val, type = "prob")

fg <- pred_val[val$FinalTestResult == "Positive", 1]
bg <- pred_val[val$FinalTestResult == "Negative", 1]

prc <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(prc)

# .407 with no time variables and sitecodes aggregated below .1%
# .389 with no time variables and sitecodes aggregated below 1%
# .425 with time variables and sitecodes aggregated below 1%
# .476  with time variables and sitecodes aggreagated below .1%

# Find cutoffs
pr_df <- data.frame(prc$curve)
names(pr_df) <- c("Recall", "Precision", "Threshold")

# Let's set thresholds at precision levels
# 75% of clients at this level or higher tested positive.
# Patient with this risk score of higher account for X% of all positive tests
thresh_75 <- pr_df %>%
  filter(Precision > .75) %>%
  filter(row_number() == 1)

thresh_50 <- pr_df %>%
  filter(Precision > .5) %>%
  filter(row_number() == 1)

thresh_25 <- pr_df %>%
  filter(Precision > .25) %>%
  filter(row_number() == 1)


# #### Differences by Precision ####
# val_pred <- cbind(val, pred_val[, 2]) %>% select(-1)
# names(val_pred)[ncol(val_pred)] <- "ProbPositive"
# u_pr_df <- pr_df %>%
#   group_by(Threshold) %>%
#   summarize(Recall = mean(Recall),
#             Precision = mean(Precision))
# val_pred_pr <- merge(val_pred, u_pr_df, by.x = "ProbPositive", by.y = "Threshold", all.x = TRUE)
# 
# saveRDS(val_pred_pr, './siaya_data_probs.rds')
# 
# ## So now, let's separate into risk quadrants, get the overall proportions by variable,
# # get the proportions the same way for each quadrant, and calculate similarity or difference with overall proportions. 
# high_prob_cases <- val_pred %>%
#   filter(ProbPositive >= thresh_50$Threshold)
# 
# low_prob_cases <- val_pred %>%
#   filter(ProbPositive < thresh_50$Threshold)
