R_DEFAULT_SAVE_VERSION = 2
R_DEFAULT_SERIALIZE_VERSION = 2
library(caret)
library(dplyr)
library(ROCR)
library(PRROC)
library(randomForest)
library(xgboost)
library(e1071)
library(lime)
library(inTrees)

setwd("~/Kenya/NDHW Data/TestData")
hts <- readRDS('./hts_0311.rds')

# # Read in facility index
# facilities <- read.csv('../Facility Export Material List.csv', stringsAsFactors = FALSE) %>%
#   select(1, 5, 6, 9, 18:20)
# 
# hts <- merge(hts, facilities, by.x = 'Sitecode', by.y = 'Code')


## Some final data prep
# Filter to Nairobi and drop temporal variables
hts <- hts %>% filter(County == 'SIAYA') %>% select(-County) %>%
  # filter(Sitecode != '17411') %>%
  select(-c(month_of_test, dayofweek, CoupleDiscordant))

# # If any Facility Type has <1% of observations, group it in SiteCode 99999
# ft_other <- names(which(prop.table(table(hts$Facility.type)) < .01))
# hts$Facility.type <- ifelse(hts$Facility.type %in% ft_other, 'Other', hts$Facility.type)
# 
# hts$Keph.level <- ifelse(hts$Keph.level %in% c("Level 4", "Level 5", "Level 6"), "Level 4 and Up", hts$Keph.level)
# 
# hts$Keph.level <- factor(hts$Keph.level)
# hts$Facility.type <- factor(hts$Facility.type)
# hts$Owner.type <- factor(hts$Owner.type)
# hts$Open_whole_day <- factor(hts$Open_whole_day)
# hts$Open_public_holidays <- factor(hts$Open_public_holidays)
# hts$Open_weekends <- factor(hts$Open_weekends)

# Key Population Type - group into
hts <- hts %>% mutate(KeyPopulationType = ifelse(KeyPopulationType == 'GP', 'GP', 'Other'))

# If any SiteCode has <1% of observations, group it in SiteCode 99999
sc_other <- names(which(prop.table(table(hts$Sitecode, useNA='always')) < .01))
hts$Sitecode <- ifelse(hts$Sitecode %in% sc_other, '99999', hts$Sitecode)

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

hts$FinalTestResult <- factor(hts$FinalTestResult, levels = c(0, 1))
hts$KeyPopulationType <- factor(hts$KeyPopulationType, levels =  c("GP", "Other"))
hts$MaritalStatus <- factor(hts$MaritalStatus, levels = c("Other", "Single", "Married", "Minor", "Divorced", "Widowed", "Married Polygamous"))
hts$Gender <- factor(hts$Gender, levels = c("Female", "Male"))
hts$PatientDisabled <- factor(hts$PatientDisabled, levels = c("Not Disabled", "Disabled"))
hts$EverTestedForHIV <- factor(hts$EverTestedForHIV, levels = c("Yes", "No"))
hts$ClientTestedAs <- factor(hts$ClientTestedAs, levels = c("Couple", "Individual"))
hts$EntryPoint <- factor(hts$EntryPoint, levels = c("MTC", "VCT", "OPD", "CCC", "Other", "IPD"))
hts$TestingStrategy <- factor(hts$TestingStrategy, levels = c("VCT", "NP", "HP", "MOBILE", "Other", "PNS"))
hts$TBScreening <- factor(hts$TBScreening, levels = c("Screened - No Signs", "Not Done", "Screened - Presumed TB"))
hts$ClientSelfTested <- factor(hts$ClientSelfTested, levels = c("Yes", "No"))
hts$Sitecode <- factor(hts$Sitecode, levels = c("14175", "13471", "99999", "13947", "13588", "14091", "13747", "13476", "16792", "13987",
                                                "13837", "13581", "13496", "13600", "13760", "14085", "14148", "14042", "14080", "16784",
                                                "14164", "13845", "13840", "14013", "13771", "13834", "14156", "14165", "16789", "13651",
                                                "16785", "14072", "13735", "14018", "14063"))
# hts$month_of_test <- factor(hts$month_of_test, levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12'))
# hts$dayofweek <- factor(hts$dayofweek, levels = c('1', '2', '3', '4', '5', '6', '7'))
saveRDS(hts, './training_siaya_20200930.rds')

# based on variable importance, let's only keep: 
# cols_to_keep <- c('FinalTestResult', 'AgeAtTest', 'MaritalStatus', 'Gender', 'MonthSinceLastTest', 'EntryPoint', 'TestingStrategy', 'TBScreening', 'Sitecode', 'ClientTestedAs', 'PatientDisabled', 'KeyPopulationType')
# hts <- hts[, which(names(hts) %in% cols_to_keep)]

set.seed(2231)


#partition and create training, testing data
split <- createDataPartition(y = hts$FinalTestResult,p = 0.8,list = FALSE)

train_all <- hts[split, ] 
test <- hts[-split, ]

split_val <- createDataPartition(y = train_all$FinalTestResult,p = 0.75,list = FALSE)

train <- train_all[split_val, ]
val <- train_all[-split_val, ]

grid <- expand.grid(mtry = 2:6, nodesize = c(10, 5, 1))

for (i in 1:nrow(grid)){
  
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
  fg <- pred_val[val$FinalTestResult == 1, 2]
  bg <- pred_val[val$FinalTestResult == 0, 2]
  prc <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
  grid$val_pr_auc[i] <- prc$auc.integral
  
}

grid %>% arrange(desc(val_pr_auc))

set.seed(2231)
rf <- randomForest(
  as.factor(FinalTestResult) ~ .,
  data = train,
  importance = TRUE,
  mtry = 4,
  nodesize = 5 
  # cutoff = c(.85, .15)
)

saveRDS(rf, './rf_siaya_20200930.rds')

pred_val = predict(rf, newdata=val, type = "prob")

fg <- pred_val[val$FinalTestResult == 1, 2]
bg <- pred_val[val$FinalTestResult == 0, 2]

prc <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(prc)

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


#### Differences by Precision ####
