setwd("~/Kenya/Deployment")
library(xgboost); library(caret); library(dplyr); library(Matrix); library(r2pmml); library(PRROC)

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
  
  dataset[, !(names(dataset) %in% c(ohe_features, "TBScreening"))]
  
}

# Load Data ------------
dat <- readRDS('../NDHW Data/TestData/National/national_outlist_0322_allages.rds')
train <- dat$sparse$train
train$TBScreening <- gsub(" ", "", train$TBScreening)
train <- train %>% dplyr::select(-FacilityName, -Longitude, -Latitude, -SiteCode, -County)#-month_of_test, -dayofweek)#, -starts_with("PC"))
hts <- encodeXGBoost(train) 
train$MaritalStatus <- stringr::str_to_title(train$MaritalStatus)
train$Gender <- stringr::str_to_title(train$Gender)
val <- dat$sparse$val
val$TBScreening <- gsub(" ", "", val$TBScreening)
val <- val %>% dplyr::select(-FacilityName, -Longitude, -Latitude, -SiteCode, - County)#-month_of_test, -dayofweek)#, -starts_with("PC"))
val <- encodeXGBoost(val)

# Train best-performing model --------------
# hts_input <- train[, -1]
# hts_label <- as.numeric(train[, 1])-1
# previous_na_action <- options('na.action')
# options(na.action='na.pass')
# hts.matrix = model.matrix(~ . -1, data = hts_input)
# options(na.action=previous_na_action$na.action)
# # Generate XGBoost DMatrix and feature map based on R model matrix
# hts.DMatrix = xgb.DMatrix(hts.matrix, label = hts_label)
# hts.fmap = as.fmap(hts.matrix)
# 
# hts.xgb = xgboost(data = hts.DMatrix,
#                   eta = .3,
#                   max_depth = 14, 
#                   colsample_bytree = .5,
#                   objective = "binary:logistic",
#                   nrounds = 25)
# 
# r2pmml(hts.xgb, "hts_xgb_0323.pmml", fmap = hts.fmap, compact = TRUE)

# Get Shap ------------------------
set.seed(2231)
xgb <- xgboost(data = data.matrix(hts[,which(names(hts) != "FinalTestResult")]), 
               label = hts$FinalTestResult, 
               nrounds = 25,
               eta = .3,
               max_depth = 14, 
               colsample_bytree = .5,
               objective = "binary:logistic",
               metric = 'aucpr'
)

saveRDS(xgb, './xgb_0509.rds')

# ## Calculate shap values
# shap_result = shap.score.rank(xgb_model = xgb, 
#                               X_train = data.matrix(hts[,which(names(hts) != "FinalTestResult")]),
#                               shap_approx = TRUE
# )
# 
# ## Prepare data for top N variables
# shap_long = shap.prep(shap = shap_result,
#                       X_train = data.matrix(hts[,which(names(hts) != "FinalTestResult")]), 
#                       top_n = 10
# )
# 
# ## Plot shap overall metrics
# plot.shap.summary(data_long = shap_long)

# Get PR and ROC curve ---------------
val_predict <- predict(xgb,newdata = data.matrix(val[, which(names(val) != "FinalTestResult")]))
fg <- val_predict[val$FinalTestResult == 1]
bg <- val_predict[val$FinalTestResult == 0]
prc <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(prc, curve = TRUE)
roc <- roc.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(roc, curve = TRUE)

fg_child <- val_predict[val$FinalTestResult == 1 & val$AgeAtTest <= 15]
bg_child <- val_predict[val$FinalTestResult == 0 & val$AgeAtTest <= 15]
prc_child <- pr.curve(scores.class0 = fg_child, scores.class1 = bg_child, curve = T)
plot(prc_child, curve = TRUE)
roc_child <- roc.curve(scores.class0 = fg_child, scores.class1 = bg_child, curve = T)
plot(roc_child, curve = TRUE)


# Get cutoffs
pr_df <- data.frame(prc$curve)
names(pr_df) <- c("Recall", "Precision", "Threshold")
thresh_10 <- pr_df %>%
  filter(Precision >= .1) %>%
  filter(row_number() == 1)

thresh_05 <- pr_df %>%
  filter(Precision >= .05) %>%
  filter(row_number() == 1)

thresh_02 <- pr_df %>%
  filter(Precision >= .02) %>%
  filter(row_number() == 1)

thresh_all <- rbind(thresh_10, thresh_05, thresh_02)
# Recall Precision   Threshold
# 1 0.5764658      0.10 0.028924102
# 2 0.7740877      0.05 0.010638781
# 3 0.9585896      0.02 0.002625179
write.csv(thresh_all, "thresholds_0328.csv", row.names = FALSE)

# Analyze Counts by Risk Score ----------------------
val <- dat$sparse$val
val$risk <- "Low"
val[val_predict >= 0.002625179 & val_predict < 0.0106387809, "risk"] <- "Medium"
val[val_predict >= 0.0106387809 & val_predict < 0.0289241020, "risk"] <- "High"
val[val_predict >= 0.0289241020, "risk"] <- "Highest"

val %>%
  group_by(risk) %>%
  summarize(count = n(),
            positives = sum(FinalTestResult == "Positive"),
            negatives = sum(FinalTestResult == "Negative"))

val %>%
  group_by(risk) %>%
  filter(AgeAtTest <= 15) %>%
  summarize(count = n(),
            positives = sum(FinalTestResult == "Positive"),
            negatives = sum(FinalTestResult == "Negative"))

val %>%
  filter(County %in% c("Homa Bay", "HOMA BAY")) %>%
  group_by(risk) %>%
  summarize(count = n(),
            positives = sum(FinalTestResult == "Positive"),
            negatives = sum(FinalTestResult == "Negative"))

val %>%
  filter(County %in% c("NYERI")) %>%
  group_by(risk) %>%
  summarize(count = n(),
            positives = sum(FinalTestResult == "Positive"),
            negatives = sum(FinalTestResult == "Negative"))

# 556 false negatives out of 2,439
fn <- val[val_predict < 0.010737761 & val$FinalTestResult == 1, ]

summary(fn)
# What's the FNR for adults?
# 0.3% (514 / 168,527)
nrow(val[val_predict < 0.010737761 & val$FinalTestResult == 1 & val$AgeAtTest > 15, ])/
  nrow(val[val_predict < 0.010737761 & val$AgeAtTest > 15, ])
  
# What's the FNR for children?
# 0.27% (42 / 15,606)
nrow(val[val_predict < 0.010737761 & val$FinalTestResult == 1 & val$AgeAtTest <= 15, ])/
  nrow(val[val_predict < 0.010737761 & val$AgeAtTest <= 15, ])

# What's the FNR for women?
# 0.29% (345 / 116,387)
nrow(val[val_predict < 0.010737761 & val$FinalTestResult == 1 & val$Gender.FEMALE == 1, ])/
  nrow(val[val_predict < 0.010737761 & val$Gender.FEMALE == 1, ])

# What's the FNR for men?
# 0.31% (211 / 67,746)
nrow(val[val_predict < 0.010737761 & val$FinalTestResult == 1 & val$Gender.MALE == 1, ])/
  nrow(val[val_predict < 0.010737761 & val$Gender.MALE == 1, ])

# Write facility location data to JSON ---------------
dat <- readRDS('../NDHW Data/TestData/National/national_outlist_0322_allages.rds')
dat <- dat$sparse$train
#Drop the manually added Litare - lat/lon of 34.20599, -0.43659
facs <- dat %>%
  dplyr::filter(County %in% c("Homa Bay", "HOMA BAY")) %>%
  filter(!(Longitude == 34.20599 & Latitude == -0.43659)) %>%
  dplyr::select(1, 16, 19:38) %>%
  unique()
write.csv(facs, './homabay_facilities_0323.csv', row.names = FALSE)
library(rjson)
x <- toJSON(unname(split(facs, 1:nrow(facs))))
write(x, "homabay_facilities.json")
