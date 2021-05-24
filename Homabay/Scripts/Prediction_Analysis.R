## This script explores the best performing model using Shapley values and uses the best performing model
## to generate an AUC-PR on the test set and to generate test set predictions for review.
##
## Author: Yoni Friedman
## Last edited: May 20, 2021


library(caret)
library(dplyr)
library(lime)
library(xgboost)
library(PRROC)

setwd("~/Kenya/NDHW Data/TestData/HTS_App/FHW App/HomaBay")
hts <- readRDS('./hts_homabay_imputed_0520.rds')


# XGBoost requires only numeric inputs, so one-hot encode all categorical variables
encodeXGBoost <- function(dataset){
  dataset$FinalTestResult <- if_else(dataset$FinalTestResult == "Positive", 1, 0)
  # Need to one-hot encode all the factor variables
  ohe_features <- names(dataset)[ sapply(dataset, is.factor) ]
  
  dmy <- dummyVars("~ Gender + KeyPopulationType + MaritalStatus + PatientDisabled + EverTestedForHIV +
                   ClientTestedAs + EntryPoint + TestingStrategy + TBScreening + ClientSelfTested +
                   month_of_test + dayofweek",
                   data = dataset)
  ohe <- data.frame(predict(dmy, newdata = dataset))
  dataset <- cbind(dataset, ohe)
  
  dataset[, !(names(dataset) %in% ohe_features)]
  
}

hts_sparse_train <- encodeXGBoost(hts$sparse$train) %>%
  select(-Facility.Name, -Longitude, -Latitude, -Sitecode)
hts_sparse_val <- encodeXGBoost(hts$sparse$val) %>%
  select(-Facility.Name, -Longitude, -Latitude, -Sitecode)
hts_sparse_test <- encodeXGBoost(hts$sparse$test) %>%
  select(-Facility.Name, -Longitude, -Latitude, -Sitecode)

# Train best-performing model --------------
set.seed(2231)
xgb <- xgboost::xgboost(data = data.matrix(hts_sparse_train[,which(names(hts_sparse_train) != "FinalTestResult")]), 
                        label = hts_sparse_train$FinalTestResult, 
                        eta = .1,
                        max_depth = 12, 
                        colsample_bytree = .3,
                        nrounds = 100,
                        objective = "binary:logistic",
                        metric = 'auc',
                        verbose = 0
)

# Shapley Values -----------------
source('./shap.R')
## Calculate shap values
shap_result = shap.score.rank(xgb_model = xgb, 
                              X_train = data.matrix(hts_sparse_train[,which(names(hts_sparse_train) != "FinalTestResult")]),
                              shap_approx = F
)

## Prepare data for top N variables
shap_long = shap.prep(shap = shap_result,
                      X_train = data.matrix(hts_sparse_train[,which(names(hts_sparse_train) != "FinalTestResult")]), 
                      top_n = 12
)

## Plot shap overall metrics
plot.shap.summary(data_long = shap_long)


## 
xgb.plot.shap(data = data.matrix(hts_sparse_train[,which(names(hts_sparse_train) != "FinalTestResult")]), # input data
              model = xgb, # xgboost model
              features = names(shap_result$mean_shap_score[1:12]), # only top 10 var
              n_col = 4, # layout option
              plot_loess = T # add red line to plot
)

# Generate predictions for test set -------------------------

# Get AUC-PR for test set
test_predict <- predict(xgb,newdata = data.matrix(hts_sparse_test[, which(names(hts_sparse_test) != "FinalTestResult")]))
fg <- test_predict[hts_sparse_test$FinalTestResult == 1]
bg <- test_predict[hts_sparse_test$FinalTestResult == 0]
prc <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(prc, curve = TRUE)

# Append predictions to predictors and save out
hts_sparse_test <- encodeXGBoost(hts$sparse$test)
test_out <- cbind(test_predict, hts_sparse_test)
write.csv(test_out, './homabay_test_predictions.csv')

# Generate plot of precision/recall by % of patients ---------------------
# Get index of values to select (every ten percentage points)
vals <- c(round(nrow(pr_df)*.01),
          round(nrow(pr_df)*.03),
          round(seq(nrow(pr_df)*.05, nrow(pr_df), length.out = 20)))
pr_by_share <- pr_df %>%
  arrange(desc(Threshold), desc(Precision)) %>% 
  mutate(Recall = round(Recall * 100), Precision = round(Precision * 100)) %>%
  mutate(rownum = row_number()) %>%
  filter(rownum %in% vals) %>%
  mutate(Percent_Patients = round(rownum * 100 / max(vals))) %>%
  pivot_longer(cols = c("Recall", "Precision"), names_to = "Metric")

ggplot() +
  geom_line(data = pr_by_share, aes(x = Percent_Patients, y = value, color = Metric)) +
  xlab("Percent of Patients Tested") +
  ylab("") +
  scale_x_continuous(breaks = seq(5,100,length.out = 20)) +
  scale_color_manual(labels = c("Precision (% Positive)", "Recall (% of all Positives)"), values = c("blue", "red")) +
  theme(legend.position = c(.8, .5)) +
  ggtitle("Precision and Recall by Percent of Patients Tested")
