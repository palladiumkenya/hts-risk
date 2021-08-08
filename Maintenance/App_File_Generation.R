## This script explores the best performing model using Shapley values and uses the best performing model
## to generate an AUC-PR on the test set and to generate test set predictions for review.
##
## Author: Yoni Friedman
## Last edited: May 20, 2021

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

prepAppFiles <- function(model_data){

  hts_sparse_train <- encodeXGBoost(model_data$sparse$train) %>%
    dplyr::select(-Facility.Name, -Longitude, -Latitude, -Sitecode)
  hts_sparse_val <- encodeXGBoost(model_data$sparse$val) %>%
    dplyr::select(-Facility.Name, -Longitude, -Latitude, -Sitecode)
  hts_sparse_test <- encodeXGBoost(model_data$sparse$test) %>%
    dplyr::select(-Facility.Name, -Longitude, -Latitude, -Sitecode)
  
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
  
  # Generate cutoffs for each facility ----------------------
  hts_for_cutoffs <- encodeXGBoost(model_data$sparse$test) %>%
    dplyr::select(-Longitude, -Latitude, -Sitecode)
  facilities_for_cutoff <- hts_for_cutoffs %>%
    .$Facility.Name %>%
    unique()
  
  cutoff_list <- list()
  
  for(i in facilities_for_cutoff){
    
    hts_tmp <- hts_for_cutoffs[hts_for_cutoffs$Facility.Name==i, ]
    if(nrow(hts_tmp)<200){next}
    hts_mod <- hts_tmp %>% dplyr::select(xgb$feature_names)
    preds_tmp <- predict(xgb, newdata = data.matrix(hts_mod))
    fg <- preds_tmp[hts_tmp[, "FinalTestResult"] == 1]
    bg <- preds_tmp[hts_tmp[, "FinalTestResult"] == 0]
    prc <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
    pr_df <- data.frame(prc$curve)
    names(pr_df) <- c("Recall", "Precision", "Threshold")
    thresh_75 <- pr_df %>%
      filter(Precision > .75) %>%
      filter(row_number() == 1)
    
    thresh_50 <- pr_df %>%
      filter(Precision > .5) %>%
      filter(row_number() == 1)
    
    thresh_25 <- pr_df %>%
      filter(Precision > .25) %>%
      filter(row_number() == 1)
    
    thresh_all <- rbind(thresh_75, thresh_50, thresh_25)
    
    cutoff_list[[i]] <- thresh_all 
    
  }
  
  # For facilities with fewer than 100 observations, use overall cutoffs
  test_predict <- predict(xgb,newdata = data.matrix(hts_sparse_test[, which(names(hts_sparse_test) != "FinalTestResult")]))
  fg <- test_predict[hts_sparse_test$FinalTestResult == 1]
  bg <- test_predict[hts_sparse_test$FinalTestResult == 0]
  prc <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
  pr_df <- data.frame(prc$curve)
  names(pr_df) <- c("Recall", "Precision", "Threshold")
  thresh_75 <- pr_df %>%
    filter(Precision > .75) %>%
    filter(row_number() == 1)
  
  thresh_50 <- pr_df %>%
    filter(Precision > .5) %>%
    filter(row_number() == 1)
  
  thresh_25 <- pr_df %>%
    filter(Precision > .25) %>%
    filter(row_number() == 1)
  
  thresh_all <- rbind(thresh_75, thresh_50, thresh_25)
  
  cutoff_list[["Overall"]] <- thresh_all
  
  outlist <- list(
    "xgb_model" = xgb,
    "cutoff_list" = cutoff_list
  )

  return(outlist)
  
}
