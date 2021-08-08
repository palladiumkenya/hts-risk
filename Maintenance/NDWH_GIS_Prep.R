## This script combines patient-level HTS data from NDWH with geospatial data to prepare a model-ready dataset
##
## Author: Yoni Friedman
## Last edited: May 19, 2021

ndwhGISPrep <- function(ndwh_data, gis_data, county){
  
  # Filter to County
  hts <- ndwh_data %>% filter(County == county) %>% dplyr::select(-County)
  
  # Filter to facilities included in hts
  facilities <- gis_data %>%
    filter(Facility.Name %in% unique(hts$FacilityName))
  
  # Convert demographic counts to shares of population
  facilities[, 23:36] <- facilities[, 23:36] / facilities[, 22]
  
  # Use PCA to synthesize the GIS data -------------
  fac_pca <- facilities %>% dplyr::select(4:ncol(facilities))
  # Remove any varaibles with zero variance as these add no value
  no_var <- apply(fac_pca, 2, var)
  fac_pca <- fac_pca[, no_var != 0]
  fac_pca <- prcomp(fac_pca, center = TRUE,scale. = TRUE) # center and scale data prior to running PCA
  fac_pca_out <- fac_pca$x[, 1:10]
  
  facilities <- cbind.data.frame(facilities[, 1:3], fac_pca_out)
  
  # Join HTS and GIS data -------------------------
  # left join so we don't lose any records
  hts <- merge(facilities, hts, by.x = "Facility.Name", by.y = "FacilityName", all.y = TRUE)
  
  # Group rare classes -----------------------------------------
  # Key Population Type - group into
  kp_other <- names(which(prop.table(table(hts$KeyPopulationType)) < .01))
  hts$KeyPopulationType <- ifelse(hts$KeyPopulationType %in% kp_other, 'OtherKP', hts$KeyPopulationType)
  
  # If any entry point has <1% of observations, group it in Other
  ep_other <- names(which(prop.table(table(hts$EntryPoint)) < .01))
  hts$EntryPoint <- ifelse(hts$EntryPoint %in% ep_other, 'Other', hts$EntryPoint)
  
  
  # Convert to factor variables ---------------
  hts$Gender <- factor(hts$Gender, levels = unique(na.omit(hts$Gender)))
  hts$PatientDisabled <- factor(hts$PatientDisabled, levels = unique(na.omit(hts$PatientDisabled)))
  hts$MaritalStatus <- factor(hts$MaritalStatus, levels = unique(na.omit(hts$MaritalStatus)))
  hts$KeyPopulationType <- factor(hts$KeyPopulationType, levels = unique(na.omit(hts$KeyPopulationType)))
  hts$EverTestedForHIV <- factor(hts$EverTestedForHIV, levels = unique(na.omit(hts$EverTestedForHIV)))
  hts$dayofweek <- factor(hts$dayofweek, levels = unique(na.omit(hts$dayofweek)))
  hts$month_of_test <- factor(hts$month_of_test, levels = unique(na.omit(hts$month_of_test)))
  hts$TBScreening <- factor(hts$TBScreening, levels = unique(na.omit(hts$TBScreening)))
  hts$EntryPoint <- factor(hts$EntryPoint, levels = unique(na.omit(hts$EntryPoint)))
  hts$TestingStrategy <- factor(hts$TestingStrategy, levels = unique(na.omit(hts$TestingStrategy)))
  hts$ClientSelfTested <- factor(hts$ClientSelfTested, levels = unique(na.omit(hts$ClientSelfTested)))
  hts$ClientTestedAs <- factor(hts$ClientTestedAs, levels = unique(na.omit(hts$ClientTestedAs)))
  hts$FinalTestResult <- factor(hts$FinalTestResult, levels = unique(na.omit(hts$FinalTestResult)))
  hts$MonthsSinceLastTest <- as.numeric(hts$MonthsSinceLastTest)
  hts$AgeAtTest <- as.numeric(hts$AgeAtTest)
  
  # Imputation ------------------------------
  set.seed(2231)
  #partition and create training, evaluation testing data
  split <- createDataPartition(y = hts$FinalTestResult,p = 0.6,list = FALSE)
  
  # split into train and test
  train <- hts[split, ] 
  test_all <- hts[-split, ]
  
  # split train into train and validation
  split_test <- createDataPartition(y = test_all$FinalTestResult,p = 0.5,list = FALSE)
  val <- test_all[split_test, ]
  test <- test_all[-split_test, ]
  
  # MICE Imputation -------------------------------
  # Select variables for imputation (no target variable and no id variables)
  cols_to_not_impute <- c("Facility.Name", "Longitude", "Latitude", "Sitecode")
  train_to_impute <- train[, !names(train) %in% cols_to_not_impute]
  test_all_to_impute <- test_all[, !names(test) %in% cols_to_not_impute]
  
  hts_impute_mice <- mice(train_to_impute,
                         m = 5,
                         maxit = 5,
                         seed = 2231)
  hts_impute_mice_train <- complete(hts_impute_mice, action = "broad") # get all imputations
  
  # Get variables with missing fields
  vars_to_impute <- names(train_to_impute)[which(apply(train_to_impute, 2, function(x) any(is.na(x))))]
  
  # loop through vars_to_impute and average imputations
  for(i in vars_to_impute){
    if(is.factor(hts[, i])){
      # for factors, take mode
      train_to_impute[,i] <- factor(apply(hts_impute_mice_train[, c(paste0(i, ".1"), paste0(i, ".2"), paste0(i, ".3"),
                                                                    paste0(i, ".4"), paste0(i, ".5"))],
                                                    1, function(x) unique(x)[which.max(tabulate(match(x, unique(x))))]), 
                                              levels = unique(na.omit(hts[, i])))
    } else if(is.numeric(hts[, i])){
      # for numerics, take mean
      train_to_impute[,i] <- apply(hts_impute_mice_train[, c(paste0(i, ".1"), paste0(i, ".2"), paste0(i, ".3"),
                                                                    paste0(i, ".4"), paste0(i, ".5"))],
                                                                1, function(x) mean(x))
    }
  }
  
  # reuse imputation on test/val data
  hts_impute_mice_test_all <- mice.reuse(hts_impute_mice, test_all_to_impute, maxit = 1)
  hts_impute_mice_test_all <- do.call(cbind, hts_impute_mice_test_all)
  
  # as before, combine imputations - mode for factors, mean fo rnumberics
  for(i in vars_to_impute){
    if(is.factor(hts[, i])){
      test_all_to_impute[,i] <- factor(apply(hts_impute_mice_test_all[, c(paste0("1.", i), paste0("2.", i), paste0("3.", i),
                                                                    paste0("4.", i), paste0("5.", i))],
                                          1, function(x) unique(x)[which.max(tabulate(match(x, unique(x))))]),
                                    levels = unique(na.omit(hts[, i])))
    } else if(is.numeric(hts[, i])){
      test_all_to_impute[,i] <- apply(hts_impute_mice_test_all[, c(paste0("1.", i), paste0("2.", i), paste0("3.", i),
                                                                   paste0("4.", i), paste0("5.", i))],
                                   1, function(x) mean(x))
    }
  }
  
  hts_impute_mice_val <- test_all_to_impute[split_test, ]
  hts_impute_mice_test <- test_all_to_impute[-split_test, ]
  
  # Simple Imputation ------------------------------
  # For continuous variables, take the mean; for categorical variables, take the mode
  Mode <- function(x, na.rm = TRUE) {
    if(na.rm){
      x = x[!is.na(x)]
    }
    
    ux <- unique(x)
    return(ux[which.max(tabulate(match(x, ux)))])
  }
  # Impute missing values with mode or mean from train set
  hts$MaritalStatus <- if_else(is.na(hts$MaritalStatus), Mode(train$MaritalStatus), hts$MaritalStatus)
  hts$EntryPoint <- if_else(is.na(hts$EntryPoint), Mode(train$EntryPoint), hts$EntryPoint)
  hts$TestingStrategy <- if_else(is.na(hts$TestingStrategy), Mode(train$TestingStrategy), hts$TestingStrategy)
  hts$TBScreening <- if_else(is.na(hts$TBScreening), Mode(train$TBScreening), hts$TBScreening)
  hts$ClientSelfTested <- if_else(is.na(hts$ClientSelfTested), Mode(train$ClientSelfTested), hts$ClientSelfTested)
  hts$MonthsSinceLastTest <- if_else(is.na(hts$MonthsSinceLastTest), mean(train$MonthsSinceLastTest, na.rm = TRUE), hts$MonthsSinceLastTest)
  hts$MonthsSinceLastTest <- if_else(hts$EverTestedForHIV == "No", 0, hts$MonthsSinceLastTest)
  
  # split into train and test
  hts_impute_simple_train <- hts[split, ] 
  test_all <- hts[-split, ]
  hts_impute_simple_val <- test_all[split_test, ]
  hts_impute_simple_test <- test_all[-split_test, ]
  
  
  
  
  outlist <- list(
    "facilities" = facilities,
    "sparse" = list(
      "train" = train,
      "val" = val,
      "test" = test
    ),
    "impute_mice" = list(
      "train" = train_to_impute,
      "val" = hts_impute_mice_val,
      "test" = hts_impute_mice_test
    ),
    "impute_simple" = list(
      "train" = hts_impute_simple_train,
      "val" = hts_impute_simple_val,
      "test" = hts_impute_simple_test
    )
  )
  
  return(outlist)

}


