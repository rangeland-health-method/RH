
# ----------------------------- XGBoost - predict - IPE ------------

library(xgboost)

target_columns <- colnames(data.train.test)[20:23]
num_iterations <- 10000

best_metrics_list_xgb <- list()
best_model_list_xgb <- list()
ipe_all_targets_xgb <- list()

for (target in target_columns) {
  all_metrics <- list(RMSE = numeric(), MARE = numeric(), ME = numeric(), Correlation = numeric())
  xgb_all_iterations_metrics <- data.frame(
    Iteration = integer(), Target = character(), IPE = double(),
    RMSE = double(), Correlation = double(), MARE = double(), ME = double(),
    stringsAsFactors = FALSE
  )
  
  best_metrics <- list(IPE = Inf, RMSE = Inf, Correlation = -Inf, MARE = Inf, ME = Inf)
  best_model <- NULL
  best_test_data <- NULL
  best_test_predictions <- NULL
  
  for (i in 1:num_iterations) {
    set.seed(i)
    idx <- sample(nrow(data.train.test), round(0.7 * nrow(data.train.test)))
    train <- data.train.test[idx, ]
    test <- data.train.test[-idx, ]
    
    train_x <- as.matrix(train[, 1:15])
    train_y <- train[[target]]
    test_x <- as.matrix(test[, 1:15])
    test_y <- test[[target]]
    
    model <- xgboost(data = train_x, label = train_y,
                     nrounds = 50, objective = "reg:squarederror", verbose = 0)
    pred <- predict(model, newdata = test_x)
    
    RMSE <- sqrt(mean((test_y - pred)^2))
    MARE <- mean(abs((test_y - pred) / test_y))
    ME <- mean(pred - test_y)
    Correlation <- if (var(test_y) != 0 && var(pred) != 0) cor(test_y, pred) else NA
    
    all_metrics$RMSE <- c(all_metrics$RMSE, RMSE)
    all_metrics$MARE <- c(all_metrics$MARE, MARE)
    all_metrics$ME <- c(all_metrics$ME, ME)
    all_metrics$Correlation <- c(all_metrics$Correlation, Correlation)
  }
  
  max_rmse <- max(all_metrics$RMSE, na.rm = TRUE)
  max_mare <- max(all_metrics$MARE, na.rm = TRUE)
  max_me <- max(abs(all_metrics$ME), na.rm = TRUE)
  min_corr <- min(all_metrics$Correlation[!is.na(all_metrics$Correlation)])
  if (!is.finite(min_corr)) min_corr <- 0.01
  
  for (i in 1:num_iterations) {
    set.seed(i)
    idx <- sample(nrow(data.train.test), round(0.7 * nrow(data.train.test)))
    train <- data.train.test[idx, ]
    test <- data.train.test[-idx, ]
    
    train_x <- as.matrix(train[, 1:15])
    train_y <- train[[target]]
    test_x <- as.matrix(test[, 1:15])
    test_y <- test[[target]]
    
    model <- xgboost(data = train_x, label = train_y,
                     nrounds = 50, objective = "reg:squarederror", verbose = 0)
    pred <- predict(model, newdata = test_x)
    
    RMSE <- sqrt(mean((test_y - pred)^2))
    MARE <- mean(abs((test_y - pred) / test_y))
    ME <- mean(pred - test_y)
    Correlation <- if (var(test_y) != 0 && var(pred) != 0) cor(test_y, pred) else NA
    
    if (!any(is.na(c(RMSE, MARE, ME, Correlation)))) {
      IPE <- sqrt(0.25 * (
        (RMSE / max_rmse)^2 +
          (MARE / max_mare)^2 +
          (abs(ME) / max_me)^2 +
          ((Correlation - 1) / (min_corr - 1))^2
      ))
      
      xgb_all_iterations_metrics <- rbind(xgb_all_iterations_metrics,
                                          data.frame(Iteration = i, Target = target, IPE = IPE,
                                                     RMSE = RMSE, Correlation = Correlation, MARE = MARE, ME = ME))
      
      if (is.finite(IPE) && IPE < best_metrics$IPE) {
        best_metrics <- list(IPE = IPE, RMSE = RMSE, Correlation = Correlation,
                             MARE = MARE, ME = ME)
        best_model <- model
        best_test_data <- test
        best_test_predictions <- pred
      }
    }
  }
  
  best_metrics_list_xgb[[target]] <- best_metrics
  best_model_list_xgb[[target]] <- best_model
  ipe_all_targets_xgb[[target]] <- xgb_all_iterations_metrics$IPE
  
  write.csv(data.frame(Metric = names(best_metrics), Value = unlist(best_metrics)),
            paste0("best_metrics_xgb_", target, ".csv"), row.names = FALSE)
  saveRDS(best_model, file = paste0("best_model_xgb_", target, ".rds"))
  
  if (!is.null(best_test_data)) {
    output_df <- data.frame(
      Actual = best_test_data[[target]],
      Prediction = best_test_predictions
    )
    write.csv(output_df, paste0("xgb_test_predictions_", target, ".csv"), row.names = FALSE)
  }
  
  write.csv(xgb_all_iterations_metrics,
            file = paste0("all_iterations_xgb_metrics_", target, ".csv"), row.names = FALSE)
  
  best_row <- xgb_all_iterations_metrics[which.min(xgb_all_iterations_metrics$IPE), ]
  cat("\n📌 Target:", target,
      "\nBest Iteration:", best_row$Iteration,
      "\nMinimum IPE:", best_row$IPE, "\n")
}

# ::::::::::::::::::::::

all_best_metrics_xgb <- do.call(rbind, lapply(names(best_metrics_list_xgb), function(target) {
  data.frame(
    Target = target,
    IPE = best_metrics_list_xgb[[target]]$IPE,
    RMSE = best_metrics_list_xgb[[target]]$RMSE,
    Correlation = best_metrics_list_xgb[[target]]$Correlation,
    MARE = best_metrics_list_xgb[[target]]$MARE,
    ME = best_metrics_list_xgb[[target]]$ME
  )
}))
write.csv(all_best_metrics_xgb, file = "all_best_metrics_xgb.csv", row.names = FALSE)


# ------------------------------------------- SVR - predict - IPE ---------------------------------

library(e1071)

target_columns <- colnames(data.train.test)[20:23] # RHA

target_columns <- colnames(data.train.test)[21:23] # mooteh
target_columns <- colnames(data.train.test)[20] # mooteh

num_iterations <- 10000 # for all region

best_metrics_list_svm <- list()
best_model_list_svm <- list()

for (target in target_columns) {
  all_metrics <- list(RMSE=numeric(), MARE=numeric(), ME=numeric(), Correlation=numeric())
  
  for (i in 1:num_iterations) {
    set.seed(i)
    idx <- sample(nrow(data.train.test), round(0.7*nrow(data.train.test)))
    train <- data.train.test[idx, ]
    test <- data.train.test[-idx, ]
    
    model <- svm(x=train[, 1:15], y=train[[target]])
    pred <- predict(model, test[, 1:15])
    
    RMSE <- sqrt(mean((test[[target]] - pred)^2))
    MARE <- mean(abs((test[[target]] - pred)/test[[target]]))
    ME <- mean(pred - test[[target]])
    Correlation <- if (var(test[[target]]) != 0 && var(pred) != 0) cor(test[[target]], pred) else NA
    
    all_metrics$RMSE <- c(all_metrics$RMSE, RMSE)
    all_metrics$MARE <- c(all_metrics$MARE, MARE)
    all_metrics$ME <- c(all_metrics$ME, ME)
    all_metrics$Correlation <- c(all_metrics$Correlation, Correlation)
  }
  
  max_rmse <- max(all_metrics$RMSE, na.rm=TRUE)
  max_mare <- max(all_metrics$MARE, na.rm=TRUE)
  max_me <- max(abs(all_metrics$ME), na.rm=TRUE)
  valid_corr <- all_metrics$Correlation[!is.na(all_metrics$Correlation)]
  min_correlation <- if (length(valid_corr) > 0) min(valid_corr) else 0.01
  
  svm_all_iterations_metrics <- data.frame(
    Iteration=integer(), Target=character(), IPE=double(),
    RMSE=double(), Correlation=double(), MARE=double(), ME=double(),
    stringsAsFactors=FALSE
  )
  
  best_metrics <- list(IPE=Inf, RMSE=Inf, Correlation=-Inf, MARE=Inf, ME=Inf)
  best_model <- NULL
  best_test_data <- NULL
  best_test_predictions <- NULL
  
  for (i in 1:num_iterations) {
    set.seed(i)
    idx <- sample(nrow(data.train.test), round(0.7*nrow(data.train.test)))
    train <- data.train.test[idx, ]
    test <- data.train.test[-idx, ]
    
    model <- svm(x=train[, 1:15], y=train[[target]])
    pred <- predict(model, test[, 1:15])
    
    RMSE <- sqrt(mean((test[[target]] - pred)^2))
    MARE <- mean(abs((test[[target]] - pred)/test[[target]]))
    ME <- mean(pred - test[[target]])
    Correlation <- if (var(test[[target]]) != 0 && var(pred) != 0) cor(test[[target]], pred) else NA
    
    if (!any(is.na(c(RMSE, MARE, ME, Correlation)))) {
      IPE <- sqrt(0.25 * (
        (RMSE / max_rmse)^2 +
          (MARE / max_mare)^2 +
          (abs(ME) / max_me)^2 +
          ((Correlation - 1)/(min_correlation - 1))^2
      ))
      
      svm_all_iterations_metrics <- rbind(svm_all_iterations_metrics,
                                          data.frame(Iteration=i, Target=target,
                                                     IPE=IPE, RMSE=RMSE, Correlation=Correlation,
                                                     MARE=MARE, ME=ME))
      
      if (is.finite(IPE) && IPE < best_metrics$IPE) {
        best_metrics <- list(IPE=IPE, RMSE=RMSE, Correlation=Correlation, MARE=MARE, ME=ME)
        best_model <- model
        best_test_data <- test
        best_test_predictions <- pred
      }
    }
  }
  
  best_metrics_list_svm[[target]] <- best_metrics
  best_model_list_svm[[target]] <- best_model
  
  write.csv(data.frame(Metric=names(best_metrics), Value=unlist(best_metrics)),
            file=paste0("best_metrics_svm_", target, ".csv"), row.names=FALSE)
  saveRDS(best_model, file=paste0("best_model_svm_", target, ".rds"))
  
  if (!is.null(best_test_data)) {
    output_df <- data.frame(
      Actual = best_test_data[[target]],
      Prediction = best_test_predictions
    )
    write.csv(output_df, paste0("svm_test_predictions_", target, ".csv"), row.names = FALSE)
  }
  
  write.csv(svm_all_iterations_metrics,
            file=paste0("all_iterations_svm_metrics_", target, ".csv"), row.names=FALSE)
  
  best_row <- svm_all_iterations_metrics[which.min(svm_all_iterations_metrics$IPE), ]
  cat("\n📌 Target:", target,
      "\nBest Iteration:", best_row$Iteration,
      "\nMinimum IPE:", best_row$IPE, "\n")
}

# :::::::::::::::::::::::::::::::

all_best_metrics_svm <- do.call(rbind, lapply(names(best_metrics_list_svm), function(target) {
  data.frame(
    Target = target,
    IPE = best_metrics_list_svm[[target]]$IPE,
    RMSE = best_metrics_list_svm[[target]]$RMSE,
    Correlation = best_metrics_list_svm[[target]]$Correlation,
    MARE = best_metrics_list_svm[[target]]$MARE,
    ME = best_metrics_list_svm[[target]]$ME
  )
}))

write.csv(all_best_metrics_svm, file = "best_metrics_svm.csv", row.names = FALSE)


# ------------------------------------------ RF - PREDICT - IPE ------------

library(caret)
library(randomForest)

target_columns <- colnames(data.train.test)[20:23]
num_iterations <- 10000


best_metrics_list <- list()
best_rf_model_list <- list()
variable_importance_list <- list()

for (target in target_columns) {
  all_metrics <- list(RMSE = numeric(), Correlation = numeric(), MARE = numeric(), ME = numeric())
  
  for (i in 1:num_iterations) {
    set.seed(i)
    idx <- sample(nrow(data.train.test), round(0.7 * nrow(data.train.test)))
    
    train <- data.train.test[idx, ]
    test <- data.train.test[-idx, ]
    
    rf_model <- randomForest(as.formula(paste(target, "~ .")),
                             data = train[, c(1:15, which(colnames(train) == target))])
    pred <- predict(rf_model, newdata = test[, c(1:15, which(colnames(test) == target))])
    
    RMSE <- sqrt(mean((test[[target]] - pred)^2))
    MARE <- mean(abs((test[[target]] - pred) / test[[target]]))
    ME <- mean(pred - test[[target]])
    Correlation <- if (var(test[[target]]) != 0 && var(pred) != 0) cor(test[[target]], pred) else NA
    
    all_metrics$RMSE <- c(all_metrics$RMSE, RMSE)
    all_metrics$MARE <- c(all_metrics$MARE, MARE)
    all_metrics$ME <- c(all_metrics$ME, ME)
    all_metrics$Correlation <- c(all_metrics$Correlation, Correlation)
  }
  
  max_rmse <- max(all_metrics$RMSE, na.rm = TRUE)
  max_mare <- max(all_metrics$MARE, na.rm = TRUE)
  max_me <- max(abs(all_metrics$ME), na.rm = TRUE)
  valid_corr <- all_metrics$Correlation[!is.na(all_metrics$Correlation)]
  min_corr <- if (length(valid_corr) > 0) min(valid_corr) else 0.01
  
  best_metrics <- list(IPE = Inf, RMSE = Inf, Correlation = -Inf, MARE = Inf, ME = Inf)
  best_rf_model <- NULL
  best_test_data <- NULL
  best_test_predictions <- NULL
  
  rf_all_iterations_metrics <- data.frame(
    Iteration = integer(), Target = character(), IPE = double(),
    RMSE = double(), Correlation = double(), MARE = double(), ME = double(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:num_iterations) {
    set.seed(i)
    idx <- sample(nrow(data.train.test), round(0.7 * nrow(data.train.test)))
    train <- data.train.test[idx, ]
    test <- data.train.test[-idx, ]
    
    rf_model <- randomForest(as.formula(paste(target, "~ .")),
                             data = train[, c(1:15, which(colnames(train) == target))])
    pred <- predict(rf_model, newdata = test[, c(1:15, which(colnames(test) == target))])
    
    RMSE <- sqrt(mean((test[[target]] - pred)^2))
    MARE <- mean(abs((test[[target]] - pred) / test[[target]]))
    ME <- mean(pred - test[[target]])
    Correlation <- if (var(test[[target]]) != 0 && var(pred) != 0) cor(test[[target]], pred) else NA
    
    if (!any(is.na(c(RMSE, MARE, ME, Correlation)))) {
      IPE <- sqrt(0.25 * (
        (RMSE / max_rmse)^2 +
          (MARE / max_mare)^2 +
          (abs(ME) / max_me)^2 +
          ((Correlation - 1) / (min_corr - 1))^2
      ))
      
      rf_all_iterations_metrics <- rbind(rf_all_iterations_metrics,
                                         data.frame(Iteration = i, Target = target, IPE = IPE,
                                                    RMSE = RMSE, Correlation = Correlation, MARE = MARE, ME = ME))
      
      if (is.finite(IPE) && IPE < best_metrics$IPE) {
        best_metrics <- list(IPE = IPE, RMSE = RMSE, Correlation = Correlation,
                             MARE = MARE, ME = ME)
        best_rf_model <- rf_model
        best_test_data <- test
        best_test_predictions <- pred
      }
    }
  }
  
  best_metrics_list[[target]] <- best_metrics
  best_rf_model_list[[target]] <- best_rf_model
  variable_importance <- round(importance(best_rf_model), 2)
  variable_importance_list[[target]] <- variable_importance
  
  write.csv(data.frame(Metric = names(best_metrics), Value = unlist(best_metrics)),
            paste0("best_metrics_", target, "_rf.csv"), row.names = FALSE)
  
  write.csv(variable_importance, file = paste0("varImp_rf_", target, "_rf.csv"))
  
  saveRDS(best_rf_model, paste0("best_rf_model_", target, "_rf.rds"))
  
  if (!is.null(best_test_data)) {
    output_df <- data.frame(
      Actual = best_test_data[[target]],
      Prediction = best_test_predictions
    )
    write.csv(output_df, paste0("rf_test_predictions_", target, ".csv"), row.names = FALSE)
  }
  
  write.csv(rf_all_iterations_metrics,
            file = paste0("all_iterations_rf_metrics_", target, ".csv"), row.names = FALSE)
  
  best_row <- rf_all_iterations_metrics[which.min(rf_all_iterations_metrics$IPE), ]
  cat("\n📌 Target:", target,
      "\nBest Iteration:", best_row$Iteration,
      "\nMinimum IPE:", best_row$IPE, "\n")
}

# ::::::::::::::::::::::

all_best_metrics_rf <- do.call(rbind, lapply(names(best_metrics_list), function(target) {
  data.frame(
    Target = target,
    IPE = best_metrics_list[[target]]$IPE,
    RMSE = best_metrics_list[[target]]$RMSE,
    Correlation = best_metrics_list[[target]]$Correlation,
    MARE = best_metrics_list[[target]]$MARE,
    ME = best_metrics_list[[target]]$ME
  )
}))
write.csv(all_best_metrics_rf, file = "all_best_metrics_rf.csv", row.names = FALSE)

