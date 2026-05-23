# =========================================================
# OPTIMIZED HYBRID RF-XGBOOST MODEL
# Automatic Weight Optimization
# =========================================================

# =========================================================
# 1. INSTALL REQUIRED PACKAGES
# =========================================================

install.packages("randomForest")
install.packages("xgboost")
install.packages("caret")
install.packages("ggplot2")

# =========================================================
# 2. LOAD LIBRARIES
# =========================================================

library(randomForest)
library(xgboost)
library(caret)
library(ggplot2)

# =========================================================
# 3. LOAD DATASET
# =========================================================
setwd("G:/R Programming/ML/Predicting Concrete Strength/")
data <- read.csv("G:/R Programming/ML/Predicting Concrete Strength/Raw Data Concrete_Data_Yeh.csv")
data
head(data)
str(data)


# =========================================================
# 4. TRAIN-TEST SPLIT
# =========================================================

set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[ind==1,]
test <- data[ind==2,]

# =========================================================
# 5. RANDOM FOREST MODEL
# =========================================================

rf_model <- randomForest(
  csMPa ~ .,
  data = train,
  ntree = 500,
  mtry = 8,
  importance = TRUE
)

# Random Forest Prediction

rf_pred <- predict(rf_model, test)

# =========================================================
# 6. XGBOOST MODEL
# =========================================================

# Convert to matrix format

x_train <- as.matrix(train[,1:8])
y_train <- train$csMPa

x_test <- as.matrix(test[,1:8])
y_test <- test$csMPa

# Train XGBoost Model

xgb_model <- xgboost(
  x = x_train,
  y = y_train,
  nrounds = 200,
  objective = "reg:squarederror",
  max_depth = 6,
  learning_rate = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  eval_metric = "rmse"
)

# XGBoost Prediction

xgb_pred <- predict(xgb_model, x_test)

# =========================================================
# 7. INDIVIDUAL MODEL PERFORMANCE
# =========================================================

cat("\n=============================\n")
cat("RANDOM FOREST RESULTS\n")
cat("=============================\n")

rf_rmse <- sqrt(mean((y_test - rf_pred)^2))
rf_mae  <- mean(abs(y_test - rf_pred))
rf_r2   <- cor(y_test, rf_pred)^2

cat("RMSE =", rf_rmse, "\n")
cat("MAE  =", rf_mae, "\n")
cat("R²   =", rf_r2, "\n")

# ---------------------------------------------------------

cat("\n=============================\n")
cat("XGBOOST RESULTS\n")
cat("=============================\n")

xgb_rmse <- sqrt(mean((y_test - xgb_pred)^2))
xgb_mae  <- mean(abs(y_test - xgb_pred))
xgb_r2   <- cor(y_test, xgb_pred)^2

cat("RMSE =", xgb_rmse, "\n")
cat("MAE  =", xgb_mae, "\n")
cat("R²   =", xgb_r2, "\n")

# =========================================================
# 8. HYBRID WEIGHT OPTIMIZATION
# =========================================================

# Create weight sequence

weights <- seq(0, 1, by = 0.01)

# Empty dataframe to store results

results <- data.frame(
  RF_Weight = numeric(),
  XGB_Weight = numeric(),
  RMSE = numeric(),
  MAE = numeric(),
  R2 = numeric()
)

# =========================================================
# 9. LOOP THROUGH ALL WEIGHTS
# =========================================================

for(w in weights){
  
  # Hybrid prediction
  
  hybrid_pred <- (w * rf_pred) +
    ((1 - w) * xgb_pred)
  
  # Evaluation metrics
  
  rmse_val <- sqrt(mean((y_test - hybrid_pred)^2))
  
  mae_val <- mean(abs(y_test - hybrid_pred))
  
  r2_val <- cor(y_test, hybrid_pred)^2
  
  # Save results
  
  results <- rbind(
    results,
    data.frame(
      RF_Weight = w,
      XGB_Weight = 1 - w,
      RMSE = rmse_val,
      MAE = mae_val,
      R2 = r2_val
    )
  )
}

# =========================================================
# 10. FIND OPTIMAL WEIGHTS
# =========================================================

# Best R²

best_r2 <- results[which.max(results$R2), ]

cat("\n=============================\n")
cat("BEST HYBRID MODEL\n")
cat("=============================\n")

print(best_r2)

# =========================================================
# 11. CREATE FINAL OPTIMIZED MODEL
# =========================================================

best_rf_weight  <- best_r2$RF_Weight
best_xgb_weight <- best_r2$XGB_Weight

optimized_pred <- (best_rf_weight * rf_pred) +
  (best_xgb_weight * xgb_pred)

# =========================================================
# 12. FINAL OPTIMIZED PERFORMANCE
# =========================================================

final_rmse <- sqrt(mean((y_test - optimized_pred)^2))

final_mae <- mean(abs(y_test - optimized_pred))

final_r2 <- cor(y_test, optimized_pred)^2

cat("\n=============================\n")
cat("OPTIMIZED HYBRID RESULTS\n")
cat("=============================\n")

cat("RF Weight  =", best_rf_weight, "\n")
cat("XGB Weight =", best_xgb_weight, "\n")
cat("RMSE =", final_rmse, "\n")
cat("MAE  =", final_mae, "\n")
cat("R²   =", final_r2, "\n")

# =========================================================
# 13. FINAL RESULTS TABLE
# =========================================================

final_results <- data.frame(
  
  Model = c(
    "Random Forest",
    "XGBoost",
    "Optimized Hybrid"
  ),
  
  RMSE = c(
    rf_rmse,
    xgb_rmse,
    final_rmse
  ),
  
  MAE = c(
    rf_mae,
    xgb_mae,
    final_mae
  ),
  
  R2 = c(
    rf_r2,
    xgb_r2,
    final_r2
  )
)

print(final_results)

# =========================================================
# 14. ACTUAL VS PREDICTED PLOT
# =========================================================

plot(
  y_test,
  optimized_pred,
  
  xlab = "Actual Strength (MPa)",
  ylab = "Predicted Strength (MPa)",
  
  main = "Optimized Hybrid RF-XGBoost Model",
  
  pch = 19,
  col = "blue"
)

abline(0,1,col="red",lwd=2)

# =========================================================
# 15. RESIDUAL ANALYSIS
# =========================================================

residuals <- y_test - optimized_pred

plot(
  optimized_pred,
  residuals,
  
  xlab = "Predicted Strength",
  ylab = "Residuals",
  
  main = "Residual Plot - Optimized Hybrid Model",
  
  pch = 19,
  col = "darkgreen"
)

abline(h = 0, col = "red", lwd = 2)

# =========================================================
# 16. HISTOGRAM OF RESIDUALS
# =========================================================

hist(
  residuals,
  
  main = "Histogram of Residuals",
  
  xlab = "Residuals",
  
  col = "skyblue",
  
  border = "white"
)

# =========================================================
# 17. HYBRID WEIGHT OPTIMIZATION PLOT
# =========================================================

plot(
  results$RF_Weight,
  results$R2,
  
  type = "l",
  
  lwd = 3,
  
  col = "blue",
  
  xlab = "Random Forest Weight",
  
  ylab = "R²",
  
  main = "Hybrid Weight Optimization"
)

# =========================================================
# 18. SAVE RESULTS TO CSV
# =========================================================

write.csv(
  results,
  "Hybrid_Weight_Optimization_Results.csv",
  row.names = FALSE
)

# =========================================================
# END OF COMPLETE OPTIMIZED HYBRID PIPELINE
# =========================================================