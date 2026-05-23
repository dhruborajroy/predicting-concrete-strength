# =========================================================
# HYBRID RANDOM FOREST + XGBOOST MODEL
# Concrete Compressive Strength Prediction
# =========================================================

# =========================================================
# 1. INSTALL REQUIRED PACKAGES
# =========================================================

install.packages("randomForest")
install.packages("xgboost")
install.packages("caret")
install.packages("Metrics")
install.packages("ggplot2")

# =========================================================
# 2. LOAD LIBRARIES
# =========================================================

library(randomForest)
library(xgboost)
library(caret)
library(Metrics)
library(ggplot2)

# =========================================================
# 3. LOAD DATASET
# =========================================================

# Example:

data <- read.csv("G:/R Programming/ML/Predicting Concrete Strength/Raw Data Concrete_Data_Yeh.csv")
data
head(data)
str(data)

# Check dataset
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
# 7. HYBRID ENSEMBLE MODEL
# =========================================================

# Weighted Ensemble
# 40% Random Forest + 60% XGBoost

hybrid_pred <- (0.4 * rf_pred) + (0.6 * xgb_pred)

# =========================================================
# 8. MODEL EVALUATION FUNCTION
# =========================================================

evaluate_model <- function(actual, predicted, model_name){
  
  rmse_value <- sqrt(mean((actual - predicted)^2))
  
  mae_value <- mean(abs(actual - predicted))
  
  r2_value <- cor(actual, predicted)^2
  
  cat("\n==============================\n")
  cat("MODEL:", model_name, "\n")
  cat("==============================\n")
  cat("RMSE =", rmse_value, "\n")
  cat("MAE  =", mae_value, "\n")
  cat("R²   =", r2_value, "\n")
  
  return(data.frame(
    Model = model_name,
    RMSE = rmse_value,
    MAE = mae_value,
    R2 = r2_value
  ))
}

# =========================================================
# 9. EVALUATE ALL MODELS
# =========================================================

rf_results <- evaluate_model(y_test, rf_pred, "Random Forest")

xgb_results <- evaluate_model(y_test, xgb_pred, "XGBoost")

hybrid_results <- evaluate_model(y_test, hybrid_pred, "Hybrid RF-XGBoost")

# =========================================================
# 10. COMBINE RESULTS TABLE
# =========================================================

final_results <- rbind(
  rf_results,
  xgb_results,
  hybrid_results
)

print(final_results)

# =========================================================
# 11. ACTUAL VS PREDICTED PLOT
# =========================================================

plot(
  y_test,
  hybrid_pred,
  xlab = "Actual Strength (MPa)",
  ylab = "Predicted Strength (MPa)",
  main = "Hybrid RF-XGBoost Model",
  pch = 19,
  col = "blue"
)

abline(0,1,col="red",lwd=2)

# =========================================================
# 12. RESIDUAL ANALYSIS
# =========================================================

residuals <- y_test - hybrid_pred

# Residual Plot
  
  plot(
    hybrid_pred,
    residuals,
    xlab = "Predicted Strength",
    ylab = "Residuals",
    main = "Residual Plot - Hybrid Model",
    pch = 19,
    col = "darkgreen"
  )

abline(h = 0, col = "red", lwd = 2)

# =========================================================
# 13. HISTOGRAM OF RESIDUALS
# =========================================================

hist(
  residuals,
  main = "Histogram of Residuals",
  xlab = "Residuals",
  col = "skyblue",
  border = "white"
)

# =========================================================
# 14. RANDOM FOREST FEATURE IMPORTANCE
# =========================================================

importance(rf_model)

varImpPlot(
  rf_model,
  main = "Random Forest Feature Importance"
)

# =========================================================
# 15. XGBOOST FEATURE IMPORTANCE
# =========================================================

importance_matrix <- xgb.importance(
  feature_names = colnames(x_train),
  model = xgb_model
)

print(importance_matrix)

xgb.plot.importance(importance_matrix)

# =========================================================
# 16. SCATTER PLOT MATRIX
# =========================================================

install.packages("GGally")

library(GGally)

important_vars <- data[, c(
  "cement",
  "water",
  "superplasticizer",
  "age",
  "csMPa"
)]

ggpairs(
  important_vars,
  lower = list(
    continuous = wrap(
      "points",
      alpha = 0.6,
      color = "blue"
    )
  ),
  
  diag = list(
    continuous = wrap(
      "densityDiag",
      fill = "skyblue"
    )
  ),
  
  upper = list(
    continuous = wrap(
      "cor",
      size = 5
    )
  )
) +
  theme_bw() +
  ggtitle("Scatter Plot Matrix of Important Concrete Variables")

# =========================================================
# END OF COMPLETE HYBRID MODEL PIPELINE
# =========================================================

