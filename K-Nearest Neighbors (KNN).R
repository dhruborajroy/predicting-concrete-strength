# =========================================================
# K-NEAREST NEIGHBORS (KNN)
# =========================================================

install.packages("caret")

library(caret)

# =========================================================
# LOAD DATA
# =========================================================

setwd("G:/R Programming/ML/Predicting Concrete Strength/")

data <- read.csv(
  "G:/R Programming/ML/Predicting Concrete Strength/Raw Data Concrete_Data_Yeh.csv"
)

# =========================================================
# NORMALIZATION
# =========================================================

normalize <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

data_norm <- as.data.frame(lapply(data, normalize))

# =========================================================
# TRAIN TEST SPLIT
# =========================================================

set.seed(1234)

ind <- sample(
  2,
  nrow(data_norm),
  replace = TRUE,
  prob = c(0.8,0.2)
)

train <- data_norm[ind==1,]
test  <- data_norm[ind==2,]

# =========================================================
# TRAIN KNN MODEL
# =========================================================

knn_model <- train(
  csMPa ~ .,
  data = train,
  
  method = "knn",
  
  tuneLength = 10
)

# =========================================================
# PREDICTION
# =========================================================

knn_pred_norm <- predict(knn_model, test)

# =========================================================
# DENORMALIZATION
# =========================================================

max_strength <- max(data$csMPa)
min_strength <- min(data$csMPa)

knn_pred <- knn_pred_norm *
  (max_strength - min_strength) +
  min_strength

actual <- test$csMPa *
  (max_strength - min_strength) +
  min_strength

# =========================================================
# EVALUATION
# =========================================================

rmse <- sqrt(mean((actual - knn_pred)^2))

mae <- mean(abs(actual - knn_pred))

r2 <- cor(actual, knn_pred)^2

cat("KNN RESULTS\n")
cat("RMSE =", rmse, "\n")
cat("MAE =", mae, "\n")
cat("R² =", r2, "\n")

# =========================================================
# PLOT
# =========================================================

plot(
  actual,
  knn_pred,
  
  pch = 19,
  col = "blue",
  
  xlab = "Actual Strength",
  ylab = "Predicted Strength",
  
  main = "KNN: Actual vs Predicted"
)

abline(0,1,col="red",lwd=2)