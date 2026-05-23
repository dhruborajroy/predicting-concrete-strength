# =========================================================
# ARTIFICIAL NEURAL NETWORK (ANN)
# Concrete Strength Prediction
# =========================================================

# =========================================================
# 1. INSTALL REQUIRED PACKAGES
# =========================================================

install.packages("neuralnet")
install.packages("caret")

# =========================================================
# 2. LOAD LIBRARIES
# =========================================================

library(neuralnet)
library(caret)

# =========================================================
# 3. LOAD DATASET
# =========================================================

setwd("G:/R Programming/ML/Predicting Concrete Strength/")

data <- read.csv(
  "G:/R Programming/ML/Predicting Concrete Strength/Raw Data Concrete_Data_Yeh.csv"
)

head(data)
str(data)

# =========================================================
# 4. TRAIN-TEST SPLIT
# =========================================================

set.seed(1234)

ind <- sample(
  2,
  nrow(data),
  replace = TRUE,
  prob = c(0.8, 0.2)
)

train <- data[ind==1,]
test  <- data[ind==2,]

# =========================================================
# 5. NORMALIZATION
# =========================================================

normalize <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

data_norm <- as.data.frame(lapply(data, normalize))

train_norm <- data_norm[ind==1,]
test_norm  <- data_norm[ind==2,]

# =========================================================
# 6. TRAIN ANN MODEL
# =========================================================

ann_model <- neuralnet(
  csMPa ~ cement + slag + flyash + water +
    superplasticizer + coarseaggregate +
    fineaggregate + age,
  
  data = train_norm,
  
  hidden = c(10,5),
  
  linear.output = TRUE
)

# =========================================================
# 7. PREDICTION
# =========================================================

ann_pred_norm <- compute(
  ann_model,
  test_norm[,1:8]
)

ann_pred_norm <- ann_pred_norm$net.result

# =========================================================
# 8. DENORMALIZATION
# =========================================================

max_strength <- max(data$csMPa)
min_strength <- min(data$csMPa)

ann_pred <- ann_pred_norm *
  (max_strength - min_strength) +
  min_strength

# =========================================================
# 9. EVALUATION
# =========================================================

actual <- test$csMPa

rmse <- sqrt(mean((actual - ann_pred)^2))

mae <- mean(abs(actual - ann_pred))

r2 <- cor(actual, ann_pred)^2

cat("ANN RESULTS\n")
cat("RMSE =", rmse, "\n")
cat("MAE =", mae, "\n")
cat("R² =", r2, "\n")

# =========================================================
# 10. ACTUAL VS PREDICTED
# =========================================================

plot(
  actual,
  ann_pred,
  
  pch = 19,
  col = "blue",
  
  xlab = "Actual Strength (MPa)",
  ylab = "Predicted Strength (MPa)",
  
  main = "ANN: Actual vs Predicted"
)

abline(0,1,col="red",lwd=2)