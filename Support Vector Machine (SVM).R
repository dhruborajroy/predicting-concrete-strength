# =========================================================
# SUPPORT VECTOR MACHINE (SVM)
# =========================================================

install.packages("e1071")

library(e1071)

# =========================================================
# LOAD DATA
# =========================================================

setwd("G:/R Programming/ML/Predicting Concrete Strength/")

data <- read.csv(
  "G:/R Programming/ML/Predicting Concrete Strength/Raw Data Concrete_Data_Yeh.csv"
)

# =========================================================
# TRAIN TEST SPLIT
# =========================================================

set.seed(1234)

ind <- sample(
  2,
  nrow(data),
  replace = TRUE,
  prob = c(0.8,0.2)
)

train <- data[ind==1,]
test  <- data[ind==2,]

# =========================================================
# TRAIN SVM MODEL
# =========================================================

svm_model <- svm(
  csMPa ~ .,
  data = train,
  
  type = "eps-regression",
  
  kernel = "radial"
)

# =========================================================
# PREDICTION
# =========================================================

svm_pred <- predict(svm_model, test)

# =========================================================
# EVALUATION
# =========================================================

actual <- test$csMPa

rmse <- sqrt(mean((actual - svm_pred)^2))

mae <- mean(abs(actual - svm_pred))

r2 <- cor(actual, svm_pred)^2

cat("SVM RESULTS\n")
cat("RMSE =", rmse, "\n")
cat("MAE =", mae, "\n")
cat("R² =", r2, "\n")

# =========================================================
# PLOT
# =========================================================

plot(
  actual,
  svm_pred,
  
  pch = 19,
  col = "blue",
  
  xlab = "Actual Strength",
  ylab = "Predicted Strength",
  
  main = "SVM: Actual vs Predicted"
)

abline(0,1,col="red",lwd=2)