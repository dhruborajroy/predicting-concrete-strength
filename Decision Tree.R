# =========================================================
# DECISION TREE
# =========================================================

install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

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
# TRAIN MODEL
# =========================================================

dt_model <- rpart(
  csMPa ~ .,
  data = train,
  method = "anova"
)

# =========================================================
# PREDICTION
# =========================================================

dt_pred <- predict(dt_model, test)

# =========================================================
# EVALUATION
# =========================================================

actual <- test$csMPa

rmse <- sqrt(mean((actual - dt_pred)^2))

mae <- mean(abs(actual - dt_pred))

r2 <- cor(actual, dt_pred)^2

cat("DECISION TREE RESULTS\n")
cat("RMSE =", rmse, "\n")
cat("MAE =", mae, "\n")
cat("R² =", r2, "\n")

# =========================================================
# TREE PLOT
# =========================================================

rpart.plot(dt_model)

# =========================================================
# ACTUAL VS PREDICTED
# =========================================================

plot(
  actual,
  dt_pred,
  
  pch = 19,
  col = "blue",
  
  xlab = "Actual Strength",
  ylab = "Predicted Strength",
  
  main = "Decision Tree: Actual vs Predicted"
)

abline(0,1,col="red",lwd=2)