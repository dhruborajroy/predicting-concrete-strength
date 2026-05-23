#read data

library(xgboost)
library(caret)


data <- read.csv("G:/R Programming/ML/Predicting Concrete Strength/Raw Data Concrete_Data_Yeh.csv")
data
head(data)
str(data)


#data partition
set.seed(123)
ind <- sample(2,nrow(data),replace=TRUE, prob = c(0.7,.3))
train <- data[ind==1,] # after comma means all the columns
test <- data[ind==2,] # after comma means all the columns

#Convert Data to Matrix Format
x_train <- as.matrix(train[,1:8])
str(x_train)
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

#Predict Concrete Strength
xgb_pred <- predict(xgb_model, x_test)


#Evaluate Model Performance
#RMSE
sqrt(mean((y_test - xgb_pred)^2))
#MAE
mean(abs(y_test - xgb_pred))
#R²
cor(y_test, xgb_pred)^2
#Create Actual vs Predicted Plot
plot(y_test, xgb_pred,
     xlab = "Actual Strength (MPa)",
     ylab = "Predicted Strength (MPa)",
     main = "XGBoost: Actual vs Predicted Concrete Strength",
     pch = 19,
     col = "blue")

abline(0,1,col="red",lwd=2)
#Feature Importance
importance_matrix <- xgb.importance(
  feature_names = colnames(x_train),
  model = xgb_model
)

print(importance_matrix)
#Importance Plot
xgb.plot.importance(importance_matrix)
