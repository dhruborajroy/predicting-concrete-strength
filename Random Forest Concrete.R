#read data

data <- read.csv("G:/R Programming/ML/Predicting Concrete Strength/Raw Data Concrete_Data_Yeh.csv")
data
head(data)
str(data)
#data$csMPaF <- as.factor(data$csMPa)
#data$csMPaF 


table(data$csMPaF )

#data partition

set.seed(123)

ind <- sample(2,nrow(data),replace=TRUE, prob = c(0.7,.3))
train <- data[ind==1,] # after comma means all the columns
test <- data[ind==2,] # after comma means all the columns

#Random Forest
#install.packages("randomForest")

library(randomForest)
set.seed(222)
rf <-  randomForest(csMPa~.,data=train) # ~. means all the variables 

rf
write.csv(rf,file = "random_forest_result.csv", row.names = TRUE)

# randomForest(formula = NSP ~ ., data = train) 
######## Means NSP model, ~. means all the variables, data= train
# Type of random forest: classification
# Number of trees: 500  <- ntree
#Means 500 trees default
# No. of variables tried at each split: 4 <- mtry 
# 4 is sqrt of total variables sqrt(21 variables) 
# OOB estimate of  error rate: 5.81%
# 95% accuracy 
# Confusion matrix:
# 1   2   3 class.error
# 1 1127  15   4  0.01657941
# 2   50 152   2  0.25490196
# 3    8   7 115  0.11538462

attributes(rf)

rf$type
rf$mtry


#prediction & confusion matrix - train data
#install.packages(caret)
library(caret)

p1 <- predict(rf,train)
#RMSE
sqrt(mean((train$csMPa - p1)^2))
#MAE 
mean(abs(train$csMPa - p1))

#R^2
cor(train$csMPa, p1)^2

#Plot Actual vs Predicted
plot(train$csMPa, p1,
     xlab="Actual Strength",
     ylab="Predicted Strength",
     main="Actual vs Predicted Concrete Strength")
abline(0,1,col="red")

importance(rf)
varImpPlot(rf)

str(p1)
# Identify all unique levels from both sources
all_levels <- union(levels(as.factor(p1)), levels(as.factor(train$csMPa)))

# Re-factor both variables with these shared levels
p1_factored <- factor(p1, levels = all_levels)
reference_factored <- factor(train$csMPa, levels = all_levels)

# Run the confusion matrix again
conf_matrix<-confusionMatrix(p1_factored, reference_factored)

write.csv(conf_matrix, file = "conf_matrix.csv", row.names = TRUE)




atribute(train$csMPa)

#Reference
#Prediction    1    2    3
#          1 1146    2    0
#          2    0  202    0
#          3    0    0  130

# here the overall accuracy is (sum of diagonal member)/(total train data) 
# total 2 miss prediction 2 class


#prediction & confusion matrix - test data
#install.packages(caret)
#library(caret)

p2 <- predict(rf,test)


str(p2)
# Identify all unique levels from both sources
all_levels <- union(levels(as.factor(p2)), levels(as.factor(test$csMPa)))

# Re-factor both variables with these shared levels
p2_factored <- factor(p2, levels = all_levels)
reference_factored <- factor(test$csMPa, levels = all_levels)

# Run the confusion matrix again
confusionMatrix(p2_factored, reference_factored)


#RMSE
sqrt(mean((test$csMPa - p2)^2))
#MAE 
mean(abs(test$csMPa - p2))

#R^2
cor(test$csMPa, p2)^2

#Plot Actual vs Predicted
plot(train$csMPa, p2,
     xlab="Actual Strength",
     ylab="Predicted Strength",
     main="Actual vs Predicted Concrete Strength")
abline(0,1,col="red")

#error rate of random forest
plot(rf)

#the plot we can see the error drops down then after 300 trees the error is more 
# or less constant. 

#ture random forest model
t <- tuneRF(train[,-8], train[,8],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 100,
            trace=TRUE,
            improve = 0.05)


# After tuning the model the random forest should be call again
rf <-  randomForest(csMPa~.,data=train,
                    ntree=300, # 300 because after 300 not improving
                    mtry= 8, # here 8 comes from the plot
                    importance=TRUE, 
                    proximity = TRUE
)
rf

# no. of nodes for the tree
hist(treesize(rf),
     main = "No. of nodes for the tree",
     col="green" )



#variable importance
varImpPlot(rf, sort = TRUE,
           n.var = 10,
           main = "Importance of Variable Predicting the Results")

importance(rf)
varUsed(rf)

cor(data)
tuneRF(train[,-9], train$csMPa)

rf_opt <- randomForest(csMPa ~ ., data=train, mtry=8)


p_opt <- predict(rf_opt, test)

sqrt(mean((test$csMPa - p_opt)^2))
mean(abs(test$csMPa - p_opt))
cor(test$csMPa, p_opt)^2

plot(test$csMPa, p_opt,
     xlab="Actual Strength (MPa)",
     ylab="Predicted Strength (MPa)",
     main="Actual vs Predicted Concrete Strength",
     pch=19,
     col="blue")

abline(0,1,col="red",lwd=2)