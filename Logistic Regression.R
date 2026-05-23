# =========================================================
# LOGISTIC REGRESSION
# Concrete Strength Classification
# =========================================================

# =========================================================
# INSTALL PACKAGES
# =========================================================

install.packages("caret")

# =========================================================
# LOAD LIBRARIES
# =========================================================

library(caret)

# =========================================================
# LOAD DATASET
# =========================================================

setwd("G:/R Programming/ML/Predicting Concrete Strength/")

data <- read.csv(
  "G:/R Programming/ML/Predicting Concrete Strength/Raw Data Concrete_Data_Yeh.csv"
)

head(data)

# =========================================================
# CREATE CLASSIFICATION TARGET
# =========================================================

# High Strength = 1
# Low Strength = 0

median_strength <- median(data$csMPa)

data$strength_class <- ifelse(
  data$csMPa >= median_strength,
  1,
  0
)

data$strength_class <- as.factor(data$strength_class)

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
# TRAIN LOGISTIC MODEL
# =========================================================

log_model <- glm(
  strength_class ~ cement + slag + flyash +
    water + superplasticizer +
    coarseaggregate + fineaggregate + age,
  
  data = train,
  
  family = "binomial"
)

summary(log_model)

# =========================================================
# PREDICTION
# =========================================================

prob_pred <- predict(
  log_model,
  test,
  type = "response"
)

class_pred <- ifelse(prob_pred > 0.5,1,0)

# =========================================================
# CONFUSION MATRIX
# =========================================================

confusionMatrix(
  as.factor(class_pred),
  test$strength_class
)