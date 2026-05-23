# =========================================================
# LDA ANALYSIS
# =========================================================

# =========================================================
# INSTALL PACKAGES
# =========================================================

install.packages("MASS")
install.packages("caret")

# =========================================================
# LOAD LIBRARIES
# =========================================================

library(MASS)
library(caret)

# =========================================================
# LOAD DATA
# =========================================================

setwd("G:/R Programming/ML/Predicting Concrete Strength/")

data <- read.csv(
  "G:/R Programming/ML/Predicting Concrete Strength/Raw Data Concrete_Data_Yeh.csv"
)

# =========================================================
# CREATE CLASS VARIABLE
# =========================================================

median_strength <- median(data$csMPa)

data$strength_class <- ifelse(
  data$csMPa >= median_strength,
  "High",
  "Low"
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
# TRAIN LDA MODEL
# =========================================================

lda_model <- lda(
  strength_class ~ cement + slag + flyash +
    water + superplasticizer +
    coarseaggregate + fineaggregate + age,
  
  data = train
)

lda_model

# =========================================================
# PREDICTION
# =========================================================

lda_pred <- predict(lda_model, test)

# =========================================================
# CONFUSION MATRIX
# =========================================================

confusionMatrix(
  lda_pred$class,
  test$strength_class
)