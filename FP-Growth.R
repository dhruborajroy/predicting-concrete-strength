# =========================================================
# FP-GROWTH
# =========================================================

# =========================================================
# INSTALL PACKAGES
# =========================================================

install.packages("arules")

# =========================================================
# LOAD LIBRARIES
# =========================================================

library(arules)

# =========================================================
# LOAD DATA
# =========================================================

setwd("G:/R Programming/ML/Predicting Concrete Strength/")

data <- read.csv(
  "G:/R Programming/ML/Predicting Concrete Strength/Raw Data Concrete_Data_Yeh.csv"
)

# =========================================================
# DISCRETIZATION
# =========================================================

data_disc <- discretizeDF.supervised(
  data,
  
  classColumn = 9
)

trans <- as(
  data_disc,
  "transactions"
)

# =========================================================
# FP-GROWTH MODEL
# =========================================================

rules_fp <- apriori(
  trans,
  
  parameter = list(
    supp = 0.1,
    conf = 0.8
  ),
  
  control = list(
    verbose = TRUE
  )
)

inspect(sort(
  rules_fp,
  by = "lift"
)[1:10])