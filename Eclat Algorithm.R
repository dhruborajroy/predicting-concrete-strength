# =========================================================
# ECLAT ALGORITHM
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
# ECLAT MODEL
# =========================================================

eclat_rules <- eclat(
  trans,
  
  parameter = list(
    supp = 0.1,
    maxlen = 5
  )
)

inspect(sort(
  eclat_rules,
  by = "support"
)[1:10])