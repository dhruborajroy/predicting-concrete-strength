# =========================================================
# DEEP Q NETWORK (DQN)
# =========================================================

# =========================================================
# INSTALL PACKAGES
# =========================================================

install.packages("keras")

# =========================================================
# LOAD LIBRARIES
# =========================================================

library(keras)

# =========================================================
# BUILD DQN MODEL
# =========================================================

model <- keras_model_sequential()

model %>%
  layer_dense(
    units = 64,
    activation = "relu",
    input_shape = c(8)
  ) %>%
  
  layer_dense(
    units = 64,
    activation = "relu"
  ) %>%
  
  layer_dense(
    units = 4,
    activation = "linear"
  )

# =========================================================
# COMPILE MODEL
# =========================================================

model %>% compile(
  loss = "mse",
  optimizer = optimizer_adam()
)

summary(model)