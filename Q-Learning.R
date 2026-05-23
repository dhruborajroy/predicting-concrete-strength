# =========================================================
# SIMPLE Q-LEARNING
# =========================================================

# =========================================================
# INITIALIZE Q TABLE
# =========================================================

states <- 1:5

actions <- c("increase_water",
             "decrease_water")

Q <- matrix(
  0,
  nrow = length(states),
  ncol = length(actions)
)

colnames(Q) <- actions

# =========================================================
# PARAMETERS
# =========================================================

alpha <- 0.1
gamma <- 0.9
epsilon <- 0.2

# =========================================================
# TRAINING
# =========================================================

for(i in 1:1000){
  
  state <- sample(states,1)
  
  if(runif(1) < epsilon){
    
    action <- sample(1:2,1)
    
  } else {
    
    action <- which.max(Q[state,])
  }
  
  next_state <- sample(states,1)
  
  reward <- sample(c(-1,1),1)
  
  Q[state,action] <-
    Q[state,action] +
    alpha * (
      reward +
        gamma * max(Q[next_state,]) -
        Q[state,action]
    )
}

# =========================================================
# FINAL Q TABLE
# =========================================================

print(Q)