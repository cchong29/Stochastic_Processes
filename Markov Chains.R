# Change to dark mode for graphs
par(col.axis = 'white', col.lab = 'white',col.main = 'white',col.sub = 'white', fg = 'black')

rm(list = ls()) # Removes all objects from workspace


# Write a function that simulates a single trajectory for a Markov Chain.
# Arguments: the initial state, tpm Gamma, and number of steps to simulate

MC_simulate = function(x0, Gamma, steps){
  # Create a vector that enumerates the state space
  # Reads off first dimension of that square matrix
  U = 1:dim(Gamma)[1]
  
  # Create storage for simulated trajectory
  # Repeat steps + 1 times
  X = rep(NA, steps + 1)
  
  # Initial State
  X[1] = x0
  for (i in 1:steps){
    # Next value of the process is drawn from discrete distribution 
    # Current state of the process is X[i]
    X[i+1] = sample(U,1,prob = Gamma[X[i],])
  }
  
  # Function returns the randomly simulated trajectory
  return(list(X=X, time = 0:steps))
}

# Try out an arbitrary tpm (transition probability matrix):
# Define a 3 state markov chain
# Define probability of moving from state 1 back to state 1 over 1 step to be 0.5
# Probabilities should sum up to 1 for each row 
Gamma = rbind(c(0.5,0.5,0), 
              c(0, 0.25, 0.75),
              c(0.75, 0.00, 0.25))

# Trajectory of the process is deterministic if the probability of 
# moving to one state from state I is 1 for all possible states I

# Simulate and plot 1000 trajectories of length 11 for the above
# starting from initial state 1

# Pop out a window
quartz()
for(i in 1:1000)
{# Plot a single trajectory
res = MC_simulate(1,Gamma,10)

# Plot X as a function of the time index
# type of plot = both
plot(res$X~res$time, type = 'b', ylim = c(0,4))
}

# Comments on the plot:
# At state 1, probability of moving to state 1 or 2 is 0.5.
# At state 2, probability of moving to state 3 is 0.75.
# y-axis is the state of the process