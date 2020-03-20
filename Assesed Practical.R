set.seed(1832020)

# Produces a transition matrix for a Markov chain based on a parameter eps
TransMat <- function(eps) {
  matrix(c(0.1, 0.9,   0,       0,       0,   0,
           0.4, 0.2, 0.4,       0,       0,   0,
           0, 0.5,   0,     0.5,       0,   0,
           0,   0, 0.5, 0.5-eps,     eps,   0,
           0,   0,   0,     eps, 0.3-eps, 0.7,
           0,   0,   0,       0,     0.5, 0.5),
         nrow = 6, ncol = 6, byrow = TRUE)
}


# Simulates a Markov chain with transition matrix P for N steps from X0
MarkovChain <- function(N, X0, P) {
  X <- numeric(N)   # empty vector of length N
  space <- nrow(P)  # number of points in sample space
  
  now <- X0
  for (n in 1:N) {
    now <- sample(space, 1, prob = P[now, ])
    X[n] <- now
  }
  
  X
}


# Finds the stationary distribution of a Markov chain
StatDist <- function(P) {
  LeftEigen <- eigen(t(P))
  statmeas <- LeftEigen$vectors[, 1]  # Unnormalised stationary measure
  statmeas / sum(statmeas)            # Normalised stationary distribution
}
#################################################################################
#Question 1
epsilon <- 0.2

transition_matrix <- TransMat(epsilon)
markov_chain <- MarkovChain(N = 50, X0 = 1, P = transition_matrix)
plot(0:length(markov_chain), c(1, markov_chain), type = "b", main = "Markov Chain (N=50, X0 = 1)", xlab="n", ylab="Xn")


#Question 2
#Function to calculate proportion of time in each state.
prop_time <- function(m_chain) {
  return(table(factor(m_chain, 1:6))/length(m_chain))
}

#Find proportion of time spent in each state for the Markov chain markov_chain. Print the result and plot it on a bar graph.
proportion_time <- prop_time(markov_chain)
print(proportion_time)
barplot(proportion_time, main = "Fraction of Time Spent in Each State",
        beside = TRUE, xlab = "State", ylab = "Proportion of Time")

#Generate the stationary distribution for the Markov chain specified by the transition matrix with eps = 0.2.
stat_dist <- StatDist(transition_matrix)

#Generate an array of numbers where each row corresponds to the proportion of time spent in each state by a Markov cahin with
#N steps for N in {5, 50, 500, 5000, 50000}. Also add the final row to be the stationary distribution.
chain_vector <- rbind(prop_time(MarkovChain(5, 1, transition_matrix)), prop_time(MarkovChain(50, 1, transition_matrix)), 
                  prop_time(MarkovChain(500, 1, transition_matrix)), prop_time(MarkovChain(5000, 1, transition_matrix)),
                  prop_time(MarkovChain(50000, 1, transition_matrix)), stat_dist)

#Produce a barplot of the fraction of time spent in each state from the rows above (inc. the stat dist)
barplot(chain_vector, main = "Fraction of Time Spent in Each State", legend = c("5", "50", "500", "5,000", "50,000", "Stationary"), 
        xlab = "State",  ylab = "Proportion of Time",  beside = TRUE, col = c("blue", "red", "green", "yellow", "pink", "black"))


#Question 3
#Generate a 2x6 matrix containing 0s. Each column will store the return time to the corresponding state.
asymtotic_chain <- MarkovChain(5000, 1, transition_matrix)
return_times_matrix <- matrix(data = 0, nrow = 2, ncol = 6)

#Iterate through all states of the Markov chain and calculate the mean return time for the Markov chain in part (a) 
#and for one with N = 5000 steps
for(state in 1:6) {
  return_times_matrix[1, state] <- mean(diff(which(markov_chain == state)))
  return_times_matrix[2, state] <- mean(diff(which(asymtotic_chain == state)))
}

#Extract the completed rows from the matrix and display the results
empirical_return_time <- return_times_matrix[1, ]
asymtotic_return_time <- return_times_matrix[2, ]
print(empirical_return_time)
print(asymtotic_return_time)

#Calculate the inverse stat_dist and display the results. 
expected_return_time <- 1/stat_dist
print(expected_return_time)

#Plot a barchart of the empirical results against the expected ones.
barplot(rbind(empirical_return_time, asymtotic_return_time , expected_return_time), 
        names.arg = 1:6, beside = TRUE, legend.text = TRUE, col = c("blue", "red", "green"),
        main = "Return Time to each State", xlab = "State", ylab = "Return Time", )