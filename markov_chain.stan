data {
  int<lower=1> K; // Number of states
  int<lower=1> N; // Number of transitions (observations)
  int<lower=1, upper=K> prev_state[N]; // Previous state indices
  int<lower=1, upper=K> next_state[N]; // Next state indices
}

parameters {
  simplex[K] transition_matrix[K]; // Transition probabilities for each state
}

model {
  for (k in 1:K) {
    transition_matrix[k] ~ dirichlet(rep_vector(1.0, K)); // Dirichlet prior
  }
  
  for (n in 1:N) {
    next_state[n] ~ categorical(transition_matrix[prev_state[n]]);
  }
}
