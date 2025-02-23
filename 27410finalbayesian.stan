data {
  int<lower=1> T;      // Number of observations (using 20)
  vector[T] y;         // Log returns of past 20 observations
}
parameters {
  real phi;            // AR(1) coefficient - can get from training set
  real<lower=0> sigma; // Noise (volatility)
}
model {
  // Priors for the parameters
  phi ~ normal(0, 0.5); # this will depend on training set
  sigma ~ normal(0, 2.5) T[0,];#this will also depend on training set

  // Likelihood (using the past 20 observations)
  // First observation: assume it comes from the stationary distribution
  y[1] ~ normal(0, sigma / sqrt(1 - square(phi)));
  
  // for the next 20 observations
  for (t in 2:T)
    y[t] ~ normal(phi * y[t-1], sigma);
}
generated quantities {
  // using the 20th observation (last time T) to forecast the next draw
  real y_next;
  y_next = normal_rng(phi * y[T], sigma);
}
