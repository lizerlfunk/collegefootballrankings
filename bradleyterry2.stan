//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//


data {
  int<lower = 0> K;                     // players
  int<lower = 0> N;                     // matches
  int<lower=1, upper = K> player1[N];   // player 1 for game n
  int<lower=1, upper = K> player0[N];   // player 0 for game n
  int<lower = 0, upper = 1> y[N];       // winner for match n
}
parameters {
  vector[K] alpha;                      // ability for player n
}
model {
  y ~ bernoulli_logit(alpha[player1] - alpha[player0]);
}

generated quantities {
  int<lower=1, upper=K> ranked[K] = sort_indices_desc(alpha);
}
