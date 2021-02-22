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
    int<lower=0> N; // N games
    int<lower=0> P; // P teams
     
    // Each team is referred to by an integer that acts as an index for the ratings vector. 
    int team1[N]; // Indicator arrays for team 1
    int team2[N]; // Indicator arrays for team 1
    int results[N]; // Results. 1 if team 1 won, 0 if team 2 won.
     
    vector[P] alpha; // Parameters for Dirichlet prior.
}
 
parameters {
    // Vector of ratings for each team.
    // The simplex constrains the ratings to sum to 1 
    simplex[P] ratings;
}
 
model {
    real p1_win[N]; // Win probabilities for player 1
     
    ratings ~ dirichlet(alpha); // Dirichlet prior.
     
    for (i in 1:N){
        p1_win[i] = ratings[team1[i]] / (ratings[team1[i]] + ratings[team2[i]]);
        results[i] ~ bernoulli(p1_win[i]);
    }
}
