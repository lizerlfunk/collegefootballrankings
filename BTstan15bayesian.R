# map (-infinity, infinty) -> (0, 1)
inv_logit <- function(u) 1 / (1 + exp(-u))
# map vector to vector with sum of zero
center <- function(u) u - sum(u) / length(u)
# parameters
K <- 131 #number of teams

# observations
N <- nrow(games_15_BTstan)    # number of games in 2015
player1 <- games_15_BTstan$player1
player0 <- games_15_BTstan$player0
#for (n in 1:N) {
#  players <- sample(1:K, 2)
#  player0[n] <- players[1]  #randomly generates matchups between players
#  player1[n] <- players[2]  #unneeded--we have data
#}
log_odds_player1 <- alpha[player1] - alpha[player0] #log odds of ability differences 
prob_win_player1 <- inv_logit(log_odds_player1) #win probability based on win differences
y <- games_15_BTstan$y #which team wins, from data

#calling Stan model

mle_model_data <-
  list(K = K, N = N, player0 = player0, player1 = player1, y = y)
individual_model <- stan_model("bradleyterrybayesian.stan")
#individual_posterior <- sampling(individual_model, data = mle_model_data)

individual_posterior <-  sampling(individual_model, data = mle_model_data, pars = NA, 
         chains = 4, iter = 4000, warmup = 1000, diagnostic_file = "BTbayes15diagnostics.csv", verbose = FALSE, 
         algorithm = c("NUTS", "HMC", "Fixed_param"),
         control = NULL, include = TRUE, 
         cores = getOption("mc.cores", 1L),
         open_progress = interactive() && !isatty(stdout()) &&
           !identical(Sys.getenv("RSTUDIO"), "1"),
         show_messages = TRUE)

sink(file = "BTbayes15.csv")
summary(individual_posterior, "alpha", probs = c(0.05,0.5,0.95))$summary
sink(file = NULL)

sink(file = "BTbayes15-2.csv")
summary(individual_posterior, pars = c("alpha[126]", "alpha[127]", "alpha[128]", "alpha[129]", "alpha[130]", "alpha[131]"), probs = c(0.05,0.5,0.95))$summary
sink(file = NULL)


#indiv_post_ss <- as.data.frame(individual_posterior)

sink(file = "BTbayesranking15.csv")
summary(individual_posterior, "ranking",
      probs = c(0.05, 0.5, 0.95),
      digits_summary = 2, include = TRUE)$summary
sink(file = NULL)

sink(file = "BTbayesranking15-2.csv")
summary(individual_posterior, pars = c("ranking[126]", "ranking[127]", "ranking[128]", "ranking[129]", "ranking[130]", "ranking[131]"),
        probs = c(0.05, 0.5, 0.95),
        digits_summary = 2, include = TRUE)$summary
sink(file = NULL)


