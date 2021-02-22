# map (-infinity, infinty) -> (0, 1)
inv_logit <- function(u) 1 / (1 + exp(-u))
# map vector to vector with sum of zero
center <- function(u) u - sum(u) / length(u)
# parameters
K <- nrow(records_19) #number of teams
alpha <- center(rnorm(K))

# observations
N <- nrow(games_19_BTstan)    # number of games in 2019
player1 <- games_19_BTstan$player1
player0 <- games_19_BTstan$player0
#for (n in 1:N) {
#  players <- sample(1:K, 2)
#  player0[n] <- players[1]  #randomly generates matchups between players
#  player1[n] <- players[2]  #unneeded--we have data
#}
log_odds_player1 <- alpha[player1] - alpha[player0] #log odds of ability differences 
prob_win_player1 <- inv_logit(log_odds_player1) #win probability based on win differences
y <- games_19_BTstan$y #which team wins, from data

#calling Stan model

mle_model_data <-
  list(K = K, N = N, player0 = player0, player1 = player1, y = y)
mle_model <-
  stan_model("bradleyterry2.stan")
mle_model_estimates <-
  optimizing(mle_model, data = mle_model_data)


alpha_star <- mle_model_estimates$par[paste("alpha[", 1:K, "]", sep="")]
mle_fit_plot <-
  ggplot(data.frame(alpha = alpha, alpha_star = alpha_star),
         aes(x = alpha, y = alpha_star)) +
  geom_abline(slope = 1, intercept = 0, color="green", size = 2) +
  geom_point(size = 2)
mle_fit_plot

ranked_players <-
  mle_model_estimates$par[paste("ranked[", 1:K, "]",
                                sep="")]
print(ranked_players, digits=0)

ranked_players_19 <- as.data.frame(ranked_players)

ranked_players_19$ranked_players <- as.character(ranked_players_19$ranked_players)

write.csv(ranked_players_19, "BTrankings19.csv")
