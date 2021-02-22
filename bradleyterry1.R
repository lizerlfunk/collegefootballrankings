library("BradleyTerry2")
data("citations", package = "BradleyTerry2")
citations
citations.sf <- countsToBinomial(citations)
names(citations.sf)[1:2] <- c("journal1", "journal2")
citations.sf
citeModel <- BTm(cbind(win1, win2), journal1, journal2, ~ journal, id = "journal", data = citations.sf)
citeModel

data("baseball", package = "BradleyTerry2")
head(baseball)

baseballModel1 <- BTm(cbind(home.wins, away.wins), home.team, away.team, data = baseball, id = "team")
summary(baseballModel1)

BTRank19 <- BTm(cbind(homewin,awaywin), home_team, away_team, data = games_19_massey, id = "team")
BTRank19
BTabilities(BTRank19)
update(BTRank19, br = TRUE)
BTabilities(BTRank19)

citation(package = "cfbscrapR")

model {
  for (i in 1:N) {
    Y[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha + beta * (x[i] - x.bar)
  }
  x.bar <- mean(x)
  alpha ~ dnorm(0.0, 1.0E-4)
  beta ~ dnorm(0.0, 1.0E-4)
  sigma <- 1.0/sqrt(tau)
  tau ~ dgamma(1.0E-3, 1.0E-3)
}
line_data <- list("x" = c(1, 2, 3, 4, 5), "Y" = c(1, 3, 3, 3, 5), "N"=5)
line_inits <- list(list("alpha" = 3, "beta"= 0, "tau" = 1.5),
                   list("alpha" = 0, "beta" = 1, "tau" = 0.375))
model <- jags.model("line.bug", data=line_data, inits=line_inits, n.chains=2)
