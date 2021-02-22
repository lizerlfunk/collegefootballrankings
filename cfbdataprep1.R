pbp_2019 <- data.frame()
seasons <- 2019
pbp_2019 <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/saiemgilani/cfbscrapR-data/master/data/rds/pbp_players_pos_{x}.rds")
    )
  )
})
#Game level data
games_19 <- cfb_game_info(2019)

#Join Games and Play-by-Play
plays19 <- left_join(pbp_2019, games_19, by = c("game_id" = "game_id"))

cfb_game_info(
  2019,
  week = NULL,
  season_type = "regular",
  team = NULL,
  home_team = NULL,
  away_team = NULL,
  conference = NULL,
  game_id = NULL,
  quarter_scores = FALSE
)

cfb_rankings(2019, week = 11, season_type = "regular")

pbp_2018 <- data.frame()
seasons <- 2018
pbp_2018 <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/saiemgilani/cfbscrapR-data/master/data/rds/pbp_players_pos_{x}.rds")
    )
  )
})

games_18 <- cfb_game_info(2018)

#Join Games and Play-by-Play
plays18 <- left_join(pbp_2018, games_18, by = c("game_id" = "game_id"))

cfb_game_team_stats(
  2019,
  week = 1,
  season_type = "regular",
  team = NULL,
  conference = NULL,
  game_id = NULL,
  rows_per_team = 1
)

games_18 <- games_18 %>% filter(!is.na(home_conference) & !is.na(away_conference))
games_19 <- games_19 %>% filter(!is.na(home_conference) & !is.na(away_conference))

games_19_BTstan <- games_19

rankings_19 <- cfb_rankings(2019, week = NULL, season_type = "regular")
view(rankings_19)
rankings_19 <- rankings_19 %>% filter(poll == "Playoff Committee Rankings")

records_19 <- cfb_game_records(2019, team = NULL, conference = NULL)
view(records_19)

pbp_2017 <- data.frame()
seasons <- 2017
pbp_2017 <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/saiemgilani/cfbscrapR-data/master/data/rds/pbp_players_pos_{x}.rds")
    )
  )
})

games_17 <- cfb_game_info(2017)

pbp_2016 <- data.frame()
seasons <- 2016
pbp_2016 <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/saiemgilani/cfbscrapR-data/master/data/rds/pbp_players_pos_{x}.rds")
    )
  )
})

#scrape game info
games_16 <- cfb_game_info(2016)
games_15 <- cfb_game_info(2015)
games_14 <- cfb_game_info(2014)


#filter out non-FBS games
games_17 <- games_17 %>% filter(!is.na(home_conference) & !is.na(away_conference))
games_16 <- games_16 %>% filter(!is.na(home_conference) & !is.na(away_conference))
games_15 <- games_15 %>% filter(!is.na(home_conference) & !is.na(away_conference))
games_14 <- games_14 %>% filter(!is.na(home_conference) & !is.na(away_conference))

#scrape record info
records_18 <- cfb_game_records(2018, team = NULL, conference = NULL)
records_17 <- cfb_game_records(2017, team = NULL, conference = NULL)
records_16 <- cfb_game_records(2016, team = NULL, conference = NULL)
records_15 <- cfb_game_records(2015, team = NULL, conference = NULL)
records_14 <- cfb_game_records(2014, team = NULL, conference = NULL)

#histogram of total wins by team
wins2019 <- records_19$total_wins
hist(wins2019)
wins2018 <- records_18$total_wins
hist(wins2018)
wins2017 <- records_17$total_wins
hist(wins2017)
wins2016 <- records_16$total_wins
hist(wins2016)
wins2015 <- records_15$total_wins
hist(wins2015)
wins2014 <- records_14$total_wins
hist(wins2014)

#descriptive statistics total wins by team
descdist(wins2019)
descdist(wins2018)
descdist(wins2017)
descdist(wins2016)
descdist(wins2015)
descdist(wins2014)

fit1 <- fitdist(wins2019, 'beta')

descdist(games_19$home_points)
descdist(games_19$away_points)

games_19<- subset(games_19, select = -c(season_type,start_time_tbd))
games_19<- subset(games_19, select = -c(game_id,neutral_site,conference_game,attendance,venue_id,venue,home_id,away_id,home_post_win_prob,away_post_win_prob,excitement_index))
games_19<- subset(games_19, select = -c(home_conference,away_conference))
games_19<- subset(games_19, select = -c(start_date))
games_19 <- games_19 %>% mutate(winner = 0)
games_19$winner <- ifelse(games_19$home_points > games_19$away_points, games_19$home_team, games_19$away_team)
games_19 <- games_19 %>% mutate(homewin = 0)
games_19 <- games_19 %>% mutate(awaywin = 0)
games_19$homewin <- ifelse(games_19$home_points > games_19$away_points, 1, 0)
games_19$awaywin <- ifelse(games_19$home_points > games_19$away_points, 0, 1)

games_18<- subset(games_18, select = -c(season_type,start_time_tbd))
games_18<- subset(games_18, select = -c(game_id,neutral_site,conference_game,attendance,venue_id,venue,home_id,away_id,home_post_win_prob,away_post_win_prob,excitement_index))
games_18<- subset(games_18, select = -c(start_date))
games_18 <- games_18 %>% mutate(winner = 0)
games_18$winner <- ifelse(games_18$home_points > games_18$away_points, games_18$home_team, games_18$away_team)
games_18 <- games_18 %>% mutate(homewin = 0)
games_18 <- games_18 %>% mutate(awaywin = 0)
games_18$homewin <- ifelse(games_18$home_points > games_18$away_points, 1, 0)
games_18$awaywin <- ifelse(games_18$home_points > games_18$away_points, 0, 1)

games_19_massey<- subset(games_19, select = -c(season,week))
games_19_massey <- games_19_massey[,c(1,3,2,4,5,6,7)]
games_19_massey<- subset(games_19_massey, select = -c(home_points, away_points, winner))
games_19_massey$home_team <- as.factor(games_19_massey$home_team)
games_19_massey$away_team <- as.factor(games_19_massey$away_team)

games_18_ACC<- subset(games_18_ACC, select = -c(season,week))
games_18_ACC<- subset(games_18_ACC, select = -c(home_conference,away_conference))
games_18_ACC <- games_18_ACC[,c(1,3,2,4,5)]

rankings_18 <- cfb_rankings(2018, week = NULL, season_type = "regular")
rankings_18 <- rankings_18 %>% filter(poll == "Playoff Committee Rankings")
write.csv(rankings_18, "CFPrankings2018.csv")  

rankings_17 <- cfb_rankings(2017, week = NULL, season_type = "regular")
rankings_17 <- rankings_17 %>% filter(poll == "Playoff Committee Rankings")
write.csv(rankings_17, "CFPrankings2017.csv")

rankings_16 <- cfb_rankings(2016, week = NULL, season_type = "regular")
rankings_16 <- rankings_16 %>% filter(poll == "Playoff Committee Rankings")
write.csv(rankings_16, "CFPrankings2016.csv")

rankings_15 <- cfb_rankings(2015, week = NULL, season_type = "regular")
rankings_15 <- rankings_15 %>% filter(poll == "Playoff Committee Rankings")
write.csv(rankings_15, "CFPrankings2015.csv")

rankings_14 <- cfb_rankings(2014, week = NULL, season_type = "regular")
rankings_14 <- rankings_14 %>% filter(poll == "Playoff Committee Rankings")
write.csv(rankings_14, "CFPrankings2014.csv")  
