####################################### NBA THREE POINTERS AND TV VIEWERSHIP ####################################### 

library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(stringr)
library(hoopR)

############################################ 01. DATA PREP ############################################ 

# Read in TV viewership data, retrieved from SportsMediaWatch.com & Statista
tv <- read.csv('viewership_by_season.csv') %>% rename(season = season_start)
head(tv)

# Get team box score stats for each game in each season since 2002 
data <- load_nba_team_box(seasons = 2002:2025) %>%
  filter(season_type == 2) %>%
  mutate(season = season - 1) %>%
  select(game_id, season, game_date, team_location, team_name, team_abbreviation, team_display_name, team_score, 
         field_goals_made, field_goals_attempted, field_goal_pct, free_throws_made, free_throws_attempted, free_throw_pct, 
         three_point_field_goals_made, three_point_field_goals_attempted, three_point_field_goal_pct,
         flagrant_fouls, fouls, total_technical_fouls)

# Filter out all-star teams/games, etc.
data <- data %>%
  filter(team_name %in% c("76ers","Bucks","Bulls","Cavaliers","Celtics","Clippers","Grizzlies","Hawks","Heat",
                          "Hornets","Jazz","Kings","Knicks","Lakers","Magic","Mavericks","Nets","Nuggets","Pacers",
                          "Pelicans","Pistons","Raptors","Rockets","Spurs","Suns","Thunder","Timberwolves",
                          "Trail Blazers","Warriors","Wizards","SuperSonics","Bobcats"))

head(data)

# Quick checks 
n_distinct(data$game_id)
data %>% group_by(season) %>% summarise(teams = n_distinct(team_abbreviation)) %>% tail()
sum(is.na(data))
data %>% group_by(game_id) %>% summarise(teams = n_distinct(team_name)) %>% arrange(-teams)

# Compute game-level stats 
game <- data %>%
  group_by(game_id, season) %>%
  summarise(
    total_points = sum(team_score),
    point_differential = max(team_score) - min(team_score),
    fgm = sum(field_goals_made),
    fga = sum(field_goals_attempted),
    fg_pct = fgm/fga,
    ftm = sum(free_throws_made),
    fta = sum(free_throws_attempted),
    ft_pct = ftm/fta,
    fgm3 = sum(three_point_field_goals_made),
    fga3 = sum(three_point_field_goals_attempted),
    fg3_pct = fgm3/fga3,
    fouls = sum(fouls),
    flagrants = sum(flagrant_fouls),
    techs = sum(total_technical_fouls),
    fga3_pct_of_total_fga = fga3/fga,
    fgm3_pct_of_total_fgm = fgm3/fgm
  ) %>%
  ungroup()

head(game)

# Compute team-level stats
teams <- data %>% 
  group_by(team_display_name, season) %>%
  summarize(
    games = n_distinct(game_id),
    avg_points = mean(team_score),
    avg_fgm = mean(field_goals_made),
    avg_fga = mean(field_goals_attempted),
    fg_pct = avg_fgm/avg_fga,
    avg_ftm = mean(free_throws_made),
    avg_fta = mean(free_throws_attempted),
    ft_pct = avg_ftm/avg_fta,
    avg_fg3m = mean(three_point_field_goals_made),
    avg_fg3a = mean(three_point_field_goals_attempted),
    fg3_pct = avg_fg3m/avg_fg3a,
    fg3a_pct_fga = avg_fg3a/avg_fga, 
    avg_flagrants = mean(flagrant_fouls),
    avg_techs = mean(total_technical_fouls),
    avg_fouls = mean(fouls)
  ) %>%
  ungroup() %>%
  rename(team = team_display_name)

head(teams)

# Read in team shots by location
shots <- read.csv('team_shots_by_distance.csv')
head(shots)

# Join to shots data
teams <- teams %>%
  left_join(shots, by=c("season","team")) %>%
  rename(fgm_under_5 = under_5_ft_fgm, fga_under_5 = under_5_ft_fga,
         fgm_5_9 = X_5to9_fgm, fga_5_9 = X_5to9_fga, 
         fgm_10_14 = X_10to14_fgm, fga_10_14 = X_10to14_fga,
         fgm_15_19 = X_15to19_fgm, fga_15_19 = X_15to19_fga,
         fgm_20_24 = X_20to24_fgm, fga_20_24 = X_20to24_fga,
         fgm_25_29 = X_25to29_fgm, fga_25_29 = X_25to29_fga) %>%
  mutate(
    fgm_over_29 = avg_fgm - fgm_under_5 - fgm_5_9 - fgm_10_14 - fgm_15_19 - fgm_20_24 - fgm_25_29,
    fga_over_29 = avg_fga - fga_under_5 - fga_5_9 - fga_10_14 - fga_15_19 - fga_20_24 - fga_25_29,
    fga_pct_under_5 = fga_5_9/avg_fga,
    fga_pct_5_9 = fga_5_9/avg_fga,
    fga_pct_10_14 = fga_10_14/avg_fga,
    fga_pct_15_19 = fga_15_19/avg_fga,
    fga_pct_20_24 = fga_20_24/avg_fga,
    fga_pct_25_29 = fga_25_29/avg_fga,
    fga_pct_over_29 = fga_over_29/avg_fga)

head(teams)

# Read in team shots by distance 
shot_dist <- read.csv('FGA_by_range.csv')
head(shot_dist) 

# Compute proportion of total FGA from each distance for each team/season 
shot_props_all <- shot_dist %>%
  mutate(avg_total_fga = short_range_fga + mid_range_fga + long_range_fga + three_range_fga,
         short_range_prop = short_range_fga / avg_total_fga,
         mid_range_prop = mid_range_fga / avg_total_fga,
         long_range_prop = long_range_fga / avg_total_fga,
         three_range_prop = three_range_fga / avg_total_fga) %>%
  select(season, team, avg_total_fga, short_range_prop, mid_range_prop, long_range_prop, three_range_prop)

head(shot_props_all)  

# Aggregate by season
shot_props_season <- shot_props_all %>%
  group_by(season) %>%
  summarise(mean_short_range_prop = mean(short_range_prop),
            mean_mid_range_prop = mean(mid_range_prop),
            mean_long_range_prop = mean(long_range_prop),
            mean_three_range_prop = mean(three_range_prop),
            min_three_range_prop = min(three_range_prop),
            max_three_range_prop = max(three_range_prop),
            range_three_range_prop = max_three_range_prop - min_three_range_prop)

tail(shot_props_season)
