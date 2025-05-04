####################################### NBA THREE POINTERS AND TV VIEWERSHIP ####################################### 

library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggtext)
library(lubridate)
library(stringr)
library(hoopR)

source('01_Data_Prep.R')

############################################ 02. GAME-LEVEL EDA ############################################ 

# Aggregate per-game stats by season and join viewership data
game_df <- game %>% 
  group_by(season) %>% 
  summarise(
    avg_total_points = mean(total_points),
    avg_fgm = mean(fgm),
    avg_fga = mean(fga),
    avg_fg_pct = mean(fg_pct),
    avg_fg3m = mean(fgm3),
    avg_fg3a = mean(fga3),
    avg_3fg_pct = mean(fg3_pct),
    avg_3fg_pct_all_fga = mean(fga3_pct_of_total_fga)
    ) %>% 
  ungroup() %>%
  left_join(tv, by="season") %>%
  filter(season >= 2002 & season < 2025)

head(game_df)

# Plot relationship between avg viewship and avg 3 point attempts per game by season 
ggplot(game_df, aes(x = season)) +
  geom_line(aes(y = avg_viewship, color = "Avg Viewership"), size = 1) + 
  geom_point(aes(y = avg_viewship, color = "Avg Viewership"), size = 2) +
  geom_line(aes(y = avg_fg3a * 0.04, color = "Avg 3PT Attempts"), size = 1) + 
  geom_point(aes(y = avg_fg3a * 0.04, color = "Avg 3PT Attempts"), size = 2) +
  scale_y_continuous(
    name = "Average Viewership (millions)",
    breaks = seq(1, 4, by = 0.5),
    sec.axis = sec_axis(~ . / 0.04, name = "Average 3PT Attempts per Game")
  ) +
  scale_x_continuous(
    breaks = seq(min(game_df$season), max(game_df$season), by = 2)
  ) +
  scale_color_manual(
    values = c("Avg Viewership" = "#E47041", "Avg 3PT Attempts" = "#163c83"),
    name = "Metric"
  ) +
  labs(
    title = "NBA Game Average <span style='color:#E47041;'>TV Viewership</span> and<br><span style='color:#163c83;'>Three-Point Attempts</span> by Season",
    x = "Season"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Verdana"), 
    plot.title = element_markdown(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 0.5),
    legend.position = "none",
    legend.title = element_blank()
  )

# Scatter plot of viewership and 3PA
ggplot(game_df, aes(x = avg_fg3a, y = avg_viewship)) +
  geom_point(color = "#E47041", size = 3, alpha = 0.9) +  
  labs(
    title = "NBA Viewership vs. Average Three-Point \nAttempts Per Game",
    x = "Average 3PT Attempts Per Game",
    y = "Average Viewership (millions)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Verdana"), 
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none",
    legend.title = element_blank()
  )

# Plot relationship between avg viewship and avg 3 point attempts as a pct of total fga per game by season 
ggplot(game_df, aes(x = season)) +
  geom_line(aes(y = avg_viewship, color = "Avg Viewership"), size = 1) + 
  geom_point(aes(y = avg_viewship, color = "Avg Viewership"), size = 2) +
  geom_line(aes(y = avg_3fg_pct_all_fga * 6, color = "Avg 3PT FGA Proportion"), size = 1) + 
  geom_point(aes(y = avg_3fg_pct_all_fga * 6, color = "Avg 3PT FGA Proportion"), size = 2) +
  scale_y_continuous(
    name = "Average Viewership (millions)",
    sec.axis = sec_axis(~ . / 6, name = "Average Proportion of 3PA to All FGA", breaks = seq(0, 1, by = 0.1))  
  ) +
  scale_x_continuous(
    breaks = seq(min(game_df$season), max(game_df$season), by = 2)
  ) +
  scale_color_manual(
    values = c("Avg Viewership" = "#E47041", "Avg 3PT FGA Proportion" = "#163c83"),
    name = "Metric"
  ) +
  labs(
    title = "NBA Game Average <span style='color:#E47041;'>TV Viewership</span> and<br><span style='color:#163c83;'>Three-Point Attempt Porportion</span> by Season",
    x = "Season"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Verdana"), 
    plot.title = element_markdown(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 0.5),
    legend.position = "none",
    legend.title = element_blank()
  )

# Scatter plot of viewership and 3PA
ggplot(game_df, aes(x = avg_3fg_pct_all_fga, y = avg_viewship)) +
  geom_point(color = "#E47041", size = 3, alpha = 0.9) +  
  labs(
    title = "NBA Viewership vs. Average Proportion\n of 3PA Per Game",
    x = "Average 3PT Attempts Per Game",
    y = "Average Viewership (millions)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Verdana"), 
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "none"
  )

# Compute YoY growth in 3PA, 3PA Prop.
game_df <- game_df %>%
  arrange(season) %>%
  mutate(
    lag_3PA = lag(avg_fg3a),
    lag_3PAP = lag(avg_3fg_pct_all_fga),
    yoy_growth_3PA = (avg_fg3a - lag_3PA)/lag_3PA,
    yoy_growth_3PAPP_pct_pts = (avg_3fg_pct_all_fga - lag_3PAP)
  ) %>%
  select(-lag_3PA, -lag_3PAP)


head(game_df)

# Plot YoY growth in 3PA 
ggplot(game_df %>% filter(season > 2002), aes(x = season)) +
  geom_col(aes(y = avg_fg3a, fill = "Avg 3PT Attempts"), alpha = 0.4) +
  geom_line(aes(y = yoy_growth_3PA * 100, color = "YoY Growth in 3PA"), size = 0.8) +  
  geom_point(aes(y = yoy_growth_3PA * 100, color = "YoY Growth in 3PA"), size = 2) +
  geom_text(aes(y = yoy_growth_3PA * 100, label = scales::percent(yoy_growth_3PA, accuracy = 1.0)),  
            vjust = -1.5, size = 3, color = "black", fontface = "bold",  
            angle = 15, hjust = 0.5, family = "Verdana") +
  scale_y_continuous(
    name = "Average 3PT Attempts",
    sec.axis = sec_axis(~ . / 100, name = "YoY Growth in 3PA (%)")
  ) +
  scale_x_continuous(
    breaks = seq(min(game_df$season), max(game_df$season), by = 2)
  ) +
  scale_fill_manual(values = c("Avg 3PT Attempts" = "#163c83"), name = "Metric") +  
  scale_color_manual(values = c("YoY Growth in 3PA" = "#E47041"), name = "Metric") +  
  labs(
    title = "NBA Average 3PT Attempts and Year-over-Year Growth",
    x = "Season"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Verdana"), 
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),  
    legend.position = "top",
    legend.title = element_blank()  
  )

# Plot YoY growth in 3PA Proportion
ggplot(game_df %>% filter(season > 2002), aes(x = season)) +
  geom_col(aes(y = avg_3fg_pct_all_fga, fill = "Avg 3PA Proportion of Total FGA"), alpha = 0.4) +
  geom_line(aes(y = yoy_growth_3PAPP_pct_pts, color = "YoY Growth in 3PA Proportion"), size = 0.8) +  
  geom_point(aes(y = yoy_growth_3PAPP_pct_pts, color = "YoY Growth in 3PA Proportion"), size = 2) +
  geom_text(aes(y = yoy_growth_3PAPP_pct_pts, 
                label = scales::percent(yoy_growth_3PAPP_pct_pts, accuracy = 1.0)),  
            vjust = -1.5, size = 3, color = "black", fontface = "bold",  
            angle = 15, hjust = 0.5, family = "Verdana") +
  scale_y_continuous(
    name = "3PT Attempts as % of Total FGA",
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_x_continuous(
    breaks = seq(min(game_df$season), max(game_df$season), by = 2)
  ) +
  scale_fill_manual(values = c("Avg 3PA Proportion of Total FGA" = "#163c83"), name = "Metric") +  
  scale_color_manual(values = c("YoY Growth in 3PA Proportion" = "#E47041"), name = "Metric") +  
  labs(
    title = "NBA Average 3PT Proportion of Total FGA\n and Year-over-Year Growth",
    x = "Season"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Verdana"), 
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),  
    legend.position = "top",
    legend.title = element_blank()
  )

# Are players missing more shots as they take more threes?
head(game_df)

ggplot(game_df, aes(x = season)) +
  geom_line(aes(y = avg_fg_pct * 100, color = "Avg FG%"), size = 1, color = '#E47041') +  
  geom_point(aes(y = avg_fg_pct * 100, color = "Avg FG%"), size = 2, color = '#E47041') +  
  scale_y_continuous(name = "Overall FG%", breaks = seq(40, 50, by = 1)) +
  scale_x_continuous(
    breaks = seq(min(game_df$season), max(game_df$season), by = 2) 
  ) +
  labs(
    title = "NBA Game Average Field Goal Percentage Over Time",
    x = "Season"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Verdana"), 
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.title = element_blank()
  )

# Are players missing more shots - number of missed shots 
game_df <- game_df %>% mutate(missed_fgs = avg_fga - avg_fgm)

game_long <- game_df %>%
  pivot_longer(cols = c(missed_fgs, avg_fga), names_to = "stat", values_to = "value")

ggplot(game_long, aes(x = season, y = value)) +
  geom_line(aes(color = stat), size = 1) +
  geom_point(aes(color = stat), size = 2) +
  facet_grid(rows = vars(stat), scales = "free_y", switch = "y") +
  scale_x_continuous(
    breaks = seq(min(game_df$season), max(game_df$season), by = 2)
  ) +
  scale_color_manual(
    values = c(
      "missed_fgs" = "#163c83",
      "avg_fga" = "#E47041"
    ),
    labels = c(
      "missed_fgs" = "Missed FGs",
      "avg_fga" = "Attempted FGs"
    )
  ) +
  labs(
    title = "NBA Game Average <span style='color:#E47041;'>Shot Attempts</span> and<br><span style='color:#163c83;'>Missed Shots</span> by Season",
    x = "Season",
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_family = "Verdana") +
  theme(
    text = element_text(family = "Verdana"), 
    plot.title = element_markdown(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 0.5),
    legend.position = "none",
    legend.title = element_blank()
  )






