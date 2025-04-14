####################################### NBA THREE POINTERS AND TV VIEWERSHIP ####################################### 

library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(stringr)
library(hoopR)
library(sf)

source('01_Data_Prep.R')

############################################ 03. TEAM-LEVEL EDA ############################################ 

head(teams)
head(shot_props_all)
head(shot_props_season)

# Boxplot of fg3a_prop (3PT Attempts as % of Total FGA) Over Time - no outliers 
ggplot(
  shot_props_all %>% 
    filter(season %in% c(2004, 2008, 2012, 2016, 2020, 2024)), 
  aes(x = factor(season), y = three_range_prop * 100)
) +
  geom_boxplot(fill = "#E47041", alpha = 0.5, outlier.shape = NA, coef = Inf) + 
  labs(
    title = "Distribution of Team 3PT Attempt Proportions \nOver Time",
    x = "Season",
    y = "3PT Attempts as % of Total FGA"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Verdana"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Line Plot of max_3p_prop, min_3p_prop, and range_3p_prop Over Time
ggplot(shot_props_season, aes(x = season)) +
  geom_ribbon(aes(ymin = min_three_range_prop * 100, ymax = max_three_range_prop * 100),
              fill = "grey", alpha = 0.5) +
  geom_line(aes(y = max_three_range_prop * 100, color = "Max 3PT % of FGA"), size = 1) +
  geom_line(aes(y = min_three_range_prop * 100, color = "Min 3PT % of FGA"), size = 1) +
  scale_x_continuous(
    breaks = seq(min(shot_props_season$season), max(shot_props_season$season), by = 2)
  ) +
  scale_y_continuous(breaks = seq(0, 60, by = 5)) +
  scale_color_manual(values = c(
    "Max 3PT % of FGA" = "#163c83", 
    "Min 3PT % of FGA" = "#E47041"
  )) +
  labs(
    title = "Three-Point Shooting Variation Across Teams",
    x = "Season",
    y = "3PT Attempts as % of Total FGA"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Verdana"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.title = element_blank()
  )

# Smoothed version of the above 
ggplot(shot_props_season, aes(x = season)) +
  geom_ribbon(
    aes(
      ymin = predict(lm(min_three_range_prop * 100 ~ season)),
      ymax = predict(lm(max_three_range_prop * 100 ~ season))
    ),
    fill = "grey", alpha = 0.4
  ) +
  # Add linear regression trend lines
  geom_smooth(aes(y = max_three_range_prop * 100, color = "Max 3PT % of FGA"),
              method = "lm", se = FALSE, linewidth = 1) +
  geom_smooth(aes(y = min_three_range_prop * 100, color = "Min 3PT % of FGA"),
              method = "lm", se = FALSE, linewidth = 1) +
  scale_x_continuous(
    breaks = seq(min(shot_props_season$season), max(shot_props_season$season), by = 2)
  ) +
  scale_y_continuous(breaks = seq(0, 60, by = 5)) +
  scale_color_manual(values = c(
    "Max 3PT % of FGA" = "#163c83", 
    "Min 3PT % of FGA" = "#E47041"
  )) +
  labs(
    title = "Smoothed Trends in Three-Point Shooting\n Range Across Teams",
    x = "Season",
    y = "3PT Attempts as % of Total FGA"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Verdana"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.title = element_blank()
  )

# Density Plot of fg3a_prop for Select Years
ggplot(shot_props_all %>% filter(season %in% c(2002, 2008, 2012, 2016, 2020, 2024)), 
       aes(x = three_range_prop, fill = factor(season), color = factor(season))) +
  geom_density(alpha = 0.7, size = 1) +
  scale_fill_manual(values = c("2002" = "#163c83", 
                               "2008" = "#2465db", 
                               "2012" = "#7ca2e9", 
                               "2016" = "#f96c85", 
                               "2020" = "#c9082a", 
                               "2024" = "#93061f")) +
  scale_color_manual(values = c("2002" = "#163c83", 
                                "2008" = "#2465db", 
                                "2012" = "#7ca2e9", 
                                "2016" = "#f96c85", 
                                "2020" = "#c9082a", 
                                "2024" = "#93061f")) +
  facet_wrap(~season, ncol = 3, scales = "free_y") +
  labs(
    title = "Average Three-Point Attempt Proportion by\n Team: Distribution Over Time",
    x = "3PT Attempts as % of Total FGA",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Verdana"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# Shot prop by distance and season 
shots_long <- shot_props_season %>%
  select(-min_three_range_prop, -max_three_range_prop) %>%
  pivot_longer(
    cols = c("mean_short_range_prop","mean_mid_range_prop","mean_long_range_prop","mean_three_range_prop"),
    names_to = "distance_bucket",
    values_to = "proportion"
  ) %>%
  mutate(distance_bucket = factor(distance_bucket, levels = c(
    "mean_short_range_prop",
    "mean_mid_range_prop",
    "mean_long_range_prop",
    "mean_three_range_prop"
  )))

sum(is.na(shots_long))

custom_colors <- c(
  "mean_short_range_prop" = "#f8c2ac",
  "mean_mid_range_prop" = "#f18559",
  "mean_long_range_prop" = "#B54213",
  "mean_three_range_prop" = "black"
)

ggplot(shots_long, aes(x = season, y = proportion, color = distance_bucket)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2) +
  scale_color_manual(
    values = custom_colors,
    labels = c(
      "mean_short_range_prop" = "Short Range",
      "mean_mid_range_prop" = "Mid Range",
      "mean_long_range_prop" = "Long Range",
      "mean_three_range_prop" = "Three-Point Range"
    )
  ) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05)) +
  scale_x_continuous(breaks = seq(2002, 2024, by = 2)) +
  labs(
    title = "League-Average Proportion of Total Shot Attempts\nby Distance and Season",
    x = "Season",
    y = "Percent of Total FGAs",
    color = NULL 
  ) +
  theme_minimal(base_family = "Verdana") +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    text = element_text(family = "Verdana"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )






