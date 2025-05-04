####################################### NBA THREE POINTERS AND TV VIEWERSHIP ####################################### 

library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(stringr)
library(hoopR)
library(purrr)
library(sf)

source('01_Data_Prep.R')

####################################### 04. PLOTTING A BASKETBALL COURT ###################################### 

# Define Court Elements (in feet)
width <- 50                
height <- 94 / 2         
key_width <- 16
key_height <- 19
key_radius <- 6
hoop_radius <- 0.75       
hoop_center_y <- 4       
backboard_offset <- 4    
backboard_width <- 6
backboard_thickness <- 0.1
neck_length <- 0.5        
restricted_area_radius <- 4
three_point_radius <- 23.75
three_point_side_offset <- 3.2
three_point_side_height <- 13.5

# Specify dimensions for the Key (Paint)
key_ext <- rbind(
  c(width/2 - key_width/2, 0),
  c(width/2 - key_width/2, key_height),
  c(width/2 + key_width/2, key_height),
  c(width/2 + key_width/2, 0),
  c(width/2 - key_width/2, 0)
)
key <- st_polygon(list(key_ext))

# Specify dimensions for the Key Circle
key_center <- st_point(c(width/2, key_height)) %>% st_sfc()
key_circle <- st_buffer(key_center, dist = key_radius)
key_circle <- st_crop(key_circle, xmin = 0, xmax = width, ymin = key_height, ymax = height)

# Specify dimensions for the Half Court Circle
half_center <- st_point(c(width/2, height)) %>% st_sfc()
half_circle <- st_buffer(half_center, dist = key_radius)
half_circle <- st_crop(half_circle, xmin = 0, xmax = width, ymin = 0, ymax = height)

# Specify dimensions/shapes for the Three Point Line
three_center <- st_point(c(width/2, hoop_center_y)) %>% st_sfc()
three_ext <- st_buffer(three_center, dist = three_point_radius)
three_ext <- st_crop(three_ext, 
                     xmin = three_point_side_offset, 
                     xmax = width - three_point_side_offset, 
                     ymin = three_point_side_height, ymax = height)

n <- nrow(st_coordinates(three_ext))
three_arc <- st_coordinates(three_ext)[1:(n-2), 1:2]
three_point_line <- st_polygon(list(rbind(
  c(three_point_side_offset, 0),
  c(three_point_side_offset, three_point_side_height),
  three_arc,
  c(width - three_point_side_offset, three_point_side_height),
  c(width - three_point_side_offset, 0),
  c(three_point_side_offset, 0)
)))

# Specify the Restricted Area
ra_center <- st_point(c(width/2, hoop_center_y)) %>% st_sfc()
ra_ext <- st_buffer(ra_center, dist = restricted_area_radius)
ra_ext <- st_crop(ra_ext, xmin = 0, xmax = width, ymin = hoop_center_y, ymax = height)

n <- nrow(st_coordinates(ra_ext))
ra_arc <- st_coordinates(ra_ext)[1:(n-2), 1:2]
restricted_area <- st_polygon(list(rbind(
  c(width/2 - restricted_area_radius, backboard_offset),
  c(width/2 - restricted_area_radius, hoop_center_y),
  ra_arc,
  c(width/2 + restricted_area_radius, hoop_center_y),
  c(width/2 + restricted_area_radius, backboard_offset),
  c(width/2 - restricted_area_radius, backboard_offset)
)))

# Create polygon/shape items to plot 
court_features <- st_sf(
  geometry = st_sfc(list(
    key, key_circle[[1]], half_circle[[1]], three_point_line, restricted_area
  )),
  feature = c("Key", "Key Circle", "Half Circle", "Three Point Line", "Restricted Area")
)

# Specify dimensions for the half-court base
half_court_int <- rbind(
  c(0, 0),
  c(0, height),
  c(width, height),
  c(width, 0),
  c(0, 0)
)
half_court <- st_polygon(list(half_court_int)) %>% st_sfc()

# Add a point for thr Hoop 
hoop <- st_point(c(width / 2, hoop_center_y)) %>% st_sfc()

# Distance breaks - based on 8-ft shot ranges from the NBA website 
distance_breaks <- c(0, 8, 16, 23.75, 40)

# Create rings for each distance 
rings <- map2(distance_breaks[-length(distance_breaks)], distance_breaks[-1], ~{
  outer <- st_buffer(hoop, dist = .y)
  inner <- st_buffer(hoop, dist = .x)
  diff <- st_difference(outer, inner)
  st_cast(diff, "POLYGON")
})

# Flatten the list (as st_cast returns a list per ring)
flat_rings <- do.call(c, rings)

# Create sf object
rings_sf <- st_sf(
  geometry = st_sfc(flat_rings),
  distance_bin = c("Short Range", "Mid Range", "Long Range", "Three-Point Range")
)

# Convert rings/distances to factors 
rings_sf$distance_bin <- factor(
  rings_sf$distance_bin,
  levels = c(
    "Three-Point Range",
    "Long Range",
    "Mid Range",
    "Short Range"
  )
)

# Plot shot volume by range for various seasons 
# Plot for 2004 
ggplot() +
  geom_sf(data = rings_sf, aes(fill = distance_bin), color = NA, alpha = 0.9) +
  geom_sf(data = half_court, fill = NA, color = "black", size = 3) +
  geom_point(aes(x = width / 2, y = hoop_center_y), size = 5, color = "black") +
  geom_sf(data = court_features, fill = NA, color = "black", linewidth = 1) +
  geom_segment(aes(x = 0, xend = width, y = height, yend = height), 
               color = "black", linewidth = 1) +
  geom_text(data = tibble(
    x = width / 2,
    y = c(6, 12, 19.5, 27),
    label = c("40%", "17%", "24%", "19%")
  ), aes(x = x, y = y + 3, label = label), 
  family = "Verdana", fontface = "bold", size = 4, color = "black") +
  coord_sf(xlim = c(0, width), ylim = c(0, height), expand = FALSE) +
  scale_fill_manual(
    values = c(
      "Short Range" = "#b53f0f",
      "Mid Range" = "#f49973",
      "Long Range" = "#f07d4e",
      "Three-Point Range" = "#f49973"
    )
  ) +
  labs(
    title = "2004-05",
    fill = "Shot Distance"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    text = element_text(family = "Verdana"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "none",
    legend.title = element_blank(),
    rect = element_rect(fill = "transparent"),
  )

# Plot for 2006
ggplot() +
  geom_sf(data = rings_sf, aes(fill = distance_bin), color = NA, alpha = 0.9) +
  geom_sf(data = half_court, fill = NA, color = "black", size = 3) +
  geom_point(aes(x = width / 2, y = hoop_center_y), size = 5, color = "black") +
  geom_sf(data = court_features, fill = NA, color = "black", linewidth = 1) +
  geom_segment(aes(x = 0, xend = width, y = height, yend = height), 
               color = "black", linewidth = 1) +
  geom_text(data = tibble(
    x = width / 2,
    y = c(6, 12, 19.5, 27),
    label = c("42%", "15%", "23%", "21%")
  ), aes(x = x, y = y + 3, label = label), 
  family = "Verdana", fontface = "bold", size = 4, color = "black") +
  coord_sf(xlim = c(0, width), ylim = c(0, height), expand = FALSE) +
  scale_fill_manual(
    values = c(
      "Short Range" = "#b53f0f",
      "Mid Range" = "#f49973",
      "Long Range" = "#f07d4e",
      "Three-Point Range" = "#f07d4e"
    )
  ) +
  labs(
    title = "2006-07",
    fill = "Shot Distance"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    text = element_text(family = "Verdana"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "none",
    legend.title = element_blank(),
    rect = element_rect(fill = "transparent")
  )

# Plot for 2008
ggplot() +
  geom_sf(data = rings_sf, aes(fill = distance_bin), color = NA, alpha = 0.9) +
  geom_sf(data = half_court, fill = NA, color = "black", size = 3) +
  geom_point(aes(x = width / 2, y = hoop_center_y), size = 5, color = "black") +
  geom_sf(data = court_features, fill = NA, color = "black", linewidth = 1) +
  geom_segment(aes(x = 0, xend = width, y = height, yend = height), 
               color = "black", linewidth = 1) +
  geom_text(data = tibble(
    x = width / 2,
    y = c(6, 12, 19.5, 27),
    label = c("40%", "15%", "23%", "22%")
  ), aes(x = x, y = y + 3, label = label), 
  family = "Verdana", fontface = "bold", size = 4, color = "black") +
  coord_sf(xlim = c(0, width), ylim = c(0, height), expand = FALSE) +
  scale_fill_manual(
    values = c(
      "Short Range" = "#b53f0f",
      "Mid Range" = "#f49973",
      "Long Range" = "#f07d4e",
      "Three-Point Range" = "#f07d4e"
    )
  ) +
  labs(
    title = "2008-09",
    fill = "Shot Distance"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    text = element_text(family = "Verdana"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "none",
    legend.title = element_blank(),
    rect = element_rect(fill = "transparent")
  )

# Plot for 2010
ggplot() +
  geom_sf(data = rings_sf, aes(fill = distance_bin), color = NA, alpha = 0.9) +
  geom_sf(data = half_court, fill = NA, color = "black", size = 3) +
  geom_point(aes(x = width / 2, y = hoop_center_y), size = 5, color = "black") +
  geom_sf(data = court_features, fill = NA, color = "black", linewidth = 1) +
  geom_segment(aes(x = 0, xend = width, y = height, yend = height), 
               color = "black", linewidth = 1) +
  geom_text(data = tibble(
    x = width / 2,
    y = c(6, 12, 19.5, 27),
    label = c("42%", "15%", "22%", "22%")
  ), aes(x = x, y = y + 3, label = label), 
  family = "Verdana", fontface = "bold", size = 4, color = "black") +
  coord_sf(xlim = c(0, width), ylim = c(0, height), expand = FALSE) +
  scale_fill_manual(
    values = c(
      "Short Range" = "#b53f0f",
      "Mid Range" = "#f49973",
      "Long Range" = "#f07d4e",
      "Three-Point Range" = "#f07d4e"
    )
  ) +
  labs(
    title = "2010-11",
    fill = "Shot Distance"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    text = element_text(family = "Verdana"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "none",
    legend.title = element_blank(),
    rect = element_rect(fill = "transparent")
  )

# Plot for 2012
ggplot() +
  geom_sf(data = rings_sf, aes(fill = distance_bin), color = NA, alpha = 0.9) +
  geom_sf(data = half_court, fill = NA, color = "black", size = 3) +
  geom_point(aes(x = width / 2, y = hoop_center_y), size = 5, color = "black") +
  geom_sf(data = court_features, fill = NA, color = "black", linewidth = 1) +
  geom_segment(aes(x = 0, xend = width, y = height, yend = height), 
               color = "black", linewidth = 1) +
  geom_text(data = tibble(
    x = width / 2,
    y = c(6, 12, 19.5, 27),
    label = c("42%", "14%", "19%", "24%")
  ), aes(x = x, y = y + 3, label = label), 
  family = "Verdana", fontface = "bold", size = 4, color = "black") +
  coord_sf(xlim = c(0, width), ylim = c(0, height), expand = FALSE) +
  scale_fill_manual(
    values = c(
      "Short Range" = "#b53f0f",
      "Mid Range" = "#f49973",
      "Long Range" = "#f49973",
      "Three-Point Range" = "#f07d4e"
    )
  ) +
  labs(
    title = "2012-13",
    fill = "Shot Distance"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    text = element_text(family = "Verdana"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "none",
    legend.title = element_blank(),
    rect = element_rect(fill = "transparent")
  )

# Plot for 2014 
ggplot() +
  geom_sf(data = rings_sf, aes(fill = distance_bin), color = NA, alpha = 0.9) +
  geom_sf(data = half_court, fill = NA, color = "black", size = 3) +
  geom_point(aes(x = width / 2, y = hoop_center_y), size = 5, color = "black") +
  geom_sf(data = court_features, fill = NA, color = "black", linewidth = 1) +
  geom_segment(aes(x = 0, xend = width, y = height, yend = height), 
               color = "black", linewidth = 1) +
  geom_text(data = tibble(
    x = width / 2,
    y = c(6, 12, 19.5, 27),
    label = c("42%", "14%", "18%", "27%")
  ), aes(x = x, y = y + 3, label = label), 
  family = "Verdana", fontface = "bold", size = 4, color = "black") +
  coord_sf(xlim = c(0, width), ylim = c(0, height), expand = FALSE) +
  scale_fill_manual(
    values = c(
      "Short Range" = "#b53f0f",
      "Mid Range" = "#f49973",
      "Long Range" = "#f49973",
      "Three-Point Range" = "#ed6128"
    )
  ) +
  labs(
    title = "2014-15",
    fill = "Shot Distance"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    text = element_text(family = "Verdana"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "none",
    legend.title = element_blank(),
    rect = element_rect(fill = "transparent")
  )

# Plot for 2016
ggplot() +
  geom_sf(data = rings_sf, aes(fill = distance_bin), color = NA, alpha = 0.9) +
  geom_sf(data = half_court, fill = NA, color = "black", size = 3) +
  geom_point(aes(x = width / 2, y = hoop_center_y), size = 5, color = "black") +
  geom_sf(data = court_features, fill = NA, color = "black", linewidth = 1) +
  geom_segment(aes(x = 0, xend = width, y = height, yend = height), 
               color = "black", linewidth = 1) +
  geom_text(data = tibble(
    x = width / 2,
    y = c(6, 12, 19.5, 27),
    label = c("41%", "13%", "14%", "31%")
  ), aes(x = x, y = y + 3, label = label), 
  family = "Verdana", fontface = "bold", size = 4, color = "black") +
  coord_sf(xlim = c(0, width), ylim = c(0, height), expand = FALSE) +
  scale_fill_manual(
    values = c(
      "Short Range" = "#b53f0f",
      "Mid Range" = "#f49973",
      "Long Range" = "#f49973",
      "Three-Point Range" = "#db4c12"
    )
  ) +
  labs(
    title = "2016-17",
    fill = "Shot Distance"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    text = element_text(family = "Verdana"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "none",
    legend.title = element_blank(),
    rect = element_rect(fill = "transparent")
  )

# Plot for 2018
ggplot() +
  geom_sf(data = rings_sf, aes(fill = distance_bin), color = NA, alpha = 0.9) +
  geom_sf(data = half_court, fill = NA, color = "black", size = 3) +
  geom_point(aes(x = width / 2, y = hoop_center_y), size = 5, color = "black") +
  geom_sf(data = court_features, fill = NA, color = "black", linewidth = 1) +
  geom_segment(aes(x = 0, xend = width, y = height, yend = height), 
               color = "black", linewidth = 1) +
  geom_text(data = tibble(
    x = width / 2,
    y = c(6, 12, 19.5, 27),
    label = c("42%", "13%", "9%", "36%")
  ), aes(x = x, y = y + 3, label = label), 
  family = "Verdana", fontface = "bold", size = 4, color = "black") +
  coord_sf(xlim = c(0, width), ylim = c(0, height), expand = FALSE) +
  scale_fill_manual(
    values = c(
      "Short Range" = "#b53f0f",
      "Mid Range" = "#f49973",
      "Long Range" = "#fad0bf",
      "Three-Point Range" = "#db4c12"
    )
  ) +
  labs(
    title = "2018-19",
    fill = "Shot Distance"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    text = element_text(family = "Verdana"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "none",
    legend.title = element_blank(),
    rect = element_rect(fill = "transparent")
  )

# Plot for 2020
ggplot() +
  geom_sf(data = rings_sf, aes(fill = distance_bin), color = NA, alpha = 0.9) +
  geom_sf(data = half_court, fill = NA, color = "black", size = 3) +
  geom_point(aes(x = width / 2, y = hoop_center_y), size = 5, color = "black") +
  geom_sf(data = court_features, fill = NA, color = "black", linewidth = 1) +
  geom_segment(aes(x = 0, xend = width, y = height, yend = height), 
               color = "black", linewidth = 1) +
  geom_text(data = tibble(
    x = width / 2,
    y = c(6, 12, 19.5, 27),
    label = c("41%", "13%", "7%", "39%")
  ), aes(x = x, y = y + 3, label = label), 
  family = "Verdana", fontface = "bold", size = 4, color = "black") +
  coord_sf(xlim = c(0, width), ylim = c(0, height), expand = FALSE) +
  scale_fill_manual(
    values = c(
      "Short Range" = "#b53f0f",
      "Mid Range" = "#f49973",
      "Long Range" = "#fad0bf",
      "Three-Point Range" = "#b53f0f"
    )
  ) +
  labs(
    title = "2020-21",
    fill = "Shot Distance"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    text = element_text(family = "Verdana"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "none",
    legend.title = element_blank(),
    rect = element_rect(fill = "transparent")
  )

# Plot for 2022
ggplot() +
  geom_sf(data = rings_sf, aes(fill = distance_bin), color = NA, alpha = 0.9) +
  geom_sf(data = half_court, fill = NA, color = "black", size = 3) +
  geom_point(aes(x = width / 2, y = hoop_center_y), size = 5, color = "black") +
  geom_sf(data = court_features, fill = NA, color = "black", linewidth = 1) +
  geom_segment(aes(x = 0, xend = width, y = height, yend = height), 
               color = "black", linewidth = 1) +
  geom_text(data = tibble(
    x = width / 2,
    y = c(6, 12, 19.5, 27),
    label = c("42%", "14%", "6%", "39%")
  ), aes(x = x, y = y + 3, label = label), 
  family = "Verdana", fontface = "bold", size = 4, color = "black") +
  coord_sf(xlim = c(0, width), ylim = c(0, height), expand = FALSE) +
  scale_fill_manual(
    values = c(
      "Short Range" = "#b53f0f",
      "Mid Range" = "#f49973",
      "Long Range" = "#fce1d6",
      "Three-Point Range" = "#b53f0f"
    )
  ) +
  labs(
    title = "2022-23",
    fill = "Shot Distance"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    text = element_text(family = "Verdana"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "none",
    legend.title = element_blank(),
    rect = element_rect(fill = "transparent")
  )



# Plot for 2024 
ggplot() +
  geom_sf(data = rings_sf, aes(fill = distance_bin), color = NA, alpha = 0.9) +
  geom_sf(data = half_court, fill = NA, color = "black", size = 3) +
  geom_point(aes(x = width / 2, y = hoop_center_y), size = 5, color = "black") +
  geom_sf(data = court_features, fill = NA, color = "black", linewidth = 1) +
  geom_segment(aes(x = 0, xend = width, y = height, yend = height), 
               color = "black", linewidth = 1) +
  geom_text(data = tibble(
    x = width / 2,
    y = c(6, 12, 19.5, 27),
    label = c("40%", "13%", "5%", "42%")
  ), aes(x = x, y = y + 3, label = label), 
  family = "Verdana", fontface = "bold", size = 4, color = "black") +
  coord_sf(xlim = c(0, width), ylim = c(0, height), expand = FALSE) +
  scale_fill_manual(
    values = c(
      "Short Range" = "#b53f0f",
      "Mid Range" = "#f49973",
      "Long Range" = "#fce1d6",
      "Three-Point Range" = "#b53f0f"
    )
  ) +
  labs(
    title = "2024-25",
    fill = "Shot Distance"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    text = element_text(family = "Verdana"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "none",
    legend.title = element_blank(),
    rect = element_rect(fill = "transparent")
  )




