# Coby White SHOT CHART


library(ggplot2)

library(tidyverse)

library(nbastatR)

library(devtools)
devtools::install_github("lbenz730/ncaahoopR")
library(ncaahoopR)

install.packages("extrafont")
library(extrafont)

library(cowplot)

circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(data_frame(x = center[1] + radius * cos(angles),
                    y = center[2] + radius * sin(angles)))
}

# Court Set Up
width = 50
height = 94 / 2
key_height = 19
inner_key_width = 12
outer_key_width = 16
backboard_width = 6
backboard_offset = 4
neck_length = 0.5
hoop_radius = 0.75
hoop_center_y = backboard_offset + neck_length + hoop_radius
three_point_radius = 23.75
three_point_side_radius = 22
three_point_side_height = 14


court_themes = list(
  light = list(
    court = 'floralwhite',
    lines = '#999999',
    text = '#222222',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 1,
    hex_border_color = "#000000"
  ),
  dark = list(
    court = '#000004',
    lines = '#999999',
    text = '#f0f0f0',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 0,
    hex_border_color = "#000000"
  ),
  ppt = list(
    court = 'gray15',
    lines = 'white',
    text = '#f0f0f0',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 0,
    hex_border_color = "gray15"
  )
)


plot_court = function(court_theme = court_themes$light, use_short_three = FALSE) {
  if (use_short_three) {
    three_point_radius = 22
    three_point_side_height = 0
  }
  
  court_points = data_frame(
    x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
    y = c(height, 0, 0, height, height),
    desc = "perimeter"
  )
  
  court_points = bind_rows(court_points , data_frame(
    x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
    y = c(0, key_height, key_height, 0),
    desc = "outer_key"
  ))
  
  court_points = bind_rows(court_points , data_frame(
    x = c(-backboard_width / 2, backboard_width / 2),
    y = c(backboard_offset, backboard_offset),
    desc = "backboard"
  ))
  
  court_points = bind_rows(court_points , data_frame(
    x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
  ))
  
  foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
  
  foul_circle_top = filter(foul_circle, y > key_height) %>%
    mutate(desc = "foul_circle_top")
  
  foul_circle_bottom = filter(foul_circle, y < key_height) %>%
    mutate(
      angle = atan((y - key_height) / x) * 180 / pi,
      angle_group = floor((angle - 5.625) / 11.25),
      desc = paste0("foul_circle_bottom_", angle_group)
    ) %>%
    filter(angle_group %% 2 == 0) %>%
    select(x, y, desc)
  
  hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
    mutate(desc = "hoop")
  
  restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
    filter(y >= hoop_center_y) %>%
    mutate(desc = "restricted")
  
  three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
    filter(y >= three_point_side_height, y >= hoop_center_y)
  
  three_point_line = data_frame(
    x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
    y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
    desc = "three_point_line"
  )
  
  court_points = bind_rows(
    court_points,
    foul_circle_top,
    foul_circle_bottom,
    hoop,
    restricted,
    three_point_line
  )
  
  
  court_points <- court_points
  
  
  ggplot() +
    geom_path(
      data = court_points,
      aes(x = x, y = y, group = desc),
      color = court_theme$lines
    ) +
    coord_fixed(ylim = c(0, 45), xlim = c(-25, 25)) +
    theme_minimal(base_size = 22) +
    theme(
      text = element_text(color = court_theme$text),
      plot.background = element_rect(fill = 'gray15', color = 'gray15'),
      panel.background = element_rect(fill = court_theme$court, color = court_theme$court),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.background = element_rect(fill = court_theme$court, color = court_theme$court),
      legend.margin = margin(-1, 0, 0, 0, unit = "lines"),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0))
    )
}


plot_court(court_theme = court_themes$ppt)


#Pulling Shots

nba_teams() %>% filter(isNonNBATeam == 0)
team <- teams_shots(teams = "Chicago Bulls", seasons = 2024, season_types = "Regular Season")

print(head(team))
print(unique(team$namePlayer))


Coby_ShotDiet <- team %>%
  filter(namePlayer == "Coby White") %>%
  mutate(x = as.numeric(as.character(locationX)) / 10, 
         y = as.numeric(as.character(locationY)) / 10 + hoop_center_y)


Coby_ShotDiet$x <- Coby_ShotDiet$x * -1


print(head(Coby_ShotDiet))

#Heat Map


library(paletteer)

palette <- paletteer::paletteer_d("RColorBrewer::YlOrRd", direction = -1)

p1 <-  plot_court(court_themes$ppt) +
  geom_density_2d_filled(Coby_ShotDiet, mapping = aes(x = x, y = y, fill = ..level..,),
                         contour_var = "ndensity", breaks = seq(0.1, 1.0, length.out = 10), alpha = .5) +
  scale_fill_manual(values = c(palette), aesthetics = c("fill", "color")) +
  scale_x_continuous(limits = c(-27.5, 27.5)) +
  scale_y_continuous(limits = c(0, 45)) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5, size = 22, face = "bold", vjust = -4),
        plot.subtitle = element_text(hjust = .5, size = 10, face = "bold", vjust = -8),
        legend.title = element_blank(),
        legend.text = element_text(hjust = .5, size = 10, face = "bold", colour = "white"),
        plot.caption = element_text(hjust = .5, size = 6, face = "bold", colour = "lightgrey", vjust = 8)) +
  labs(title = "Coby White Heat Shot Chart",
       subtitle = "2023-2024 Season")

library(cowplot)
ggdraw(p1) + theme(plot.background = element_rect(fill="gray15", color = NA))
