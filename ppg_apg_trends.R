# --- Coby White Three Season PPG trend

Sys.setenv("VROOM_CONNECTION_SIZE" = "1073741824")

library(nbastatR)
library(dplyr)
library(tidyr)

player_stats <- nbastatR::players_careers(players = c("Coby White"), modes = c("PerGame")) %>%
  filter(nameTable == "SeasonTotalsRegularSeason") %>%
  unnest(cols = c(dataTable)) %>%
  arrange(desc(slugSeason)) %>%
  head(3) %>%
  arrange(slugSeason)

str(player_stats)

library(ggplot2)

ggplot() +
  geom_bar(player_stats, mapping = aes(x = slugSeason, y = pts),
           stat = "identity", position = position_dodge(), fill = "#CE1141", color = "white", width = .6) +
  scale_y_continuous(limits=c(0, 35), expand = c(0,0)) +
  
  labs(title = "Coby White - Three Season PPG Trend",
       x = "Season",
       y = "Points Per Game\n") +
  geom_text(player_stats, mapping = aes(x = slugSeason, y = pts, label = pts),
            position = position_dodge(width = .9), size = 4, family = "Comic Sans MS", color = "white", vjust = 2) + 

theme(
  plot.title = element_text(hjust - .5, size = 14, family = "Comic Sans MS", face = "bold", color = "white"),
  text = element_text(family = "Comic Sans MS"),
  legend.position = "none",
  
  plot.background = element_rect(fill = "gray15", color = "gray15"),
  panel.background = element_rect(fill = "gray15", color = "gray15"),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.y = element_line(color = "gray20"),
  panel.grid.major.x = element_blank(),
  axis.line = element_line(color = "white"),
  
  axis.title.x = element_text(color = "white", size = 14),
  axis.title.y = element_text(color = "white", size = 14),
  axis.text.x = element_text(color = "white", size = 12),
  axis.text.y = element_text(color = "white", size = 12),
  plot.margin = margin(.5, .5, .5, .5, "cm"),
  
  strip.background = element_rect(fill = "gray20"),
  strip.text = element_text(hjust = .5, size = 8, family = "Comic Sans MS", face = "bold", color = "white"))

ggsave("player_pts_bar_chart.png", height = 6, width = 6, dpi = "retina")

# Coby White Last Three Season Ast

library(ggplot2)

ggplot() +
  geom_bar(player_stats, mapping = aes(x = slugSeason, y = ast),
           stat = "identity", position = position_dodge(), fill = "#CE1141", color = "white", width = .6) +
  scale_y_continuous(limits=c(0, 35), expand = c(0,0)) +
  
  labs(title = "Coby White - Three Season Assist Trend",
       x = "Season",
       y = "Assists\n") +
  geom_text(player_stats, mapping = aes(x = slugSeason, y = ast, label = ast),
            position = position_dodge(width = .9), size = 4, family = "Comic Sans MS", color = "white", vjust = 2) + 
  
  theme(
    plot.title = element_text(hjust - .5, size = 14, family = "Comic Sans MS", face = "bold", color = "white"),
    text = element_text(family = "Comic Sans MS"),
    legend.position = "none",
    
    plot.background = element_rect(fill = "gray15", color = "gray15"),
    panel.background = element_rect(fill = "gray15", color = "gray15"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "gray20"),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(color = "white"),
    
    axis.title.x = element_text(color = "white", size = 14),
    axis.title.y = element_text(color = "white", size = 14),
    axis.text.x = element_text(color = "white", size = 12),
    axis.text.y = element_text(color = "white", size = 12),
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    
    strip.background = element_rect(fill = "gray20"),
    strip.text = element_text(hjust = .5, size = 8, family = "Comic Sans MS", face = "bold", color = "white"))

ggsave("player_pts_bar_chart.png", height = 6, width = 6, dpi = "retina")
