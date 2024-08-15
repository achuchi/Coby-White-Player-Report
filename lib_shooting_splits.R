# Coby White Career Shooting Trends

  #Library Codes
Sys.setenv("VROOM_CONNECTION_SIZE" = "1073741824")

library(nbastatR)
library(dplyr)
library(tidyr)
library(formattable)

player_stats <- nbastatR::players_careers(players = c("Coby White"), modes = c("PerGame")) %>%
  filter(nameTable == "SeasonTotalsRegularSeason") %>%
  unnest(cols = c(dataTable)) %>%
  arrange(desc(slugSeason)) %>%
  head(3) %>%
  arrange(slugSeason)

str(player_stats)

player_percentages <- player_stats %>%
  select(slugSeason, pctFG, pctFG3, pctFT) %>%
  rename("FG%" = "pctFG", "3P%" = "pctFG3", "FT%" = "pctFT") %>%
  pivot_longer(!slugSeason, values_to = "value") 

player_percentages$value <- percent(player_percentages$value, 1)

bar_chart <- ggplot() +
  geom_bar(player_percentages, mapping = aes(x = slugSeason, y = value, group = name, fill = name),
    stat = "identity", position = position_dodge(), color = "white", width = .6) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0), labels = scales::percent_format()) +
  
  scale_fill_manual(values = c("#CE1141", "#C8102E", "#98002E"), aesthetics = "fill") +
  facet_wrap(~name) +
  
  labs(title = "Coby White - Last Three Season Shooting Trends",
       x = "Season",
       y = "Shooting Percentages\n") +
  
  geom_text(player_percentages, mapping = aes(x = slugSeason, y = value, label = value),
            position = position_dodge(width = .9), size = 3, family = "Comic Sans MS", color = "white", vjust = 2) +
  

  theme(
    plot.title = element_text(hjust = .5, size = 14, family = "Comic Sans MS", face = "bold", color = "white"),
    text = element_text(family = "Comic Sans MS"),
    legend.position = "none",
    
    plot.background = element_rect(fill = "gray15", color = "gray15"),
    panel.background = element_rect(fill = "gray15", color = "gray15"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "gray20"),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(color = "white"),
    
    axis.title.x = element_text(color = "white", size = 10),
    axis.title.y = element_text(color = "white", size = 10),
    axis.text.x = element_text(color = "white", size = 8),
    axis.text.y = element_text(color = "white", size = 8),
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    
    strip.background = element_rect(fill = "gray20"),
    strip.text = element_text(hjust = .5, size = 12, family = "Comic Sans MS", face = "bold", color = "white"))

print(bar_chart)
