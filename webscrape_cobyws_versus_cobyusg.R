# Web Scraping to Find Coby White on a Win Shares versus Usage Rate Graph for the 2024 season in R  

library(rvest)
library(dplyr)
library(ggplot2)

# URL for the advanced stats
url <- "https://www.basketball-reference.com/leagues/NBA_2024_advanced.html"
page <- read_html(url)

# Table ID in BBall Ref is id = "advanced_stats"
advanced_stats <- page %>%
  html_node("#advanced_stats") %>%
  html_table(fill = TRUE)
  
names(advanced_stats) <- make.unique(names(advanced_stats))
print(names(advanced_stats))
advanced_stats <- advanced_stats[, !is.na(names(advanced_stats)) & names(advanced_stats) != ""]

advanced_stats$`USG%` <- as.numeric(gsub("[^0-9.-]", "", advanced_stats$`USG%`))
advanced_stats$WS <- as.numeric(gsub("[^0-9.-]", "", advanced_stats$WS))

player_data <- advanced_stats %>%
  dplyr::filter(!is.na(`USG%`) & `USG%` > 20) %>%
  dplyr::select(Player = Player, UsageRate = `USG%`, WinShares = WS)

# Finding Coby White!

unique(player_data$Player)
player_data$Player <- trimws(player_data$Player)

coby_white <- player_data %>% 
  filter(Player == "Coby White")

print(coby_white)

# Graph 

ggplot(player_data, aes(x = UsageRate, y = WinShares)) +
  geom_point(alpha = 0.7, color = "white") +  
  geom_point(data = coby_white, aes(x = UsageRate, y = WinShares), 
             color = "white", size = 3, shape = 21, fill = "red") +  # Highlight Coby White
  labs(title = "Win Shares vs. Usage Rate for Active NBA Players (2024 Season)",
       x = "Usage Rate (%)",
       y = "Win Shares") +
  theme_minimal() +
  coord_cartesian(xlim = c(20, 40)) +
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
