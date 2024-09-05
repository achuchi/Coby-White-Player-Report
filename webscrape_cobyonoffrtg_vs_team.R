library(dplyr)
library(jsonlite)
library(httr)

api_url <- "https://api.pbpstats.com/get-game-logs/nba?Season=2023-24&SeasonType=Regular%2BSeason&EntityId=1629632&EntityType=Player"
GET_response <- GET(api_url)

if (status_code(GET_response) == 200) {
  PBPdata <- content(GET_response, "text", encoding = "UTF-8")
  json_data_PBP <- fromJSON(PBPdata)
  
  
  Coby_OnOffOrtg <- json_data_PBP$multi_row_table$OnOffRtg
  Coby_OnOffOrtg <- Coby_OnOffOrtg[!is.na(Coby_OnOffOrtg)]
  
  print(head(Coby_OnOffOrtg))
  

  
} else {
  print(paste("Failed to retrieve data. Status Code:", status_code(GET_response)))
}

Coby_data <- data.frame(OnOffRtg = Coby_OnOffOrtg)



stats <- Coby_data %>%
  summarise(
    Mean = mean(OnOffRtg),
    Q1 = quantile(OnOffRtg, 0.25),
    Q3 = quantile(OnOffRtg, 0.75)
  )

library(ggplot2)
ggplot(Coby_data, aes(x = factor(1), y = OnOffRtg)) +
  geom_jitter(width = 0.05, height = 0, color = "White", alpha = 0.6) +
  geom_hline(yintercept = 114.8, linetype = "dashed", color = "#87CEEB", size = 1) +
  geom_hline(yintercept = stats$Mean, linetype = "dashed", color = "green", size = 1) +
  annotate("text", x = 1, y = 114.8, label = "Bulls Offensive Rating 2024", color = "#87CEEB", vjust = 1.3, hjust = -.5, size = 4) +
  annotate("text", x = 1, y = stats$Mean, label = "Mean", color = "green", vjust = -0.5, hjust = -5, size = 4) +
  labs(x = NULL, y = "Offensive Rating w/ On") +
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
    
    axis.text.x = element_blank(),
    axis.title.y = element_text(color = "white", size = 14),
    axis.text.y = element_text(color = "white", size = 12),
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    
    strip.background = element_rect(fill = "gray20"),
    strip.text = element_text(hjust = .5, size = 8, family = "Comic Sans MS", face = "bold", color = "white"))

  
