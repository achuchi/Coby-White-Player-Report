# Coby White Three Season Usage

install.packages("rvest")

library(rvest)
library(dplyr)
library(ggplot2)


url <- "https://www.basketball-reference.com/players/w/whiteco01.html"
page <- read_html(url)


advanced_table <- page %>%
  html_node("#advanced") %>%
  html_table(fill = TRUE)


head(advanced_table)

names(advanced_table) <- make.names(names(advanced_table))


print(names(advanced_table))

usage_rate_data <- advanced_table %>%
  dplyr::select(Season = Season, UsageRate = USG.) %>%  
  dplyr::filter(!is.na(UsageRate)) %>%
  dplyr::arrange(desc(Season)) %>%
  dplyr::slice_head(n = 4)


usage_rate_data$UsageRate <- as.numeric(usage_rate_data$UsageRate)

ggplot(usage_rate_data, aes(x = Season, y = UsageRate)) +
  geom_bar(stat = "identity", position = position_dodge(), fill = "#CE1141", color = "white", width = .6) +
  labs(title = "Usage Rate of Coby White (Last 3 Seasons)",
       x = "Season",
       y = "Usage Rate (%)") +
  geom_text(aes(label = paste0(UsageRate, "%")), 
            vjust = 1.5, color = "white", size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
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
