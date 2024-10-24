library(shinycssloaders)
library(shiny)
library(shinydashboard)
library(ggplot2)

Sys.setenv("VROOM_CONNECTION_SIZE" = "1073741824")
options(shiny.maxRequestSize = 30 * 1024^2)
options(shiny.http.response.headers = list(Connection = "keep-alive"))

# External Scripts
source("R_CobyWhite_HeatMap.R")  
source("R_CobyWhite_PPGTrend.R")  
source("R_CobyWhite_OnOff.R")
source("R_CobyWhite_FGPcts.R")

# UI
ui <- dashboardPage(
  skin = "black",  # General theme set to 'black'
  dashboardHeader(
    title = "Coby White Analysis", 
    titleWidth = 300,
    tags$li(class = "dropdown",
            tags$style(HTML("
              .main-header .logo {
                background-color: #444444 !important;  /* Darker header */
                color: white !important;
              }
              .main-header .navbar {
                background-color: #444444 !important;  /* Change header bar */
              }
              .main-header .navbar .sidebar-toggle {
                color: white !important;
              }
            "))
    )
  ),
  
  dashboardSidebar(
    width = 300, 
    sidebarMenu(
      menuItem("2023-2024 Heat Map", tabName = "heatmap", icon = icon("fire")),
      menuItem("Seasonal PPG Trend", tabName = "ppg", icon = icon("chart-line")),
      menuItem("On/Off Offensive Rating", tabName = "onoff", icon = icon("balance-scale")),
      menuItem("Field Goal Percentages", tabName = "fg", icon = icon("percent"))
    ),
    tags$style(HTML("
      .skin-black .main-sidebar {
        background-color: #666666 !important;  /* Sidebar color */
      }
      .skin-black .sidebar-menu > li > a {
        color: white !important;  /* Sidebar text color */
      }
    "))
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
      body {
        background-color: #444444 !important;
      }
      .content-wrapper {
        background-color: #444444 !important;
      }
      .box {
        background-color: #444444 !important;
        border-radius: 10px;
        color: white;
      }
      .box-header {
        background-color: #800000 !important; 
        color: #CE1141; 
        font-weight: bold;
      }
      .box-body {
        background-color: #444444 !important;
        border: 4px solid #800000 !important;
        outline: none !important;  
        box-shadow: none !important;  
      }
      h3 {
        color: #ecf0f1;
      }
    "))
    ),
    
    tabItems(
      tabItem(
        tabName = "heatmap",
        fluidRow(
          box(
            title = "2023-2024 Heat Map",
            status = "primary", solidHeader = TRUE, width = 12,
            plotOutput("heatmap_plot", height = "500px") %>% withSpinner(color = "#FFFFFF", type = 8),  # Spinner
            h5("This is Coby White's shot distribution heat map for the 2023-2024 season.", style = "color: #95a5a6; text-align: center;")
          )
        )
      ),
      tabItem(
        tabName = "ppg",
        fluidRow(
          box(
            title = "Seasonal PPG Trend",
            status = "primary", solidHeader = TRUE, width = 12,
            plotOutput("ppg_plot", height = "500px") %>% withSpinner(color = "#FFFFFF", type = 8),  # Spinner
            h5("Trend in Points Per Game (PPG) over the past three seasons.", style = "color: #95a5a6; text-align: center;")
          )
        )
      ),
      tabItem(
        tabName = "onoff",
        fluidRow(
          box(
            title = "On/Off Offensive Rating",
            status = "primary", solidHeader = TRUE, width = 12,
            plotOutput("onoff_plot", height = "500px") %>% withSpinner(color = "#FFFFFF", type = 8),  # Spinner
            h5("Coby White's On/Off Offensive Rating for 2023-2024 season.", style = "color: #95a5a6; text-align: center;")
          )
        )
      ),
      tabItem(
        tabName = "fg",
        fluidRow(
          box(
            title = "Field Goal Percentages",
            status = "primary", solidHeader = TRUE, width = 12,
            plotOutput("fg_plot", height = "500px") %>% withSpinner(color = "#FFFFFF", type = 8),  # Spinner
            h5("Coby White's Field Goal percentages from various zones.", style = "color: #95a5a6; text-align: center;")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  output$heatmap_plot <- renderPlot({
    CobyWhite_HeatMap()  
  })
  
  output$ppg_plot <- renderPlot({
    CobyWhite_PPGTrend()  
  })
  
  output$onoff_plot <- renderPlot({
    CobyWhite_OnOff()  
  })
  
  output$fg_plot <- renderPlot({
    CobyWhite_FGPcts()  
  })
}


shinyApp(ui = ui, server = server)
