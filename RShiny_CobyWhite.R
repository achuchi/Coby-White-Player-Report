

library(shiny)
library(ggplot2)

Sys.setenv("VROOM_CONNECTION_SIZE" = "1073741824")
options(shiny.maxRequestSize = 30 * 1024^2)  
options(shiny.http.response.headers = list(Connection = "keep-alive"))
plotOutput("selected_plot", height = "600px", width = "600px")

# External Scripts
source("R_CobyWhite_HeatMap.R")  
source("R_CobyWhite_PPGTrend.R")  
source("R_CobyWhite_OnOff.R")


captions <- list(
  heatmap = "Heat map placeholder caption",
  ppg = "ppg placeholder caption",
  plot2 = "This is the caption for Plot 2.",
  plot3 = "This is the caption for Plot 3."
)

# UI
ui <- fluidPage(
  titlePanel("Interactive Plots with Captions"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Select a Plot"),
      selectInput("plot_choice", "Choose a Plot:", 
                  choices = c("2023-2024 Heat Map" = "heatmap",  
                              "Seasonal PPG Trend" = "ppg", 
                              "On/Off Offensive Rating" = "OnOff", 
                              "Plot 3" = "plot3")),
   
    ),
    
    mainPanel(
      uiOutput("plot_box")  
    )
  )
)

server <- function(input, output) {
  
  output$plot_box <- renderUI({
    tagList(
      div(style = "border: 1px solid gray; padding: 10px; background-color: #f9f9f9; border-radius: 5px;",
          plotOutput("selected_plot"),
          h5(captions[[input$plot_choice]], style = "color: gray;")  
      )
    )
  })
  
  output$selected_plot <- renderPlot({
    switch(input$plot_choice,
           "heatmap" = CobyWhite_HeatMap(),  
           "ppg" = CobyWhite_PPGTrend(),  
           "OnOff" = CobyWhite_OnOff(),  
           "plot3" = plot3())  
  })
}

shinyApp(ui = ui, server = server)
