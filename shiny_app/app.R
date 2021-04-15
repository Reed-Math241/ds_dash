library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 24, 1)
      )
    )
  )
)

server <- function(input, output) {
  
  data_full <- readr::read_csv("https://raw.githubusercontent.com/joshyam-k/scheduled-commit-action/master/data-raw/lime_data.csv")
  
  
  output$plot1 <- renderPlot({
    
    filt_data <- reactive({
      
      data_full %>% 
        filter(hour == input$slider)
      
    })    
    
    filt_data() %>% 
      ggplot(aes(lon, lat)) +
      # geom_denisty_2d_filled()
      geom_bin2d() +
      coord_map() +
      theme_minimal()
  })
}

shinyApp(ui, server)