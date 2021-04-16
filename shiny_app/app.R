library(shiny)
library(tidyverse)
library(lubridate)
library(shinydashboard)
library(sf)

data_full <- readr::read_csv("https://raw.githubusercontent.com/joshyam-k/scheduled-commit-action/master/data-raw/lime.csv")
map <- st_read('/Users/joshuayamamoto/test/ds_dash/shiny_app/sf_boundary1.shp')


data_full <- data_full %>% 
  mutate(last_updated = with_tz(last_updated, "America/Los_Angeles")) %>% 
  separate(
    last_updated,
    into = c("for_slider", "extra"),
    remove = F, sep = ":"
    ) %>% 
  mutate(
    hr = str_sub(for_slider, -2),
    hr = as.numeric(hr)
    ) %>% 
  mutate(pm_am = case_when(
    hr >= 12 & hr < 24 ~ "PM",
    T ~ "AM"
  )) %>% 
  unite("full_date", c("for_slider", "pm_am"), remove = F) %>% 
  mutate(full_date = parse_date_time(full_date, "%y%m%d%h% p"))


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(plotOutput("plot1"), width = 12, status = "primary")
      ),
    fluidRow(
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:",
                    step = 3600,
                    min = min(data_full$full_date),
                    max = max(data_full$full_date),
                    value = min(data_full$full_date)),
        width = 10,
        animate = T
     ) 
    )
  )
)

server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
    filt_data <- reactive({
      
      data_full %>% 
        filter(full_date == input$slider)
      
    })    
    
    map %>% 
      ggplot() +
      geom_sf() +
      geom_bin2d(data = filt_data(), aes(lon, lat), inherit.aes = F) +
      scale_fill_gradient(
        low = "white",
        high = "#3f9154"
      ) +
      theme_minimal()
      
  })
}

shinyApp(ui, server)