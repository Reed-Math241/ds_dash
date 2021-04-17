library(shiny)
library(tidyverse)
library(lubridate)
library(shinydashboard)
library(sf)
library(scales)
library(glue)
library(bslib)
library(dashboardthemes)

data_full <- readr::read_csv("https://raw.githubusercontent.com/joshyam-k/scheduled-commit-action/master/data-raw/lime.csv")
map <- st_read('/Users/joshuayamamoto/test/ds_dash/shiny_app/sf_boundary1.shp')
top <- max(data_full$last_updated)

data_full <- data_full %>% 
  separate(
    last_updated,
    into = c("for_slider", "extra"),
    remove = F, sep = ":"
    ) %>% 
  mutate(full_date = ymd_h(for_slider)) %>% 
  mutate(
    hr = str_sub(full_date, -2),
    hr = as.numeric(hr)
    ) %>% 
  
  mutate(full_date = with_tz(full_date, "America/Los_Angeles")) %>% 
  mutate(pm_am = case_when(
    hr >= 12 & hr < 24 ~ "PM",
    T ~ "AM"
  )) 


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    shinyDashboardThemes(
      theme = "grey_light"
    ),
    fluidRow(
      box(plotOutput("plot1"), width = 12)
      ),
    fluidRow(
      box(
        title = "Controls",
        align = "center",
        width = 10,
        sliderInput("slider", "Date/time:",
                    step = 3600,
                    min = min(data_full$full_date),
                    max = max(data_full$full_date),
                    value = min(data_full$full_date),
        timeFormat = "%h-%d, %H:%M %p")
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
      geom_hex(data = filt_data(), aes(lon, lat), inherit.aes = F, bins = 18) +
      geom_sf(fill = NA, color = "grey50") +
      scale_fill_gradient(
        name = "Number\n of free\n Scooters",
        low = "white",
        high = "#60c957"
      ) +
      theme_void() +
      labs(
        caption = glue("Last updated: {top}")
      ) +
      theme(
        legend.position = "left",
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 12)
      ) +
      guides(fill = guide_colorbar(title.position = "left"))
      
  })
}

shinyApp(ui, server)