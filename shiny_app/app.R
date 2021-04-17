library(shiny)
library(tidyverse)
library(lubridate)
library(shinydashboard)
library(sf)
library(scales)
library(glue)
library(shinyWidgets)
library(dashboardthemes)
library(extrafont)
library(plotly)

data_full <- readr::read_csv("https://raw.githubusercontent.com/joshyam-k/scheduled-commit-action/master/data-raw/lime.csv")
map <- st_read('/Users/joshuayamamoto/test/ds_dash/shiny_app/sf_boundary1.shp')
top <- max(data_full$last_updated)
top <- with_tz(top, "America/Los_Angeles")

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
      box(plotOutput("plot1"), width = 9),
      box(knobInput(
          inputId = "Knob",
          label = "Number of hours ago:",
          value = 0,
          min = 0,
          max = 24,
          displayPrevious = F,
          lineCap = "round",
          fgColor = "#60c957",
          inputColor = "#60c957"
          ), width = 3
        )
      )
    )
)



server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
    filt_data <- reactive({
      
      date <- max(data_full$full_date)
      hour(date) <- hour(date) - input$Knob 
        
      data_full %>% 
        filter(full_date == date)
      
    })   
    
    viewed <- reactive({
      date <- max(data_full$full_date)
      hour(date) <- hour(date) - input$Knob 
      date
    })
    
    
    plott <- reactive({
      map %>% 
        ggplot() +
        geom_hex(data = filt_data(), aes(lon, lat), inherit.aes = F, bins = 18) +
        geom_sf(fill = NA, color = "grey50") +
        scale_fill_gradient(
          #name = "  Number\n of free\n Scooters",
          low = "white",
          high = "#60c957"
        ) +
        theme_void() +
        labs(
          caption = glue("Last updated: {top}"),
          fill = glue('  Number of free   \n scooters on \n {format(viewed(), "%B %d, at %H:%M %p")}')
        ) +
        theme(
          plot.caption = element_text(family = "Roboto Mono", hjust = 1.3, size = 12),
          legend.position = "left",
          legend.key.size = unit(1, "cm"),
          legend.text = element_text(size = 8, family = "Roboto Mono"),
          legend.title = element_text(size = 15, family = "Roboto Mono")
        ) +
        guides(fill = guide_colorbar(title.position = "left"))
      
    })
    
    plott()
    
  })
  

}

shinyApp(ui, server)