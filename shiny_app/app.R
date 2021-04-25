## packages --------------------------------------------------------------------

library(shiny)
library(tidyverse)
library(lubridate)
library(shinydashboard)
library(sf)
library(scales)
library(glue)
library(shinyWidgets)
library(fresh)
library(leaflet)

## data loading ----------------------------------------------------------------

data_full <- readr::read_csv("https://raw.githubusercontent.com/joshyam-k/schPull/master/data-folder/lime.csv")
map <- st_read('/Users/joshuayamamoto/test/ds_dash/shiny_app/sf_boundary1.shp')
neibs <- st_read('/Users/joshuayamamoto/test/ds_dash/shiny_app/sf_neibs.shp')
districts <- st_read("/Users/joshuayamamoto/test/ds_dash/shiny_app/sf_districts.shp")
top <- max(data_full$last_updated)
top <- with_tz(top, "America/Los_Angeles")

## fresh theme -----------------------------------------------------------------

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#717b85"
  ),
  adminlte_sidebar(
    width = "300px",
    dark_bg = "#444e59",
    dark_hover_bg = "#9ca8b5",
    dark_hover_color = "#f7faf9",
    light_color = "#f7faf9" 
  ),
  adminlte_global(
    content_bg = "#d8e3df",
    box_bg = "D8DEE9",
    info_box_bg = "D8DEE9"
  )
)

## prelim wrangling ------------------------------------------------------------

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

coords <- data_full %>% 
  select(lon, lat) %>%
  st_as_sf(coords = c(1,2))

coords_new <- coords %>% 
  st_set_crs(st_crs(neibs))

neibs$n <- lengths(st_intersects(neibs$geometry, coords_new))

districts$n <- lengths(st_intersects(districts$geometry, coords_new))


## dashboard -------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(
    title = "Lime Scooter Trends"
    ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "abt", icon = icon("question-circle")),
      menuItem("Trends", tabName = "trends", icon = icon("poll")),
      menuItem("In Detail", tabName = "detail", icon = icon("binoculars"))
    )
  ),
  dashboardBody(
    use_theme(mytheme),
    tabItems(
      tabItem(
        tabName = "abt",
        fluidRow(
          tags$head(
            tags$style(HTML("
              @import url('https://fonts.googleapis.com/css2?family=Roboto+Mono:wght@100&display=swap');
              h2 {
                font-family: 'Roboto Mono', sans-serif;
                font-weight: 800;
              }
              p {
                font-family: 'Roboto Mono', sans-serif;
                font-weight: 800;
              }"))
          ),
          box(
            width = 12,
            h2("About the Dashboard"),
            p("The main purpose of this dashboard is to provide a way to explore and visualize the geolocations of 
              lime scooters across San Francisco, California. The dashboard is split into two main components, a 'Trends' tab that 
              shows the general distribution and density of lime scooters across space, and a 'In detail' tab that allows the user
              to look at the actual exact locations across San Francisco as well as some aggregation by neighborhood", style = "font:Times")
          )
        )
      ),
      tabItem(tabName = "trends",
              fluidRow(
                column(width = 9,
                  box(
                    solidHeader = T,
                    plotOutput("plot1"),
                    width = NULL
                    ),
                  box(
                    SolidHeader = T,
                    plotOutput("plot2"),
                    width = NULL, height = 225
                  )
                ),
                column(width = 3,
                  box(
                    solidHeader = T,
                    radioGroupButtons(
                      inputId = "bins",
                      selected = "14",
                      label = "Number of hexagonal bins:", 
                      choices = c("100" = 10, "200" = 14, "300" = 17, "400" = 20, "500" = 23, "600" = 25),
                      status = "primary",
                      size = "lg",
                      individual = T
                      ),
                    width = NULL, height = 135
                  ),
                  box(
                    solidHeader = T,
                    knobInput(
                      inputId = "Knob",
                      label = "Number of hours ago:",
                      thickness = 0.35,
                      value = 0,
                      min = 0,
                      max = 24,
                      displayPrevious = F,
                      lineCap = "round",
                      fgColor = "#576a7d",
                      bgColor = "#f2f4f5",
                      inputColor = "#8ca4d4",
                      pre = "-"
                    ),
                    width = NULL
                  )
              )
          )
        ),
      tabItem(tabName = "detail",
              fluidRow(
                box(
                  solidHeader = T,
                  leafletOutput("plot3"),
                  width = 9
                ),
                box(
                  solidHeader = T,
                  sliderTextInput(
                    inputId = "zoomslider",
                    label = "Level of Aggregation:",
                    grid = T,
                    force_edges = TRUE,
                    choices = c("Individual Points",
                                "Neighborhoods", "Districts")
                  )
                )
              )
        )
    )
  )
)


## server ----------------------------------------------------------------------

server <- function(input, output) {
  
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
  
  counts <- reactive({
    filt_data() %>% 
      count(full_date) %>% 
      select(n) %>% 
      pull()
  })
  
  full_with_marker <- reactive({
    data_full %>% 
      mutate(selected = if_else(full_date == viewed(), 1, 0))
  })
  
  plot_static <- reactive({
    map %>% 
      ggplot() +
      geom_hex(data = filt_data(), aes(lon, lat), inherit.aes = F, bins = as.numeric(input$bins)) +
      geom_sf(fill = NA, color = "grey50") +
      scale_fill_gradient(
        low = "white",
        high = "#60c957"
      ) +
      theme_void() +
      labs(
        caption = glue("Last updated: {top}"),
        fill = glue('  Number of free   \n scooters on \n {format(viewed(), "%B %d, at %H:%M %p")} \n in San Francisco')
      ) +
      theme(
        plot.caption = element_text(family = "Roboto Mono", hjust = 1, size = 12 ),
        legend.position = "left",
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 9, family = "Roboto Mono"),
        legend.title = element_text(size = 15, family = "Roboto Mono")
      ) +
      guides(fill = guide_colorbar(title.position = "left"))
    
  })
  
  output$plot1 <- renderPlot({
  
    plot_static()
    
  })
  
  output$plot2 <- renderPlot({
    
    full_with_marker() %>% 
      count(full_date) %>% 
      ggplot(aes(x = full_date, y = n, fill = ifelse(full_date == viewed(), "yes", "no"))) +
      geom_col(show.legend = F) +
      annotate(
        "text", y = 800, x = viewed(),
        label = counts(),
        family = "Roboto Mono",
        color = "#fcfcfc",
        size = 3.3
        ) +
      scale_fill_manual(
        values = c("#344463","#6d81a8")
      ) +
      labs(
        x = "Time",
        y = 'Count'
      ) +
      theme_minimal() +
      theme(
        axis.title.x = element_text(family = "Roboto Mono"),
        axis.title.y = element_text(family = "Roboto Mono"),
        axis.text.x = element_text(family = "Roboto Mono"),
        axis.text.y = element_text(family = "Roboto Mono")
      ) 
    
  }, height = 200)
  
  static_plot <- reactive({
    
    if (input$zoomslider == "Individual Points") {
      
      leaflet(options = leafletOptions(minZoom = 11, maxZoom = 14)) %>% 
        addProviderTiles(providers$MtbMap,
                         options = providerTileOptions(opacity = 0.35)) %>%
        addProviderTiles(providers$Stamen.TonerLines,
                         options = providerTileOptions(opacity = 0.35)) %>% 
        addProviderTiles(providers$Stamen.TonerLabels) %>% 
        addCircleMarkers(lng = ~lon, lat = ~lat,
                         data = data_full, radius = 1,
                         color = "#60c957", opacity = 0.6)
      
    } else if (input$zoomslider == "Neighborhoods") {
      
      pal <- colorNumeric(palette = "Oranges", domain = neibs$n)
      labl <- paste0(neibs$name, "<br>", "scooters: ", neibs$n)
      
      neibs %>% 
        leaflet(options = leafletOptions(minZoom = 11, maxZoom = 14)) %>% 
        addProviderTiles(providers$MtbMap,
                         options = providerTileOptions(opacity = 0.35)) %>%
        addProviderTiles(providers$Stamen.TonerLines,
                         options = providerTileOptions(opacity = 0.35)) %>% 
        addPolygons(color = ~pal(n), fillOpacity = 1.0, popup = labl) %>% 
        addPolylines(color = "black", weight = 1.5) %>% 
        addLegend("bottomright", pal = pal, 
                  values = ~n, title = "Number of scooters",
                  opacity = 1)
      
    } else if (input$zoomslider == "Districts") {
      
      pal <- colorNumeric(palette = "Oranges", domain = districts$n)
      labl <- paste0("District ", districts$supervisor, "<br>", "scooters: ", districts$n)
      
      districts %>% 
        leaflet(options = leafletOptions(minZoom = 11, maxZoom = 14)) %>% 
        addProviderTiles(providers$MtbMap,
                         options = providerTileOptions(opacity = 0.35)) %>%
        addProviderTiles(providers$Stamen.TonerLines,
                         options = providerTileOptions(opacity = 0.35)) %>% 
        addPolygons(color = ~pal(n), fillOpacity = 1.0, popup = labl) %>% 
        addPolylines(color = "black", weight = 1.5) %>% 
        addLegend("bottomright", pal = pal, 
                  values = ~n, title = "Number of scooters",
                  opacity = 1)
      
    }
  })
  
  output$plot3 <- renderLeaflet({
    static_plot()
  })
  
}


## create app ------------------------------------------------------------------

shinyApp(ui, server)



