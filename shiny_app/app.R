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

# a helper function to grab counts for a given sf object

get_counts <- function(sf_obj) {
  
  counted <- data.frame()
  
  unique_dates <- data_full %>% 
    distinct(full_date)
  
  for (i in 1:nrow(unique_dates)) {
    
    filt <- data_full %>% 
      filter(full_date == unique_dates$full_date[[i]])
    
    coords <- filt %>% 
      select(lon, lat) %>% 
      st_as_sf(coords = c(1,2))
    
    coords_new <- coords %>% 
      st_set_crs(st_crs(sf_obj))
    
    sf_obj$n <- lengths(st_intersects(sf_obj$geometry, coords_new))
    
    sf_obj$datt <- unique_dates$full_date[[i]]
    
    counted <- rbind(counted, sf_obj)
    
    
  }
  
  counted %>% 
    as_tibble() %>% 
    select(-geometry)
  
}

neib_counts <- get_counts(neibs)

districts_counts <- get_counts(districts)
                          



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
                column(width = 9,
                  box(
                    solidHeader = T,
                    leafletOutput("plot3"),
                    width = NULL
                  ),
                  box(
                    solidHeader = T,
                    status = "primary",
                    sliderTextInput(
                      inputId = "zoomslider",
                      label = "Level of Aggregation:",
                      grid = T,
                      force_edges = T,
                      choices = c("Individual Points",
                                  "Neighborhoods", "Districts"),
                      hide_min_max = T
                    ), width = NULL
                  )
                ),
                column(width = 3,
                  box(
                    solidHeader = T,
                    selectInput("plotType", "Use Time Slider?",
                                c(Yes = "yes", No = "no"),
                                selected = "no"),
                    conditionalPanel(
                      condition = "input.plotType == 'yes'",
                      knobInput(
                        inputId = "Knob2",
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
                      )
                    ), width = NULL
                  ),
                  box(
                    solidHeader = T,
                    conditionalPanel(
                      condition = "(input.zoomslider == 'Neighborhoods' & input.plotType == 'no') 
                      || (input.zoomslider == 'Districts' & input.plotType == 'no')",
                      plotOutput("tester")
                      ),
                    width = NULL
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
  
  filt_data_detail <- reactive({
    
    date <- max(data_full$full_date)
    hour(date) <- hour(date) - input$Knob2 
    
    data_full %>% 
      filter(full_date == date)
    
  })   
  
  viewed <- reactive({
    date <- max(data_full$full_date)
    hour(date) <- hour(date) - input$Knob 
    date
  })
  
  viewed_detail <- reactive({
    
    if (input$plotType == "yes") {
      date <- max(data_full$full_date)
      hour(date) <- hour(date) - input$Knob2
      date
    } else {
      string = "Over the last 24 Hours"
      string
    }
    
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
  
  selected_zone <- reactive({
    
    p <- input$plot3_shape_click
    
    if (input$zoomslider == "Neighborhoods") {
      
      if (is.null(p)) {
        neib_counts
      } else {
        neib_counts %>% 
          filter(name == p$id)
      }
      
    } else if (input$zoomslider == "Districts") {
      
      if (is.null(p)) {
        districts_counts
      } else {
        districts_counts %>% 
          filter(supervisor == p$id)
      }
      
    } 
  
    
  })
  
  output$tester <- renderPlot({
    
    selected_zone() %>% 
      ggplot(aes(x = n)) +
      geom_density(fill = "midnightblue") +
      labs(
        x = "Number of scooters",
        title = "Distribution of scooters over the last 23 hours"
      ) +
      theme_minimal()
    
  })
  
  
  
  leaf_reactive <- reactive({
    
    if (input$zoomslider == "Individual Points" & input$plotType == "yes") {
      
      filt_data_detail() 
      
    } else if (input$zoomslider == "Individual Points" & input$plotType == "no") {
      
      data_full
      
    } else if (input$zoomslider == "Neighborhoods" & input$plotType == "yes") {
      
      coords <- filt_data_detail() %>% 
        select(lon, lat) %>%
        st_as_sf(coords = c(1,2))
      
      coords_new <- coords %>% 
        st_set_crs(st_crs(neibs))
      
      neibs$n <- lengths(st_intersects(neibs$geometry, coords_new))
      
      neibs
    } else if (input$zoomslider == "Neighborhoods" & input$plotType == "no") {
      
      coords <- data_full %>% 
        select(lon, lat) %>%
        st_as_sf(coords = c(1,2))
    
      coords_new <- coords %>% 
        st_set_crs(st_crs(neibs))
    
      neibs$n <- lengths(st_intersects(neibs$geometry, coords_new))
      
      neibs
      
    } else if (input$zoomslider == "Districts" & input$plotType == "yes") {
      
      coords <- filt_data_detail() %>% 
        select(lon, lat) %>%
        st_as_sf(coords = c(1,2))
      
      coords_new <- coords %>% 
        st_set_crs(st_crs(districts))
      
      districts$n <- lengths(st_intersects(districts$geometry, coords_new))
      
      districts
      
    } else if (input$zoomslider == "Districts" & input$plotType == "no") {
    
      coords <- data_full %>% 
        select(lon, lat) %>%
        st_as_sf(coords = c(1,2))
    
      coords_new <- coords %>% 
        st_set_crs(st_crs(districts))
    
      districts$n <- lengths(st_intersects(districts$geometry, coords_new))
      
      districts
    
    }
      
  })
  
  reactive_label <- reactive({
    
    if (input$zoomslider == "Neighborhoods" & input$plotType == "yes") {
      
      labl <- paste0("Neighborhood: ", leaf_reactive()$name,"<br><br>",
                     leaf_reactive()$n, " scooters on ", "<br>",
                     {format(viewed_detail(), "%B %d, at %H:%M %p")} )
      
    } else if (input$zoomslider == "Neighborhoods" & input$plotType == "no") {
      
      labl <- paste0("Neighborhood: ", leaf_reactive()$name, "<br><br>",
                     leaf_reactive()$n, " scooters over the last 24 hours")
      
    } else if (input$zoomslider == "Districts" & input$plotType == "yes") {
      
      labl <- paste0("District ", leaf_reactive()$name,"<br><br>",
                     leaf_reactive()$n, " scooters on ", "<br>",
                     {format(viewed_detail(), "%B %d, at %H:%M %p")} )
      
    } else if (input$zoomslider == "Districts" & input$plotType == "no") {
      
      labl <- paste0("District ", leaf_reactive()$name, "<br><br>",
                     leaf_reactive()$n, " scooters over the last 24 hours")
      
    } else if (input$zoomslider == "Individual Points" & input$plotType == "yes") {
      
      labl <- paste0("Date/Time: ", {format(viewed_detail(), "%B %d, at %H:%M %p")} )
      
    } else if (input$zoomslider == "Individual Points" & input$plotType == "no") {
      
      labl <- paste0("Points over the last 24 hours")
      
    }
    
    labl
    
  })
  
  static_plot <- reactive({
    
    if (input$zoomslider == "Individual Points") {
      
      labl <- reactive_label()
      
      leaflet(options = leafletOptions(minZoom = 11, maxZoom = 14)) %>% 
        addProviderTiles(providers$MtbMap,
                         options = providerTileOptions(opacity = 0.35)) %>%
        addProviderTiles(providers$Stamen.TonerLines,
                         options = providerTileOptions(opacity = 0.35)) %>% 
        addProviderTiles(providers$Stamen.TonerLabels) %>% 
        addCircleMarkers(lng = ~lon, lat = ~lat,
                         data = leaf_reactive(), radius = 1,
                         popup = labl,
                         color = "#ad3e0e", opacity = 0.3)
      
    } else if (input$zoomslider == "Neighborhoods") {
      
      pal <- colorNumeric(palette = "Oranges", domain = leaf_reactive()$n)
      labl <- reactive_label()
      
      
      leaf_reactive() %>% 
        leaflet(options = leafletOptions(minZoom = 11, maxZoom = 14)) %>% 
        addProviderTiles(providers$MtbMap,
                         options = providerTileOptions(opacity = 0.35)) %>%
        addProviderTiles(providers$Stamen.TonerLines,
                         options = providerTileOptions(opacity = 0.35)) %>% 
        addPolygons(color = ~pal(n), fillOpacity = 1.0, popup = labl, layerId = ~name) %>% 
        addPolylines(color = "black", weight = 1.5) %>% 
        addLegend("bottomright", pal = pal, 
                  values = ~n, title = "Number of scooters",
                  opacity = 1)
      
    } else if (input$zoomslider == "Districts") {
      
      pal <- colorNumeric(palette = "Oranges", domain = leaf_reactive()$n)
      labl <- reactive_label()
      
      leaf_reactive() %>% 
        leaflet(options = leafletOptions(minZoom = 11, maxZoom = 14)) %>% 
        addProviderTiles(providers$MtbMap,
                         options = providerTileOptions(opacity = 0.35)) %>%
        addProviderTiles(providers$Stamen.TonerLines,
                         options = providerTileOptions(opacity = 0.35)) %>% 
        addPolygons(color = ~pal(n), fillOpacity = 1.0, popup = labl, layerId = ~supervisor) %>% 
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



