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
map <- st_read('sf_boundary1.shp')
neibs <- st_read('sf_neibs.shp')
districts <- st_read("sf_districts.shp")
top <- max(data_full$last_updated)
top <- with_tz(top, "America/Los_Angeles")



neibs <- neibs %>% 
  mutate(id = row_number())

districts <- districts %>% 
  mutate(id = row_number())

## fresh theme -----------------------------------------------------------------

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#717b85"
  ),
  adminlte_sidebar(
    width = "330px",
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

# some data objects for leaflet plots

coords <- data_full %>% 
  select(lon, lat) %>% 
  st_as_sf(coords = c(1,2))

coords <- coords %>% 
  st_set_crs(st_crs(districts))

indices_dists <- st_intersects(districts$geometry, coords)
indices_neibs <- st_intersects(neibs$geometry, coords)


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
      id = 'tabs',
      menuItem("About", tabName = "abt", icon = icon("question-circle")),
      menuItem("Trends", tabName = "trends", icon = icon("poll")),
      menuItem("In Detail", tabName = "detail", icon = icon("binoculars"))
    ),
    htmlOutput('res')
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
                font-weight: 850;
              }
              h3 {
                font-family: 'Roboto Mono', sans-serif;
                font-weight: 1000;
              }
              p {
                font-family: 'Roboto Mono', sans-serif;
                font-weight: 850;
                font-size: 12px;
              }"))
          ),
          box(
            width = 12,
            h2("About the Dashboard"),
            p("The main purposes of this dashboard are largely exploratory, as it aims to to provide a way to explore and visualize the geolocations of 
              lime scooters across San Francisco, California. The dashboard is split into two main components, a 'Trends' tab that 
              shows the general distribution and density of lime scooters across space, and an 'In detail' tab that allows the user
              to look at the actual exact locations across San Francisco as well as some aggregation by neighborhood and district. Each
              tab includes a temporal component where the user can interact with a slider that controls how far back in time you want to look."),
            h3("Collecting the Data"),
            p("Wonderfully, there is a plethora of Bikeshare data released through the 'General Bikeshare Feed Specification' which is
              a standardized data feed. Even more wonderfully, there is a package in R written by Simon Couch, fittingly called", tags$a(href = "https://github.com/simonpcouch/gbfs", "gbfs") 
              ,",that makes pulling the data into R very easy. That being said, when pulling bikeshare data what you get is the data from when it was last updated, but nothing from previous updates. 
              Since I wanted to look at how lime scooter density changed over time I had to figure out a way to accumulate the bikeshare data. Furthermore, there are thousands of lime scooters
              scattered across San Francisco, so when pulling data every hour you very quickly get an extremely large dataset. I decided to limit how far back I wanted to keep data from to 24 hours,
              since I thought many interesting things could be gleaned from that amount of time and I wanted to avoid having a sluggish shiny app. So, in the end the main task was to find a way
              to pull and accumulate bikeshare data every hour, while making sure to only keep data from the last 24 hours."),
            p("I decided to use github actions to run a script on a schedule to accomplish this. Simply put, my script loads in the most recent update
              of the bikshare data, binds it to my existing data set and then filters the joined data set to only include observations with timestamps in the last 24 hours. The repository where this
              is running is public and can be found", tags$a(href = "https://github.com/joshyam-k/schPull", "here"))
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
                      label = "Number of bins:", 
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
                      condition = "input.zoomslider == 'Neighborhoods'",
                      leafletOutput('smallmap')
                    ),
                    conditionalPanel(
                      condition = "input.zoomslider == 'Districts' && input.plotType == 'no'",
                      leafletOutput('smallmap2')
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
      filter(full_date == date) %>% 
      filter(lat < 37.85, lat > 37.69) %>% 
      filter(lon > -122.54, lon < -122.32)
    
  })   
  
  filt_data_detail <- reactive({
    
    date <- max(data_full$full_date)
    hour(date) <- hour(date) - input$Knob2 
    
    data_full %>% 
      filter(full_date == date) %>% 
      filter(lat < 37.85, lat > 37.69) %>% 
      filter(lon > -122.54, lon < -122.32)
    
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
      geom_bin2d(data = filt_data(), aes(lon, lat), inherit.aes = F, bins = as.numeric(input$bins)) +
      geom_sf(fill = NA, color = "grey50") +
      scale_fill_gradient(
        low = "white",
        high = "#149908"
      ) +
      theme_void() +
      labs(
        caption = glue("Last updated: {top}"),
        fill = glue('  Number of free   \n scooters on \n {format(viewed(), "%B %d, at around \n %H:%M %p")} in \n San Francisco')
      ) +
      theme(
        plot.caption = element_text(hjust = 1, size = 12 ),
        legend.position = "left",
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 15)
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
  
  output$res <- renderUI({
    
    if (input$tabs == 'abt') {
      
      text <- " "
      
    } else if (input$tabs == 'trends') {
      
      text <- HTML(paste(p('This tab primarily serves to help the user visualize how lime scooter density across the entirety of San Francisco changes over time.
               In the primary map, space is divided into Square bins. Square areas with lime scooters in them are colored green and the shade of
               green corresponds to how many scooters are in that area. The user also has 6 different options for how many bins they want the space divided into
               with higher values resulting in finer density detail.'), p('The user can also change how far back in time they want to look using the slider on the right.
               the page defaults to showing the data from when it was most recently updated, but the user can go as far as 24 hours back to see what the data looked like then.'),
               p('Finally the barchart at the bottom shows how many lime scooters there were in total in San Francisco by the hour and the bar corresponding to the 
               time chosen by the user is highlighted.'), p('One final note is that for reasons unknown to me, my script scraping the data will occasionally fail, resulting in an hour which 
               my app has no data for.')))
      
    } else if (input$tabs == 'detail') {
      
      text <- HTML(paste(p('This tab functions similarly to the trends tab, but now the level of aggregation can be chosen by the user.'),
                         p('Again the user can use the time slider, but this time must select whether they want to use it or not. The reason for this
                           becomes apparent when choosing a new level of aggregation. When the user selects either "Neighborhoods", or "Districts" a density plot
                           pops up showing the distribution of scooter counts over the last 24 hours, this plot can only be accessed when the time slider is not being used'),
                         p('When looking at the data at a Neighborhood or District level the user can also click on a region on the map and the density plot will become
                           specific to that neighborhood, and the smaller map on the right will show the exact locations of the scooters in that region.')))
      
    }
    
    text
    
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
  
  selected_zone <- reactive({
    
    p <- input$plot3_shape_click
    
    if (input$zoomslider == "Neighborhoods") {
      
      if (is.null(p)) {
        
        neib_counts
        
      } else {
        
        res <- try(filter(neib_counts, name == p$id),silent = TRUE)
        
        if (class(res) == 'try-error') {
          
          neib_counts
          
        } else {
          
          neib_counts %>% 
            filter(name == p$id)
          
        }  
      }
      
    } else if (input$zoomslider == "Districts") {
      
      if (is.null(p)) {
        
        districts_counts
        
      } else {
        
        res <- try(filter(districts_counts, supervisor == p$id),silent = TRUE)
        
        if (class(res) == 'try-error') {
          
          districts_counts
          
        } else {
          
          districts_counts %>% 
            filter(supervisor == p$id)
          
        }
      }
      
    } 
  
    
  })
  
  density_title <- reactive({
    
    p <- input$plot3_shape_click
    
    if (input$zoomslider == 'Districts') {
      
      if (is.null(p)) {
        
        title <- 'Distribution of number of scooters over the last 24 hours \n in San Francisco'
        
      } else {
        
        res <- try(selected_zone()$supervisor[[1]], silent = T)
        
        if (class(res) == 'try-error') {
          
          title <- 'Distribution of number of scooters over the last 24 hours \n in San Francisco'
          
        } else {
        
          title <- glue('Distribution of number of scooters over the last 24 hours \n in District {selected_zone()$supervisor[[1]]}')
        }
      }
      
      
    } else if (input$zoomslider == 'Neighborhoods') {
      
      if (is.null(p)) {
        
        title <- 'Distribution of number of scooters over the last 24 hours \n in San Francisco'
        
      } else {
        
        res <- try(selected_zone()$name[[1]], silent = T)
        
        if (class(res) == 'try-error') {
          title <- 'Distribution of number of scooters over the last 24 hours \n in San Francisco'
        } else {
          
          title <- glue('Distribution of number of scooters over the last 24 hours \n in {selected_zone()$name[[1]]}')
        }
        
        
        
      }
      
    }
    
    title
    
  })
  
  output$tester <- renderPlot({
    
    selected_zone() %>% 
      ggplot(aes(x = n)) +
      geom_density(fill = "midnightblue") +
      labs(
        x = "Number of scooters",
        title = density_title()
      ) +
      theme_minimal()
    
  }, height = 220)
  
  small_mapzone <- reactive({
    
    p <- input$plot3_shape_click
    
    if (input$zoomslider == 'Neighborhoods'){
      
      if (is.null(p)) {
        
        neibs 
        
      } else {
        
        tryCatch(
          error = function(cnd){
            neibs
          }, 
          {
            neibs %>% 
              filter(name == p$id)
          }
        
       )
      } 
      
    } else if (input$zoomslider == 'Districts') {
      
      if (is.null(p)) {
        
        districts
        
      } else {
        
        tryCatch(
          error = function(cnd){
            districts
          }, 
          {
            districts %>% 
              filter(supervisor == p$id)
          }
          
        )
      }
      
    }
    
  })
  
  smallzone_points <- reactive({
    
    
    if (input$zoomslider == 'Neighborhoods') {
      
      
      if (nrow(small_mapzone()) > 1 ) {
        
        points <- data_full 
        
      } else {
        
        res <- try(small_mapzone()$id[[1]],silent = TRUE)
        
        if (class(res) == "try-error") {
          
          points <- data_full 
          
        } else {
        
          index <- small_mapzone()$id[[1]]
          points <- data_full[c(indices_neibs[[index]]), ]
        
          
        }
      }
    
    } else if (input$zoomslider == 'Districts') {
      
      if (nrow(small_mapzone()) > 1) {
        
        points <- data_full
         
      } else {
        
        res <- try(small_mapzone()$id[[1]],silent = TRUE)
        
        if (class(res) == "try-error") {
          
          points <- data_full 
          
        } else {
          
          index <- small_mapzone()$id[[1]]
          points <- data_full[c(indices_dists[[index]]), ]
          
        }
      }
      
    } 
  
    points
    
  })

  
  output$smallmap <- renderLeaflet({
    
    small_mapzone() %>% 
      leaflet(options = leafletOptions(minZoom = 11, maxZoom = 14, attributionControl = F)) %>% 
      addProviderTiles(providers$MtbMap,
                       options = providerTileOptions(opacity = 0.15)) %>%
      addPolylines(color = "red", weight = 4) %>% 
      addProviderTiles(providers$Stamen.TonerLines,
                       options = providerTileOptions(opacity = 0.85)) %>% 
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
      addCircleMarkers(lng = ~lon, lat = ~lat,
                       data = smallzone_points(), radius = 1, color = "#191970")
    
  })
  
  output$smallmap2 <- renderLeaflet({
    
    small_mapzone() %>% 
      leaflet(options = leafletOptions(minZoom = 11, maxZoom = 14, attributionControl = F)) %>% 
      addProviderTiles(providers$MtbMap,
                       options = providerTileOptions(opacity = 0.15)) %>%
      addPolylines(color = "red", weight = 4) %>% 
      addProviderTiles(providers$Stamen.TonerLines,
                       options = providerTileOptions(opacity = 0.85)) %>% 
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
      addCircleMarkers(lng = ~lon, lat = ~lat, color = "#191970",
                       data = smallzone_points(), radius = 1)
    
  })
  
  
  
  reactive_label <- reactive({
    
    if (input$zoomslider == "Neighborhoods" & input$plotType == "yes") {
      
      labl <- paste0("<b>Neighborhood: ", leaf_reactive()$name,"</b><br><br>",
                     leaf_reactive()$n, " scooters on ", "<br>",
                     {format(viewed_detail(), "%B %d, at %H:%M %p")} )
      
    } else if (input$zoomslider == "Neighborhoods" & input$plotType == "no") {
      
      labl <- paste0("<b>Neighborhood: ", leaf_reactive()$name, "</b><br><br>",
                     leaf_reactive()$n, " scooters over the last 24 hours")
      
    } else if (input$zoomslider == "Districts" & input$plotType == "yes") {
      
      labl <- paste0("<b>District ", leaf_reactive()$supervisor,"</b><br><br>",
                     leaf_reactive()$n, " scooters on ", "<br>",
                     {format(viewed_detail(), "%B %d, at %H:%M %p")} )
      
    } else if (input$zoomslider == "Districts" & input$plotType == "no") {
      
      labl <- paste0("<b>District ", leaf_reactive()$supervisor, "</b><br><br>",
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



