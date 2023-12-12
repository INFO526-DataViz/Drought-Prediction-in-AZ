# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(thematic)
library(ragg)
library(readr)
library(stringr)
library(shinydashboard)
library(leaflet)
library(plotly)
library(readxl)
library(tidyr)
library(sf)
library(dplyr)
library(geojsonio)

# Load data --------------------------------------------------------------------

#load dataset

drought_df <- read_excel("data/drought_data.xlsx", sheet = "Total Area by County")


# Data-preprocessing: drought data-----------------------------------------------

# Preprocess the data to obtain Year
drought_df <- drought_df %>%
  mutate(Year = format(as.Date(Date), "%Y"))

# Calculate the yearly average for each drought level including 'None'
yearly_data <- drought_df %>%
  group_by(Year, County) %>%
  summarise(across(c(None:D4), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = c(None:D4), names_to = "Drought_Level", values_to = "Value")

# Normalize the values to percentages
yearly_data <- yearly_data %>%
  group_by(Year, County) %>%
  mutate(Percentage = Value / sum(Value) * 100) %>%
  ungroup()

# Define a color palette
drought_colors <- c(
  "None" = "#FFFFFF", # Assuming 'None' is white
  "D0" = "#FFFF00", # Yellow
  "D1" = "#FFA500", # Orange
  "D2" = "#FF0000", # Red
  "D3" = "#8B0000", # Dark Red
  "D4" = "#654321"  # Brown
)

# Load GeoJSON data for Arizona counties
arizona_counties <- st_read("data/arizona-with-county-boundaries_1085.geojson")

# Define UI --------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Drought in Arizona (2000-Present)"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      leafletOutput("map"),
      box(width = 10, plotOutput("barplot"),
          box(width = 10, plotOutput("donutplot")))
      
    )
  )
)


# Define server function --------------------------------------------

server <- function(input, output){
  
  # Reactive value to store the selected county
  selected_county <- reactiveVal(NULL)
  
  # Update selected county on map click
  observe({
    click_latlng <- input$map_shape_click
    if (!is.null(click_latlng)) {
      # Create an sf point object with the correct CRS
      click_point <- st_sfc(st_point(c(click_latlng$lng, click_latlng$lat)), crs = st_crs(arizona_counties))
      
      # Find intersection between point and counties
      intersecting_counties <- st_intersection(arizona_counties, click_point)
      
      # Take the first intersecting county (you might want to handle multiple results differently)
      selected_county(intersecting_counties$name)
    }
  })
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = -111.6646, lat = 34.0489, zoom = 6) %>%
      addTiles() %>%
      addPolygons(data = arizona_counties,
                  group = "counties",
                  options = pathOptions(weight = 0.5, color = "white", fillOpacity = 1, fillColor = "orange"),
                  label = ~name
      ) %>%
      addLabelOnlyMarkers(data = arizona_counties,
                          lng = ~st_coordinates(st_centroid(geometry))[1],
                          lat = ~st_coordinates(st_centroid(geometry))[2],
                          label = ~name, labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE))
  })
  
  output$barplot <- renderPlot({
    if (!is.null(selected_county())) {
      yearly_data_filtered <- filter(yearly_data, County == selected_county())
    }
    else {
      yearly_data_filtered <- yearly_data
    }
    ggplot(yearly_data_filtered, aes(x = Year, y = Percentage, fill = Drought_Level)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_fill_manual(values = drought_colors) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(title = "Average Yearly Drought Levels", x = "Year", y = "Percentage", fill = "Drought Level") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  })
  
  
  
  output$donutplot <- renderPlot({
    drought_filtered <- drought_df
    if (!is.null(selected_county())) {
      drought_filtered <- filter(drought_df, County == selected_county())
    }
    
    drought_totals <- drought_filtered %>% 
      summarise(D0 = sum(D0, na.rm = TRUE),
                D1 = sum(D1, na.rm = TRUE),
                D2 = sum(D2, na.rm = TRUE),
                D3 = sum(D3, na.rm = TRUE),
                D4 = sum(D4, na.rm = TRUE),
                None = sum(None, na.rm = TRUE)) %>%
      pivot_longer(cols = everything(), names_to = "Drought_Level", values_to = "Area")
    
    # Calculate percentages
    total_area <- sum(drought_totals$Area)
    drought_totals <- drought_totals %>%
      mutate(Percentage = Area / total_area * 100)
    
    ggplot(drought_totals, aes(x = 2, y = Percentage, fill = Drought_Level)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      scale_fill_manual(values = drought_colors) +
      geom_text(aes(label = sprintf("%0.2f%%", Percentage)), position = position_stack(vjust = 0.5)) +
      xlim(c(0.15, 2.5))
    
  })
}


# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)