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
percent_drought_df <- read_excel("data/drought_data.xlsx", sheet = "Percent Area by County")

# Load GeoJSON data for Arizona counties
arizona_counties <- st_read("data/arizona-with-county-boundaries_1085.geojson")


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

#Rename Column from name to County
arizona_counties_new <- dplyr::rename(arizona_counties, County = name)

#Drop all columns ecept County and geometry
arizona_counties_new <- arizona_counties_new %>%
  select(County, geometry)

#Convert to req data types
percent_drought_df$County <- as.character(percent_drought_df$County)
arizona_counties_new$County <- as.character(arizona_counties_new$County)
percent_drought_df$Date<- as.Date(percent_drought_df$Date)

drought_data_geospatial <- percent_drought_df %>%
  left_join(arizona_counties_new, by = "County")

# Assign weights to the drought levels
drought_weights <- c(None = 0, D0 = 1, D1 = 2, D2 = 3, D3 = 4, D4 = 5)

# Calculate 'Drought Score' by multiplying the area percentages by the weights
drought_data_geospatial <- drought_data_geospatial %>%
  mutate(Drought_Score = (None * drought_weights[["None"]]) +
           (D0 * drought_weights[["D0"]]) +
           (D1 * drought_weights[["D1"]]) +
           (D2 * drought_weights[["D2"]]) +
           (D3 * drought_weights[["D3"]]) +
           (D4 * drought_weights[["D4"]]))

# Normalize 'Drought Score' 
# Assuming that the maximum possible score is 5 (for D4 at 100% coverage)
drought_data_geospatial <- drought_data_geospatial %>%
  mutate(Normalized_Score = Drought_Score / max(Drought_Score))

#Make sure your date column is type date
drought_data_geospatial$Date <- as.Date(drought_data_geospatial$Date)


# Define color palette----------------------------------------------------------

drought_colors <- c(
  "None" = "#FFFFFF", # Assuming 'None' is white
  "D0" = "#FFFF00", # Yellow
  "D1" = "#FFA500", # Orange
  "D2" = "#FF0000", # Red
  "D3" = "#8B0000", # Dark Red
  "D4" = "#654321"  # Brown
)

# Define UI --------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Drought in Arizona"),
  dashboardSidebar(disable = TRUE), # Disable the sidebar if not needed
  dashboardBody(
    tags$head(
      tags$style(HTML("
      
                         /* Change the header background color */
                        .skin-blue .main-header .logo {
                          background-color: #5C4033; /* Dark brown */
                            color: #ffffff; /* White text color */
                        }
                      .skin-blue .main-header .navbar {
                        background-color: #5C4033; 
                      }
                      /* Change header menu text color to white */
                        .skin-blue .main-header li a {
                          color: #ffffff; /* White text color */
                        }
                        
                        .box {
          border: 1px solid #5C4033; /* Dark brown border for boxes */
          background-color: #FFFFFF; /* White background for boxes */
          border-radius: 5px;
          box-shadow: 0 1px 1px rgba(0, 0, 0, 0.1);
          margin-bottom: 20px;
                        }
                        
                        
        .leaflet-title {
          padding: 5px 10px;
          font-size: 18px;
          text-align: left;
          font-weight: bold;
          color: #333; /* You can change the color as needed */
        }
                        
                      "))
    ),
    
    fluidRow(
      column(width = 4,
             div(class = "leaflet-title", "Choose a County"),
             box(width = NULL, leafletOutput("map", height = "250px"), style = "overflow: hidden;")
      ),
      column(width = 8,
             hr(),
             box(plotOutput("barplot", height = "250px")),
             box(plotOutput("donutplot", height = "250px")) # Adjust height as needed
      )
    ),
    br(), # Line break
    fluidRow(width =4, 
      column(width = 8, dateInput("dateInput", "Choose a Date", value = Sys.Date())),
      box(plotOutput("droughtMap", height = "250px")),
      box(plotlyOutput("timeseriesplot", height = "250px"))
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
  
  #Render Barplot
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
  
  #Render Donut plo
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
  
  
  #Render Drought Map
  output$droughtMap <- renderPlot({
    # Filter the data based on the selected date from the dateInput
    drought_data_filtered <- drought_data_geospatial %>%
      filter(Date == as.Date(input$dateInput))
    
    
      # Plot the map with ggplot2 and sf
    ggplot(data = drought_data_filtered) +
      geom_sf(data = drought_data_filtered$geometry,
              aes(fill = drought_data_filtered$Normalized_Score), color = "black", size = 0.2) +
      scale_fill_gradientn(
        colors = drought_colors,
        name = "Drought Level",
        breaks = c(0, 1, 2, 3, 4, 5), 
        labels = c("None", "D0", "D1", "D2", "D3", "D4"), # Specifying the labels for the breaks
        limits = c(0, 5), 
        oob = scales::oob_squish
      ) +
      labs(title = paste("Drought Levels in Arizona on", format(as.Date(input$dateInput), "%m/%d/%Y"))) +
      theme_minimal() +
      theme(
        legend.position = "right",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank()
      )
  })
  
  
  #Render Timeseries Plot
  output$timeseriesplot <- renderPlotly({
    drought_filtered <- drought_df
    
    if (!is.null(selected_county())) {
      drought_filtered <- filter(drought_df, County == selected_county())
    }
    
    county_data <- drought_filtered %>%
      group_by(Year, County) %>%
      summarise(across(c(None:D4), mean, na.rm = TRUE)) %>%
      pivot_longer(cols = c(None:D4), names_to = "Drought_Level", values_to = "Value") %>%
      mutate(Percentage = Value / sum(Value) * 100) %>%
      ungroup()
    
    county_data$Year <- as.Date(paste0(county_data$Year, "-01-01"))
    
    p <- ggplot(county_data, aes(x = Year, y = Percentage, fill = Drought_Level)) +
      geom_area(alpha = 0.7, stat = "identity") +
      scale_fill_manual(values = drought_colors) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      labs(x = "Year", y = "Percentage", title = paste("Drought Level over the years for", selected_county())) +
      theme_minimal() +
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.6, size = 12),
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 10)) +
      guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
    
    # Convert ggplot to plotly for animation
    p <- ggplotly(p, tooltip = "all", dynamicTicks = TRUE)
    
    
  })
  
}


# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)