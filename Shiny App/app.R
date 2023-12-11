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
  group_by(Year) %>%
  summarise(across(c(None:D4), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = c(None:D4), names_to = "Drought_Level", values_to = "Value")

# Normalize the values to percentages
yearly_data <- yearly_data %>%
  group_by(Year) %>%
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

# Define UI --------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Drought in Arizona (2000-Present)"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(width = 10, plotOutput("barplot"),
      box(width = 10, plotOutput("donutplot")) )
    )
  )
)


# Define server function --------------------------------------------

server <- function(input, output){
  
  output$barplot <- renderPlot({
    ggplot(yearly_data, aes(x = Year, y = Percentage, fill = Drought_Level)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_fill_manual(values = drought_colors) +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(title = "Average Yearly Drought Levels", x = "Year", y = "Percentage", fill = "Drought Level") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  })
  
  
  
  output$donutplot <- renderPlot({
    
    # Assuming 'drought_df' has columns 'D0', 'D1', 'D2', 'D3', 'D4', and 'None'
    drought_totals <- drought_df %>% 
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