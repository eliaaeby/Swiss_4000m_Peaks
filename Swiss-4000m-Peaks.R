library(rvest)
library(dplyr)
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)

# URL der Webseite
url <- "https://bestswiss.ch/4000er-schweiz"

# Webseite lesen
webpage <- read_html(url)

# Tabelle mit den Bergdaten extrahieren
mountain_table <- webpage %>%
  html_node("table") %>%
  html_table()

# Daten bereinigen und vorbereiten
mountain_data <- mountain_table %>%
  rename(Name = X1, Height = X2, Region = X3) %>%  # Umbenennen der Spalten
  mutate(Height = as.numeric(gsub("[^0-9]", "", Height)))

# Koordinaten explizit hinzufügen (Reihenfolge entspricht der auf der Webseite)
coordinates <- data.frame(
  Latitude = c(45.93676, 45.94156, 45.93606, 45.93333, 46.11686, 45.92789, 45.93333, 
               46.12449, 46.09194, 45.97625, 45.93799, 46.02000, 45.92300, 46.11694, 
               45.94369, 46.11139, 46.53750, 46.11444, 45.93333, 46.04444, 46.11972, 
               46.08113, 46.05816, 46.53922, 45.99483, 45.94136, 45.93800, 45.97513, 
               45.97513, 46.54722, 46.13306, 45.94369, 45.97513, 45.97513, 46.57722, 
               45.93800, 45.96306, 45.93490, 46.08333, 46.56639, 46.37861, 46.51806, 
               46.55333, 46.57361, 46.07778, 46.57139, 46.10583, 46.10583),
  Longitude = c(7.86739, 7.86932, 7.87153, 7.86667, 7.86272, 7.84915, 7.86667, 
                7.65227, 7.85319, 7.65830, 7.86888, 7.58000, 7.86200, 7.87722, 
                7.37875, 7.86917, 8.12222, 7.85694, 7.86667, 7.67306, 7.85889, 
                7.85068, 7.83287, 8.11639, 7.86967, 7.37336, 7.86600, 7.65889, 
                7.65889, 7.98028, 7.70750, 7.37875, 7.65889, 7.65889, 8.02500, 
                7.86600, 7.48250, 7.87722, 7.65472, 8.07278, 9.90250, 8.05833, 
                8.07222, 7.85694, 7.89722, 8.09972, 7.98500, 7.98500)
)

# Koordinaten zu den Bergdaten hinzufügen
mountain_data <- cbind(mountain_data, coordinates)

# Define UI
header <- dashboardHeader(title = "Swiss 4000m Peaks", titleWidth = 340)

sidebar <- dashboardSidebar(
  width = 340,
  sliderInput('heightRange', 'Select Peak Height (meters):', min = min(mountain_data$Height, na.rm = TRUE), max = max(mountain_data$Height, na.rm = TRUE), value = c(min(mountain_data$Height, na.rm = TRUE), max(mountain_data$Height, na.rm = TRUE))),
  selectInput('regionSelect', 'Select Region:', choices = c("All", unique(mountain_data$Region)), selected = "All")
)

body <- dashboardBody(
  leafletOutput('map', width = '100%', height = 'calc(100vh - 80px)'),
  absolutePanel(top = 10, left = 20, id = 'controls', class = 'panel panel-default', fixed = TRUE, draggable = TRUE, height = 'auto',
                tags$style(type = "text/css", "#controls {background-color: rgba(255,255,255,0.9); padding: 20px;}")),
  DT::dataTableOutput('dataTable')
)

ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output, session) {
  filteredData <- reactive({
    data <- mountain_data %>%
      filter(Height >= input$heightRange[1], Height <= input$heightRange[2])
    if (input$regionSelect != "All") {
      data <- data %>%
        filter(Region == input$regionSelect)
    }
    data
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = 8.2275, lat = 46.8182, zoom = 7) %>%
      addTiles() %>%
      addCircleMarkers(data = filteredData(),
                       ~Longitude, ~Latitude,
                       weight = 1, radius = ~log(Height) * 2,
                       popup = ~paste(Name, "<br>Height: ", Height, "m", "<br>Region: ", Region))
  })
  
  output$dataTable <- DT::renderDataTable({
    DT::datatable(filteredData())
  })
}

# Run the application 
shinyApp(ui, server)
