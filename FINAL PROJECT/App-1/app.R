library(shiny)
library(shinyWidgets)
library(tidyverse)
library(geojsonio)
library(sf)
library(leaflet)
library(plotly)
library(htmltools)
library(bslib)

## github paths ##
wifis <- read.csv('https://raw.githubusercontent.com/josh1den/DATA-608/main/FINAL%20PROJECT/data/wifis.csv')
pre <- read.csv('https://raw.githubusercontent.com/josh1den/DATA-608/main/FINAL%20PROJECT/data/precincts.csv')
wifi.v.arrests <- read.csv('https://raw.githubusercontent.com/josh1den/DATA-608/main/FINAL%20PROJECT/data/wifivarrests.csv')
wifi.v.arrests$date <- as.Date(wifi.v.arrests$date, "%Y-%m-%d")
geojson <- 'https://raw.githubusercontent.com/josh1den/DATA-608/main/FINAL%20PROJECT/data/PolicePrecincts.geojson'
pre.geo <- geojson_sf(geojson)

# store borough coordinates from web
borough_coords <- data.frame(borough = c("Full Overview",
                                         "Manhattan",
                                         "Brooklyn",
                                         "The Bronx",
                                         "Queens",
                                         "Staten Island"),
                             longitude = c(-73.935242,
                                           -73.971321,
                                           -73.949997,
                                           -73.865433,
                                           -73.769417,
                                           -74.151535),
                             latitude = c(40.730610,
                                          40.776676,
                                          40.650002,
                                          40.837048,
                                          40.742054,
                                          40.579021),
                             zoom = c(10,
                                      12,
                                      12,
                                      12,
                                      12,
                                      12))

# store list of months
choices_month <- format(seq.Date(from = as.Date("2016-01-01"), by = "month",
                                                length.out = 37), "%b-%Y")

# set bins and color range
bins <- c(0, 135, 300, 400, 500, 700, 850, 1000, 1200)
pal <- colorBin("YlOrRd", domain = pre$arrests, bins = bins)

# load wifi icon 
wifi.Icon <- makeIcon(
  iconUrl = "wifi_icon.png",
  iconWidth = 10, iconHeight = 10
)

# build Shiny ui

ui <- fluidPage(
  
  # bootswatch theme
  theme = bs_theme(version = 4, bootswatch = "minty"),
  
  # Application title
  tags$h1("Arrests in Proximity to Free Wifi Kiosks"),
  
  leafletOutput("mymap"),
  p(),
  
  hr(), # horizontal rule
  
  fluidRow(
    column(
          7,
          fluidRow(
            plotlyOutput(outputId = "areaPlot"), 
            style = "height:400px")
          ),
          
    column(
      4, offset = 1, 
      verticalLayout(
        sliderTextInput(inputId = "date",
                        label = "Select Month:",
                        choices = choices_month),
        pickerInput(
          inputId = "borough",
          label = "Zoom to Borough",
          choices = c("Full Overview",
                      "Manhattan",
                      "Brooklyn",
                      "The Bronx",
                      "Queens",
                      "Staten Island"),
          selected = "Full Overview")
      )
    )
    )
  )

server <- function(input, output) {
  
  # reactive expression for the subset data (by date)
  
  hotspots <- reactive({
    stamp <- as.Date(paste("01", unlist(strsplit(input$date, ";")), sep="-"), "%d-%B-%Y")
    wifis[wifis$Activated <= stamp, ]
  })
  
  arrests <- reactive({
    stamp <- as.Date(paste("01", unlist(strsplit(input$date, ";")), sep="-"), "%d-%B-%Y")
    pre[pre$date <= stamp, ]
  })
  
  borough <- reactive({
    borough_coords[borough_coords$borough == as.character(input$borough), ]
  })
  
  output$areaPlot <- renderPlotly({
    plot_ly(wifi.v.arrests, x= ~date, y = ~wifi, name = 'wifi', type = 'scatter',
            mode = 'none', stackgroup = 'one', fillcolor = '#F5FF8D') |> 
            add_trace(y = ~arrests, name = 'arrests', fillcolor = '#50CB86') |>
            layout(title = 'Arrests and Wifi, 2016-2019',
                    xaxis = list(title = "",
                                 showgrid = FALSE),
                    yaxis = list(title = "Total",
                                 showgrid = FALSE,
                                 tickvals = list("", 5000, 10000, 15000, 20000, 25000, 30000),
                                 ticktext = list("", "5k", "10k", "15k", "20k", "25k", "30k"),
                                 tickmode = "array")) 
  })
  
  output$mymap <- renderLeaflet({
    
    leaflet(options = leafletOptions(minZoom = -30, maxZoom = 30)) |>
      addTiles() |>
      setView(lng = borough()$longitude, lat = borough()$latitude, zoom=borough()$zoom) |>
      addPolygons(data = pre.geo,
                  color = "white", fillColor = ~pal(arrests()$arrests),
                  weight = 2,
                  opacity = 1,
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions (
                    weight = 2,
                    color = "#800000",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = paste("Precinct:",
                                 arrests()$precinct,
                                 "<br>",
                                 "Arrests:",
                                 arrests()$arrests) |>
                    lapply(htmltools::HTML)) |>
      addMarkers(data = hotspots(), lat = ~ Latitude, lng = ~ Longitude, icon = wifi.Icon,
                           label = ~as.character(Neighborhood), group = "Show/Hide Wifi") |>
      addLayersControl(
        overlayGroups = "Show/Hide Wifi",
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      addLegend("bottomright", pal = pal, values = bins,
                title = "Arrests",
                opacity = 1)
  })

}

shinyApp(ui = ui, server = server)