library(shiny)
library(shinyWidgets)
library(tidyverse)
library(geojsonio)
library(sf)
library(leaflet)

# read in data
## local paths ##
wifis <- read.csv('wifis.csv')
wifi.v.arrests <- read.csv('wifivarrests.csv')
wifi.v.arrests$date <- as.Date(wifi.v.arrests$date, "%Y-%m-%d")
pre <- read.csv('precincts.csv')
geojson <- 'PolicePrecincts.geojson'

## github paths ##
# wifis <- read.csv('https://raw.githubusercontent.com/josh1den/DATA-608/main/FINAL%20PROJECT/data/wifis.csv')
# pre <- read.csv('https://raw.githubusercontent.com/josh1den/DATA-608/main/FINAL%20PROJECT/data/precincts.csv')
# geojson <- 'https://raw.githubusercontent.com/josh1den/DATA-608/main/FINAL%20PROJECT/data/PolicePrecincts.geojson'
pre.geo <- geojson_sf(geojson)
precincts <- merge(pre, pre.geo) |> st_as_sf()

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
pal <- colorBin("YlOrRd", domain = precincts$arrests, bins = bins)

# load wifi icon 
wifi.Icon <- makeIcon(
  iconUrl = "wifi_icon.png",
  iconWidth = 12, iconHeight = 12
)

# build Shiny ui

ui <- fluidPage(
  
  # Application title
  tags$h1("Arrests in Proximity to Free Wifi Hotspots"),
  
  leafletOutput("mymap"),
  p(),
  
  hr(), # horizontal rule
  
  fluidRow(
    column(4,
          plotOutput(outputId = "areaPlot")
          ),
          
    column(4, 
      sliderTextInput(inputId = "date",
                label = "Select Month:",
                choices = choices_month),
    ),
    column(4,
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
    )#,
    # column(4,
    #        awesomeCheckbox(
    #          inputId = "wifibox",
    #          label = "Show/Hide Hotspots",
    #          value = FALSE,
    #          status = "info"
    #        ))
    # #   actionButton(inputId = "go",
    #              label = "Update")
    # )
  )

  # sidebar with slider
  # sidebarLayout(
  #   sidebarPanel(
  #     sliderInput(inputId = "date",
  #                 label = "Drag to Date",
  #                 min = as.Date("2016-01-01", "%Y-%m-%d"),
  #                 max = as.Date("2019-03-01", "%Y-%m-%d"),
  #                 value = as.Date("2016-01-01", "%Y-%m-%d")
  #                 )
  #     )
  # ),
  # 
  # mainPanel(
  #   leafletOutput("mymap"),
  #   p()
  # )
  # leafletOutput("mymap"),
  # p(),
  # sidebarLayout(position = "below",
  #   sidebarPanel(
  #     sliderInput(inputId = "date",
  #                             label = "Drag to Date",
  #                             min = as.Date("2016-01-01", "%Y-%m-%d"),
  #                             max = as.Date("2019-03-01", "%Y-%m-%d"),
  #                             value = as.Date("2016-01-01", "%Y-%m-%d")
  #                             ),
  #     actionButton(inputId = "go",
  #                  label = "Update")
  # ),
  # 
  #   mainPanel(
  #     leafletOutput("mymap",
  #                 height="80vh",
  #                 width = "60vw"),
  #     p()
  # )
  # )

)

server <- function(input, output) {
  
  # reactive expression for the subset data (by date)
  
  hotspots <- reactive({
    stamp <- as.Date(paste("01", unlist(strsplit(input$date, ";")), sep="-"), "%d-%B-%Y")
    wifis[wifis$Activated <= stamp, ]
  })
  
  arrests <- reactive({
    stamp <- as.Date(paste("01", unlist(strsplit(input$date, ";")), sep="-"), "%d-%B-%Y")
    precincts[precincts$date <= stamp, ]
  })
  
  borough <- reactive({
    borough_coords[borough_coords$borough == as.character(input$borough), ]
  })
  
  output$areaPlot <- renderPlot({
    wifi.v.arrests |>
      pivot_longer(!date, names_to = "cat", values_to = "total") |>
      ggplot(aes(x = date)) +
      geom_area(aes(y=total, fill=cat), alpha=0.8) +
      scale_fill_brewer(palette="Set3", labels=c("total arrests","wifi locations")) +
      theme_classic() +
      labs(title = "Arrests + Wifi: 2016-2019", fill=NULL) +
      xlab("") + ylab("") 
  })
  
  output$mymap <- renderLeaflet({
    
    leaflet(options = leafletOptions(minZoom = -30, maxZoom = 30)) |>
      addTiles() |>
      setView(lng = borough()$longitude, lat = borough()$latitude, zoom=borough()$zoom) |>
      addPolygons(data = arrests(),
                  color = "white", fillColor = ~pal(arrests),
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
                  label = paste("Arrests:",arrests()$arrests)) |>
      addMarkers(data = hotspots(), lat = ~ Latitude, lng = ~ Longitude, icon = wifi.Icon,
                 label = ~as.character(Neighborhood))
    # leaflet(options = leafletOptions(minZoom = -300, maxZoom = 18)) |>
    #   addProviderTiles(providers$CartoDB.Voyager) |>
    #   addMarkers(data = hotspots(), lat = ~ Latitude, lng = ~ Longitude, icon = wifiIcon, 
    #              label = ~as.character(Neighborhood)) |>
    #   setView(lng=nyc_coords[2], lat=nyc_coords[1], zoom=10)
    
  })

}

shinyApp(ui = ui, server = server)