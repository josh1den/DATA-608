library(shiny)
library(rsconnect)
library(tidyverse)

# read data
data = 'https://raw.githubusercontent.com/josh1den/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv'
df = read.csv(data)

# basic transformation 
# change column names
colnames(df)[1] <- "Cause_Of_Death"
colnames(df)[6] <- "Crude_Mortality_Rate"

# convert cause of death column to title case 
df$Cause_Of_Death = str_to_title(df$Cause_Of_Death)

# question two takes all columns and
q2 = df 

# calculate national average
q2.1 = q2 |> group_by(Cause_Of_Death, Year) |>
  summarise(Nat_Avg = sum(Deaths)/sum(Population)*100000)

# Define UI for app 
ui <- fluidPage(
  
  # App title ---- 
  titlePanel("Mortality Rate by State vs. National Avg"),
  
  # Sidebar layout with input and output definitions ---- 
  sidebarLayout(
    # Sidebar panel for inputs ---- 
    sidebarPanel(
      selectInput('cause', 
                  label='Cause of Death', 
                  choices=unique(q2$Cause_Of_Death),
                  selected='Certain Infectious And Parasitic Diseases'),
      selectInput('state',
                  label='State',
                  choices=unique(q2$State),
                  selected='AL'),
      width = 3
    ),
    
    mainPanel(
      plotOutput('plot'),
      height='auto'
    )
  )
)

server <- function(input, output) {
  
  selectedData1 <- reactive({
    q2 |> filter(Cause_Of_Death == input$cause & State == input$state )
  })
  
  selectedData2 <- reactive({
    q2.1 |> filter(Cause_Of_Death == input$cause)
  })
  
  output$plot <- renderPlot({
    
    ggplot(selectedData1(), aes(x=Year)) +
      geom_line(aes(y=Crude_Mortality_Rate, color=input$state)) +
      geom_line(data=selectedData2(), aes(y=Nat_Avg, color='National Avg')) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 11)) + 
      labs(x='Year', y='Crude Mortality Rate', color="color") +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    
  }
  )
}

# Run app ----
shinyApp(ui, server)