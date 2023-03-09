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

# subset data for questions: cause, states, rate, year = 2010
q1 = df |> 
  filter(Year == '2010') |>
  select(c(1, 2, 6))

# Define UI for app 
ui <- fluidPage(
  
  # App title ---- 
  titlePanel("Mortality Rate by State"),
  
  # Sidebar layout with input and output definitions ---- 
  sidebarLayout(
    
      # Sidebar panel for inputs ---- 
      sidebarPanel(
        selectInput('cause', label='Cause of Death', 
                    choices=unique(q1$Cause_Of_Death),
                    selected='Certain Infectious And Parasitic Diseases'),
        width=3
      ),
      
      mainPanel(
        plotOutput('plot'),
        height='auto'
      )
  )
)

server <- function(input, output) {
  
  selectedData <- reactive({
    q1 |> filter(Cause_Of_Death == input$cause )
  })

  output$plot <- renderPlot({
    
    ggplot(selectedData(), aes(x=reorder(State,Crude_Mortality_Rate), y=Crude_Mortality_Rate)) +
      geom_col(fill='#f68060', alpha=.6, width=.4) +
      coord_flip() +
      labs(x='State', y='Crude Mortality Rate', title=input$cause)
    
  }
  )
}

# Run app ----
shinyApp(ui, server)