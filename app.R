# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

rm(list = ls())
setwd("C:/Users/axell/OneDrive/Documents/BOULOT/R/Developing Data Products")

library(plotly)
library(jsonlite)
library(magrittr)
library(ggplot2)
library(shiny)

#Download Data from NYC Open Data: SAT Test Results By Grade 2012 - School Level - All Students
data <- fromJSON("https://data.cityofnewyork.us/resource/f9bf-2cp4.json", flatten = TRUE)

#Add a Borough and District columnn that identifies the School Borough and District
data$Borough <- substr(as.character(data$dbn),3,3)
data$Borough <- gsub("K","Brooklyn", data$Borough)
data$Borough <- gsub("X","Bronx", data$Borough)
data$Borough <- gsub("M","Manhattan", data$Borough)
data$Borough <- gsub("Q","Queens", data$Borough)
data$Borough <- gsub("R","Staten Island", data$Borough)
data$Borough <- as.factor(data$Borough)

data$District <- as.factor(substr(as.character(data$dbn),1,2))

# Make score columns and test takers column numeric
data[,c(2:4,6)] <- sapply(data[,c(2:4,6)], as.numeric)


#Rename Data Set columns
colnames(data) <- c("DBN", "Writing", "Critical_Reading", "Math", "School_Name",
                    "n_Test_Takers","Borough","District")

names <- names(data)
#boroughs <- levels(data$Borough)

#Remove NAs
data <- data[complete.cases(data[,1:8]),]

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  headerPanel("Plot Options"),
  sidebarPanel(
    sliderInput('sampleSize', 'Number of Test Takers at School', 
                min = 0, max = 1300, value = 1000, step = 100, round = 0)
  #   selectInput('x', 'X', choices = names[2:4], selected = "Critical_Reading"),
  #   selectInput('y', 'Y', choices = names[2:4], selected = "Math")
  #  , selectInput('color', 'Color', choices = names[c(1,5,7:8)], selected = "Borough")
  ),

  mainPanel(
    plotlyOutput('SATPlot', height='700px')
  )
)


# Define server logic required to draw the scatterplot
server <- function(input, output) {
  
  dataset <- reactive({
    data[ which(data$n_Test_Takers <= input$sampleSize), ]  })
  
  
  output$SATPlot <- renderPlotly({
      p <- ( plot_ly(dataset(), 
            x = ~Critical_Reading, 
            y = ~Math, 
            type = 'scatter', mode = 'markers', 
            text= ~School_Name,
            size = ~n_Test_Takers, sizes = c(10, 35),
            color= ~Borough, 
            colors = 'PRGn',
            marker = list(opacity = 1, sizemode = 'diameter', line = list(width = 0.5, color = 'black'))))
    
      layout(p, title = 'NYC School Level 2012 SAT Test Results')
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


