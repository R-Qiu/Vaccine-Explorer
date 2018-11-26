#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(sjPlot)


# Define UI for application that draws a histogram
ui <-
  navbarPage("Immunizations", 
    tabPanel("Introduction", htmlOutput("intro")),
    tabPanel("Exploration", 
      tabsetPanel("Demographics", leafletOutput("survey_per_capita_map")))
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$intro <- renderUI({
    
     intro1 <- h3("Welcome!")
     intro2 <- p("This application was created by Richard Qiu for Gov 1005 at Harvard.")
     
     HTML(paste(intro1, intro2))
       
   })
   
   
   output$survey_per_capita_map <- renderLeaflet({
     
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

