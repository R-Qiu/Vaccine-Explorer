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
  navbarPage(strong("Title goes here"), theme = shinytheme("simplex"), 
             
    tabPanel("About", wellPanel(htmlOutput("about"))),
    
    tabPanel("Explore the Survey"),
    
    tabPanel("Influence of Outside Factors"),
    
    tabPanel("TODO", wellPanel(htmlOutput("todo")))
  
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$about <- renderUI({
    
     intro1 <- h3(strong("Summary"))
     intro2 <- p("This application allows you to explore the factors influencing vaccination rates in the US from 2013 to 2016")
     
     intro3 <- h3(strong("Data Sources"))
     intro4 <- p("Centers for Disease Control (CDC) National Immunization Survey Public Use Files, 2013-2016")
     
     intro5 <- h3(strong("Source Code"))
     intro6 <- p("URL Placeholder")
     
     HTML(paste(intro1, intro2, intro3, intro4, intro5, intro6))
       
   })
   
   output$todo <- renderUI({
     
     todo1 <- h3(strong("Explore data"))
     todo2 <- p("CDC survey distribution by wealth, state pop, age?")
     todo3 <- p("CDC insurance vs Census insurance")
     
     HTML(paste(todo1, todo2, todo3))
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

