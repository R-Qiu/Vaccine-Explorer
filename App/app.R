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
library(leaflet)
library(sjPlot)
library(plotly)
library(tidyverse)



vax_factor <- readRDS("vax_factor.rds")

vax_choices <- c("DTaP" = "dtap", 
                 "Hepatitis A" = "hepa", 
                 "Hepatitis B" = "hepb", 
                 "Polio" = "polio",
                 "Rotavirus" = "rota")

ui <-
  navbarPage(strong("Title goes here"), theme = shinytheme("simplex"), 
             
    tabPanel("About", wellPanel(htmlOutput("about"))),
    
    tabPanel("Explore the Survey"),
    
    tabPanel("Graph Outside Factors",
             titlePanel("Explore How Various Factors Influence Immunization Rate"),
             sidebarLayout(
              sidebarPanel(
                pickerInput("factor_x", "Examine the influence of an outside variable",
                            choices = c("Insurance" = "per_ins")),
                
                pickerInput("vax_choice", "Pick a vaccine schedule to analyze",
                            choices = vax_choices),
                checkboxInput("factor_combine", 'Count started but unfinished vaccine schedules as "vaccinated"?', FALSE),
                br(),
                htmlOutput("vax_factor_stats")
              ),
              
              mainPanel(plotOutput("vax_factor_plot", width=850, height=800))
             )),
    
    tabPanel("Map Outside Factors"),
    
    tabPanel("TODO", wellPanel(htmlOutput("todo")))
  
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ####################################
  # About Tab
  ####################################
  
   output$about <- renderUI({
    
     about1 <- h3(strong("Summary"))
     about2 <- p("This application allows you to explore the factors influencing vaccination rates in the US from 2013 to 2016")
     
     about3 <- h3(strong("Data Sources"))
     about4 <- p("Centers for Disease Control (CDC) National Immunization Survey Public Use Files, 2013-2016")
     
     about5 <- h3(strong("Source Code"))
     about6 <- p("URL Placeholder")
     
     HTML(paste(about1, about2, about3, about4, about5, about6))
       
   })
   
   
   
   
   
   ####################################
   # Factors Tab
   ####################################
   
   
   combine <- reactive({input$factor_combine})
   
   
   output$vax_factor_plot <- renderPlot({
     
     if(combine()){
       vax_factor <- 
         vax_factor_raw %>% 
         mutate(status = recode(status, incomplete = "adequate")) %>% 
         group_by(year, state, vaccine, status) %>% 
         summarise(per_vax = sum(per_vax), per_ins = unique(per_ins))
     }
     
     vax_factor <- 
       vax_factor %>% 
       filter(status == "adequate",
              vaccine == input$vax_choice)
     
     g <- 
       ggplot(vax_factor, aes_string(x = input$factor_x, y = "per_vax")) + 
       geom_point(aes_string(col = "year"), size = 2) +
       geom_smooth(method = "lm", se = FALSE) + 
       theme_bw(base_size = 20) + 
       xlim(75, 100) + 
       ylim(60, 100)
       labs(y = "Vaccination Rate")
       
       
     
     g
     
   })
   
   
   output$vax_factor_stats <- renderUI({
     if(combine()){
       vax_factor <- 
         vax_factor_raw %>% 
         mutate(status = recode(status, incomplete = "adequate")) %>% 
         group_by(year, state, vaccine, status) %>% 
         summarise(per_vax = sum(per_vax), per_ins = unique(per_ins))
     }
     
     vax_factor <- 
       vax_factor %>% 
       filter(status == "adequate",
              vaccine == input$vax_choice)
     
     model_input <- formula(paste(" per_vax ~ ", input$factor_x))
     
     vax_factor_model <- lm(model_input, vax_factor)
     vax_factor_table <- tab_model(vax_factor_model)
     HTML(vax_factor_table$knitr)
     
   })
   
   
   
   
   
   ####################################
   # TODO Tab
   ####################################
   
   output$todo <- renderUI({
     
     todoh1 <- h3(strong("Explore"))
     todoh11 <- p("CDC survey distribution by wealth, state pop, age?")
     todoh12 <- p("CDC insurance vs Census insurance")
     
     todoh2 <- h3(strong("Graph"))
     todoh21 <- p("Vax rate vs income")
     todoh22 <- p("Factors influencing incomplete vaccines?")
     todoh23 <- p("Years seem difficult to distinguish, perhaps some sort of time-animation")
     todoh24 <- p("Explanatory blurbs would be useful")
     
     todoh3 <- h3(strong("Others"))
     todoh31 <- p("before/after medicare expansion? may need more insurance data for this")
     todoh32 <- p("Mapping would be cool")
     
     
     HTML(paste(todoh1, todoh11, todoh12, 
                todoh2, todoh21, todoh22, todoh23, todoh24,
                todoh3, todoh31, todoh32))
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

