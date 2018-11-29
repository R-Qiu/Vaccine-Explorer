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
library(shinycssloaders)
library(leaflet)
library(sjPlot)
library(gganimate)
library(tidyverse)



vax_factor <- readRDS("vax_factor.rds")

vax_choices <- c("DTaP" = "dtap", 
                 "Hepatitis A" = "hepa", 
                 "Hepatitis B" = "hepb", 
                 "Polio" = "polio",
                 "Rotavirus" = "rota")

ui <-
  navbarPage(strong("Vaccine Explorer"), theme = shinytheme("simplex"), 
             
    tabPanel("About", wellPanel(htmlOutput("about"))),
    
    tabPanel("Explore the Survey"),
    
    
    
    tabPanel("Outside Factors",
             
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
              
              mainPanel(
                tabsetPanel(
                  tabPanel(
                    "Plot", plotOutput("vax_factor_plot", height = 800)),
                  tabPanel(
                    "Animate",
                    br(),
                    p("It may take up to a minute for the animated plot to load."),
                    br(),
                    withSpinner(imageOutput("vax_factor_anim"), color = "#d9240c"))
                  )
             ))),
    
    tabPanel("Map Outside Factors"),
    
    tabPanel("TODO", wellPanel(htmlOutput("todo")))
  
  )

  
server <- function(input, output) {
  
  ####################################
  # About Tab
  ####################################
  
   output$about <- renderUI({
    
     about1 <- h3(strong("Summary"))
     about2 <- p("This application allows you to explore the factors influencing vaccination rates in the US from 2013 to 2016")
     
     about3 <- h3(strong("Data Sources"))
     about4 <- p("Centers for Disease Control (CDC) National Immunization Survey Public Use Files, 2013-2016")
     about5 <- p("US Census Bureau: Health Insurance in the United States: 2016 - Tables")
     
     about6 <- h3(strong("Source Code"))
     about7 <- p("View the source code ", tags$a(href="https://github.com/R-Qiu/Vaccine-Explorer", "here"), ".")
     
     HTML(paste(about1, about2, about3, about4, about5, about6, about7))
       
   })
   
   
   
   
   
   ####################################
   # Factors Tab
   ####################################
   
   
   combine <- reactive({input$factor_combine})
   
   output$vax_factor_plot <- renderPlot({
     
     if(combine()){
       vax_factor <- 
         vax_factor %>% 
         mutate(status = recode(status, incomplete = "adequate")) %>% 
         group_by(year, state, vaccine, status) %>% 
         summarise(per_vax = sum(per_vax), per_ins = unique(per_ins))
     }
     
     vax_factor <- 
       vax_factor %>% 
       filter(status == "adequate",
              vaccine == input$vax_choice) %>% 
       mutate(year = as.factor(year))
     
     g <- 
       ggplot(vax_factor, aes_string(x = input$factor_x, y = "per_vax")) + 
         geom_point(aes(col = year), size = 4) +
         geom_smooth(method = "lm", se = FALSE) + 
         scale_color_brewer(palette = "Spectral") + 
         theme_bw(base_size = 24) + 
         xlim(75, 100) + 
         ylim(60, 100)
         labs(y = "Vaccination Rate")
     
     
     g
     
   })

   
   
   output$vax_factor_anim <- renderImage({
     
     if(combine()){
       vax_factor <- 
         vax_factor %>% 
         mutate(status = recode(status, incomplete = "adequate")) %>% 
         group_by(year, state, vaccine, status, per_ins, medicaid) %>% 
         summarise(per_vax = sum(per_vax))
     }
     
     vax_factor <- 
       vax_factor %>% 
       filter(status == "adequate",
              vaccine == input$vax_choice) %>% 
              mutate(color = recode(medicaid, "y" = 1, 
                                              "n" = 0))
              
     
     g <- 
       ggplot(vax_factor, aes_string(x = input$factor_x, y = "per_vax", color = "color")) + 
         geom_point(size = 6) +
         # geom_smooth(method = "lm", se = FALSE) + 
         theme_bw(base_size = 24) + 
         xlim(75, 100) + 
         ylim(60, 100) +
         labs(y = "Vaccination Rate", title = "Year: {frame_time}") + 
         scale_color_continuous(name = "Medicare\nExpansion?",low = "firebrick", high = "steelblue") +
         transition_time(year)
     
     options(gganimate.dev_args = list(width = 1000, height = 800))
     
     anim_save("outfile.gif", animate(g))
     
     list(src = "outfile.gif",
          contentType = 'image/gif',
          width = 800,
          height = 600,
          alt = "gif of vaccination rates vs insurance rates over time"
     )}, deleteFile = TRUE)
     
   
   
   output$vax_factor_stats <- renderUI({
     if(combine()){
       vax_factor <- 
         vax_factor %>% 
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
     vax_factor_table <- tab_model(vax_factor_model, show.intercept = FALSE, 
                                   pred.labels = "Percent Insured", dv.labels = "Percent Vaccinated")
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
     todoh23 <- p(tags$s("Years seem difficult to distinguish, perhaps some sort of time-animation"))
     todoh24 <- p("Explanatory blurbs would be useful")
     todoh25 <- p("Resolution/legend on animation")
     
     todoh3 <- h3(strong("Others"))
     todoh31 <- p("before/after medicare expansion? may need more insurance data for this")
     todoh32 <- p("Mapping would be cool")
     
     
     HTML(paste(todoh1, todoh11, todoh12, 
                todoh2, todoh21, todoh22, todoh23, todoh24, todoh25,
                todoh3, todoh31, todoh32))
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

