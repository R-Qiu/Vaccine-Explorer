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
library(sjPlot)
library(ggiraph)
library(plotly)
library(stringr)
library(tidyverse)


vax_factor <- readRDS("vax_factor.rds")

vax_choices <- tibble("DTaP" = "dtap", 
                      "Hepatitis A" = "hepa", 
                      "Hepatitis B" = "hepb", 
                      "Polio" = "polio",
                      "Rotavirus" = "rota")

vax_lookup <- 
  vax_choices %>% gather(name, symbol)

color_choices <-
  tibble("Medicaid Expansion Status" = "medicaid",
         "Year" = "as.factor(year)",
         "None of the above" = "none")

ui <-
  navbarPage("Vaccine Explorer", theme = shinytheme("simplex"), 
             
    tabPanel("About", wellPanel(htmlOutput("about"))),

    tabPanel("Explore the Data",
             
             titlePanel("Explore How Various Factors Influence Immunization Rate"),
             sidebarLayout(
              sidebarPanel(
                pickerInput("factor_x", strong("Examine the influence of an outside variable"),
                            choices = c("Insurance" = "per_ins")),
                
                pickerInput("vax_choice", strong("Pick a vaccine schedule to analyze"),
                            choices = vax_choices),
                
                conditionalPanel(
                  condition = "input.tabs == 'Plot Factors'",
                  pickerInput(
                    "color_choice", strong("Distinguish points by an additional factor"),
                    choices = color_choices)),
                
                br(),
                checkboxInput("factor_incomplete", 'Count started but unfinished vaccine schedules as "vaccinated"?', FALSE),
                
                conditionalPanel(
                  condition = "input.tabs == 'Plot Factors'",
                  checkboxInput("factor_years", "Average across years?", FALSE)
                ),
                
                
                br(), br(), 
                htmlOutput("vax_factor_info"),
                
                br(), br(),
                conditionalPanel(
                  condition = "input.tabs == 'Plot Factors'",
                  htmlOutput("vax_factor_stats")) 
              ),
              
              mainPanel(
                tabsetPanel(id = "tabs",
                tabPanel("Plot Factors",
                      
                    ggiraphOutput("vax_factor_plot", height = 800)
                        ), 

                tabPanel("Animate Factors Through Time",
                         plotlyOutput("animate", height = 600, width = 800),
                         br(),
                         htmlOutput("anim_caption"))
                )
                  
                 
              )
             )),
    
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
     about4 <- p("Vaccination rates data from the Centers for Disease Control (CDC) National Immunization Survey Public Use Files, 2013-2016")
     about5 <- p("Insurance data from the US Census Bureau, Health Insurance in the United States: 2016 - Tables")
     
     about6 <- h3(strong("Source Code"))
     about7 <- p("View the source code ", tags$a(href="https://github.com/R-Qiu/Vaccine-Explorer", "here"), ".")
     
     HTML(paste(about1, about2, about3, about4, about5, about6, about7))
       
   })
   
   
   
   
   
   ####################################
   # Factors Tab
   ####################################
   
   
   incomplete <- reactive({input$factor_incomplete})
   years <- reactive({input$factor_years})
   
   
   vax_factor_data <- reactive({
     
     if(incomplete()){
       vax_factor <- 
         vax_factor %>% 
         mutate(status = recode(status, incomplete = "adequate")) %>% 
         group_by(year, state, state_name, medicaid, medicaid_num, vaccine, status, per_ins) %>% 
         summarise(per_vax = sum(per_vax)) %>% 
         ungroup()
     }
     
     
     vax_factor <- 
       vax_factor %>% 
       filter(status == "adequate",
              vaccine == input$vax_choice,
              !is.na(eval(parse(text = input$factor_x))))
     
     
     if(years()){
       vax_factor <- 
         vax_factor %>% 
         group_by(state, state_name, vaccine, status) %>% 
         summarise(per_vax = mean(per_vax),
                   per_ins = mean(per_ins)) %>% 
         ungroup()
     }
     
     vax_factor
     
   })
   
   output$vax_factor_plot <- renderggiraph({
     
     data <- vax_factor_data()
     
     caption_hover <- "Hover over a point for more info. \n"
     caption_states <- "Each point represents one state in one year.\n\n"
     caption_source <- "Data source: CDC National Immunization Survey"
     
     if(years()){
       caption_states <- "Each point represents one state."
     }
     
     caption_pts <- paste(caption_hover, caption_states, caption_source, sep = "")
     
     
     g <- 
       ggplot(data, aes_string(x = input$factor_x, y = "per_vax")) + 
         geom_smooth(method = "lm", se = FALSE) + 
         theme_bw(base_size = 12) + 
         xlim(NA, 100) + 
         ylim(NA, 100) +
         labs(y = paste(filter(vax_lookup, symbol==input$vax_choice)["name"], "Vaccination Rate (%)"),
              caption = caption_pts)
     
     if (years()){
       g <- 
         g +
         geom_point_interactive(aes(tooltip = str_to_title(state_name)), 
                                size = 1) +
         geom_rug(alpha = 0.5)
       
     } else if (input$color_choice == "none"){
       
       
       g <- 
         g + 
         geom_point_interactive(aes(tooltip = paste(str_to_title(state_name),", ", year, sep = ""),
                                    data_id = state), size = 1) + 
         geom_rug(alpha = 0.25)
       
       
     } else {
       
       g <- 
         g + 
         geom_point_interactive(aes(col = eval(parse(text = input$color_choice)), 
                                    tooltip = paste(str_to_title(state_name),", ", year, sep = ""),
                                    data_id = state), size = 1) +
         labs(color = "Year")
       
       if (input$color_choice == "year"){
         
         g <- 
           g +
           geom_rug()
         
       } else if (input$color_choice == "medicaid"){
         
         g <-
           g +
           geom_rug(aes(col = eval(parse(text = input$color_choice))), alpha = 0.4) +
           scale_color_manual(name = "Medicaid\nExpansion?",
                              values = c("firebrick", "steelblue"))
         
       }
     }
       
       
     
     girafe(code = print(g))
     
   })
   
   
   
   output$animate <- renderPlotly({
     
     data <- vax_factor_data()
     
     anim <-
       ggplot(data, aes_string(x = input$factor_x, y = "per_vax", frame = "year", color = "medicaid_num")) +
         geom_point(size = 2.5) +
         theme_bw() +
         xlim(NA, 100) +
         ylim(NA, 100) +
         scale_color_continuous(guide = FALSE, low = "firebrick", high = "steelblue") +
         labs(y = paste(filter(vax_lookup, symbol==input$vax_choice)["name"], "Vaccination Rate (%)"))

     
     anim <-
       anim %>%
       animation_opts(5000, transition = 1000, easing = "elastic", redraw = FALSE)
     
     
     ggplotly(anim)

     
   })
   
   
   output$anim_caption <- renderUI({
     
     anim_caption_point <- "Each point represents a state in a given year."
     anim_caption_red <- paste( tags$span(style="color:red", "Red"), "indicates a state that has not expanded Medicaid under the Affordable Care Act.")
     anim_caption_blue <- paste( tags$span(style="color:blue", "Blue"), "indicates a state that has expanded Medicaid under the Affordable Care Act.")
     
     HTML(paste(anim_caption_point, anim_caption_red, anim_caption_blue, sep = "<br>"))
     
   })
   
   
   output$vax_factor_stats <- renderUI({
     
     data <- vax_factor_data()
     
     model_input <- formula(paste(" per_vax ~ ", input$factor_x))
     
     vax_factor_model <- lm(model_input, data)
     vax_factor_table <- tab_model(vax_factor_model, show.intercept = FALSE, 
                                   pred.labels = "Percent Insured", dv.labels = "Percent Vaccinated")
     HTML(vax_factor_table$knitr)
     
     
   })
   
   
   
   output$vax_factor_info <- renderUI({
     
     stats <- p("Blarg!")
     
     HTML(paste(stats))
     
   })
   
   
   
   ####################################
   # TODO Tab
   ####################################
   
   output$todo <- renderUI({
     
     todoh1 <- h3(strong("Background and Definitions Tab"))
     todoh11 <- p("Define schedule and each vaccine")
     
     todoh2 <- h3(strong("Graph"))
     todoh21 <- p("Vax rate vs income")
     todoh22 <- p("Factors influencing incomplete vaccines? Option or tab")
     todoh23 <- p("Explanatory blurbs")
     todoh24 <- p( tags$s("Legend on animation"))
     todoh25 <- p( tags$s("Merge 'Plot' and 'Plot Over Time' to singe tab w/ color options"))
     todoh26 <- p("Clean up axis labels")
     todoh27 <- p("Before/after Medicaid expansion tab violin plots w/ years before/after?")
     todoh28 <- p( tags$s("Insurance data/income data going to 2008?"))
     todoh29 <- p("More vaccines: Varicella (Chickenpox), MMR, PCV13")
     todoh210 <- p( tags$s('Conditional stats, "average across years", "addtl factor" w/ animate'))
     
     
     HTML(paste(todoh1, todoh11, 
                todoh2, todoh21, todoh22, todoh23, todoh24, todoh25, todoh26, todoh27, todoh28, todoh29, todoh210))
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

