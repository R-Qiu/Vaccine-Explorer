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

ui <-
  navbarPage(strong("Vaccine Explorer"), theme = shinytheme("simplex"), 
             
    tabPanel("About", wellPanel(htmlOutput("about"))),
    
    tabPanel("Explore the Survey"),
    
    
    
    tabPanel("Outside Factors",
             
             titlePanel("Explore How Various Factors Influence Immunization Rate"),
             sidebarLayout(
              sidebarPanel(
                pickerInput("factor_x", strong("Examine the influence of an outside variable"),
                            choices = c("Insurance" = "per_ins")),
                
                pickerInput("vax_choice", strong("Pick a vaccine schedule to analyze"),
                            choices = vax_choices),
                checkboxInput("factor_incomplete", 'Count started but unfinished vaccine schedules as "vaccinated"?', FALSE),
                checkboxInput("factor_years", "Average across years?", FALSE),
                br(), br(), 
                htmlOutput("vax_factor_info"),
                br(), br(),
                htmlOutput("vax_factor_stats")
              ),
              
              mainPanel(
                tabsetPanel(id = "tabs",
                tabPanel("Plot",
                      
                    ggiraphOutput("vax_factor_plot", height = 800)
                        ), 
                
                tabPanel("Plot Over Time", 
                         ggiraphOutput("vax_factor_time_plot", height = 800)),
                
                tabPanel("animate",
                         plotlyOutput("animate", height = 700),
                         br(),
                         htmlOutput("anim_caption"))
                )
                  
                  # Currenty, gganimate is not supported by ShinyApps.io
                  # ,tabPanel(
                  #   "Animate",
                  #   br(),
                  #   p("It may take up to a minute for the animated plot to load."),
                  #   br(),
                  #   withSpinner(imageOutput("vax_factor_anim"), color = "#d9240c"))
                  )
             # )
             )),
    
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
     
     g <- 
       ggplot(data, aes_string(x = input$factor_x, y = "per_vax")) + 
         geom_smooth(method = "lm", se = FALSE) + 
         theme_bw(base_size = 12) + 
         xlim(75, 100) + 
         ylim(60, 100) +
         labs(y = paste(filter(vax_lookup, symbol==input$vax_choice)["name"], "Vaccination Rate (%)"),
              caption = "Each point represents one state\n\nData source: CDC National Immunization Survey")
     
     if(years()){
       g <- 
         g +
         geom_point_interactive(size = 1.25)
     }else{
       g <- 
         g + 
         geom_point_interactive(aes(col = as.factor(year), 
                                    tooltip = paste(str_to_title(state_name), year),
                                    data_id = state), size = 1.25) + 
         scale_color_brewer(palette = "BrBG") + 
         labs(color = "Year")
     }
     
     
     girafe(code = print(g))
     
   })

   
   output$vax_factor_stats <- renderUI({
     
     data <- vax_factor_data()
     
     model_input <- formula(paste(" per_vax ~ ", input$factor_x))
     
     vax_factor_model <- lm(model_input, data)
     vax_factor_table <- tab_model(vax_factor_model, show.intercept = FALSE, 
                                   pred.labels = "Percent Insured", dv.labels = "Percent Vaccinated")
     HTML(vax_factor_table$knitr)
     
     
   })
   
   
   output$vax_factor_time_plot <- renderggiraph({
     
     data <- vax_factor_data()
     
     g <- 
       ggplot(data, aes_string(x = input$factor_x, y = "per_vax")) + 
         geom_smooth(method = "lm", se = FALSE) + 
         theme_bw(base_size = 12) + 
         xlim(75, 100) + 
         ylim(60, 100) +
         labs(y = paste(filter(vax_lookup, symbol==input$vax_choice)["name"], "Vaccination Rate (%)"),
              caption = "Each point represents one state\n\nData source: CDC National Immunization Survey")
     
     g <- 
       g + 
       geom_point_interactive(aes(col = as.factor(medicaid),
                                  tooltip = paste(str_to_title(state_name), year),
                              data_id = state), size = 1.25) +
       scale_color_manual(name = "Medicare\nExpansion?",
                          values = c("firebrick", "steelblue"))
     
     girafe(code = print(g))
     
   })
   
   
   
   output$animate <- renderPlotly({
     
     data <- vax_factor_data()
     
     anim <-
       ggplot(data, aes_string(x = input$factor_x, y = "per_vax", frame = "year", color = "medicaid_num")) +
         geom_point(size = 2.5) +
         theme_bw() +
         xlim(75, 100) +
         ylim(60, 100) +
         scale_color_continuous(name = "Medicare\nExpansion?",low = "firebrick", high = "steelblue")
         # labs(y = paste(filter(vax_lookup, symbol==input$vax_choice)["name"], "Vaccination Rate (%)"),
         #      caption = "Each point represents one state\n\nData source: CDC National Immunization Survey")

     
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
   
   
   output$vax_factor_info <- renderUI({
     
     stats <- p("Blarg!")
     
     HTML(paste(stats))
     
   })
   
   
   
   # Currently, gganimate is not supported by ShinyApps.io
   #
   # output$vax_factor_anim <- renderImage({
   # 
   # data <- vax_factor_data()
   #
   #   g <- 
   #     ggplot(data, aes_string(x = input$factor_x, y = "per_vax", color = "color")) + 
   #       geom_point(size = 6) +
   #       # geom_smooth(method = "lm", se = FALSE) + 
   #       theme_bw(base_size = 24) + 
   #       xlim(75, 100) + 
   #       ylim(60, 100) +
   #       labs(y = "Vaccination Rate", title = "Year: {next_state}") + 
   #       scale_color_continuous(name = "Medicare\nExpansion?",low = "firebrick", high = "steelblue") +
   #     
   #     
   #       # gganimate
   #     
   #       transition_states(year, transition_length = 0.5, state_length = 2, wrap = FALSE) + 
   #       ease_aes('quadratic-in-out')
   #   
   #   options(gganimate.dev_args = list(width = 1000, height = 800))
   #   
   #   tmpfile <-
   #     animate(g, renderer = magick_renderer()) %>% 
   #     image_write_gif(tempfile(fileext = 'gif'), delay = 0.08)
   #   
   #   list(src = tmpfile,
   #        contentType = 'image/gif',
   #        alt = "gif of vaccination rates vs insurance rates over time"
   #   )}, deleteFile = TRUE)
   #   
   # 
   
   
   
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

