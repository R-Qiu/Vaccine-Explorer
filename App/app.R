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
library(viridis)
library(sjPlot)
library(ggiraph)
library(plotly)
library(stringr)
library(tidyverse)


vax_factor <- readRDS("vax_factor.rds")

vax_choices <- tibble("DTaP (Diptheria, Tetanus, Whooping Cough)" = "dtap", 
                      "Hepatitis A" = "hepa", 
                      "Hepatitis B" = "hepb", 
                      "IPV (Polio)" = "polio",
                      "Rotavirus" = "rota",
                      "MMR (Measles, Mumps, Rubella)" = "mmr",
                      "Varicella (Chickenpox)" = "vrc",
                      "PCV (Some Pneumonia, Meningitis, Sepsis)" = "pcv")

# Makes a table to lookup full names from symbols for axes labels
# Regex grabs everything between parenthesis and the preceding space

vax_lookup <- 
  vax_choices %>% 
  gather(name, symbol) %>% 
  mutate(name = str_remove(name, "\\s\\(.*?\\)$"))


factor_choices <- tibble("Insurance Rate" = "per_ins",
                         "Poverty Rate" = "per_pov")

factor_lookup <- 
  factor_choices %>% 
  gather(name, symbol)

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
                            choices = factor_choices),
                
                pickerInput("vax_choice", strong("Pick a vaccine schedule to analyze"),
                            choices = vax_choices),
                
                # Disables color choice on Animation tab
                
                conditionalPanel(
                  condition = "input.tabs == 'Plot Factors'",
                  pickerInput(
                    "color_choice", strong("Distinguish points by an additional factor"),
                    choices = color_choices)),
                
                br(),
                checkboxInput("factor_incomplete", 'Count vaccine schedules that are behind as completed?', FALSE),
                
                # Disables option to average on Animation tab
                
                conditionalPanel(
                  condition = "input.tabs == 'Plot Factors'",
                  checkboxInput("factor_years", "Average across years?", FALSE)
                ),
                
                # Disable linear regression summary stats on Animation tab
                
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

                tabPanel("Animate Factors",
                         plotlyOutput("animate", height = 600, width = 800),
                         br(),
                         htmlOutput("anim_caption"))
                )
                  
                 
              )
             )
    
    # tabPanel("TODO", wellPanel(htmlOutput("todo"))
             
      )
  
  )

  
server <- function(input, output) {
  
  ####################################
  # About Tab
  ####################################
  
   output$about <- renderUI({
    
     about1 <- h3(strong("Summary"))
     about2 <- p("This application enables exploration of how poverty rate and insurance rate influence state-level vaccination rates in the US from 2010 to 2016.")
     about3 <- p("Additionally, this application examines the influence of Medicaid expansion under the Affordable Care Act and how it has influenced vaccination rates over time.")
     
     about4 <- h3(strong("Data Sources"))
     about5 <- p("Vaccination rates data from the Centers for Disease Control (CDC) National Immunization Survey Public Use Files, 2010-2016")
     about6 <- p("Medicaid Expansion Insurance data (2013-2016) from the US Census Bureau, Health Insurance in the United States: 2016 - Tables")
     about7 <- p("Historic Insurance data (2010-2012) from the US Census Bureau, Health Insurance Historical Tables - HIC Series.")
     about8 <- p("Poverty data from the US Census Bureau, Historical Poverty Tables: People and Families - 1959 to 2017")
     
     about9 <- h3(strong("Source Code"))
     about10 <- p("View the source code ", tags$a(href="https://github.com/R-Qiu/Vaccine-Explorer", "here"), ".")
     
     HTML(paste(about1, about2, about3, about4, about5, about6, about7, about8, about9, about10))
       
   })
   
   
   
   
   
   ####################################
   # Factors Tab
   ####################################
   
   
   incomplete <- reactive({input$factor_incomplete})
   years <- reactive({input$factor_years})
   
   
   # Make reactive data manipulation reactive to avoid performing manipulation twice
   # This way, altered data can be moved imported into each render
   
   vax_factor_data <- reactive({
    
     # Must do incomplete before summarizing percent of "adequate" immunizations per state
     # This is because "incomplete" must get recoded first so that those also get counted as "adequate"
     
     if(incomplete()){
       vax_factor <- 
         vax_factor %>% 
         mutate(status = recode(status, incomplete = "adequate")) %>% 
         group_by(year, state, state_name, medicaid, medicaid_num, vaccine, status, per_ins, per_pov) %>% 
         summarise(per_vax = sum(per_vax)) %>% 
         ungroup()
     }
     
     
     # Calculate percent of "adequate" vaccinations per state
     
     vax_factor <- 
       vax_factor %>% 
       filter(status == "adequate",
              vaccine == input$vax_choice,
              !is.na(eval(parse(text = input$factor_x))))
     
     
     # If combine years is selected, summarizes all years together
     
     if(years()){
       vax_factor <- 
         vax_factor %>% 
         group_by(state, state_name, vaccine, status) %>% 
         summarise(per_vax = mean(per_vax),
                   per_ins = mean(per_ins),
                   per_pov = mean(per_pov)) %>% 
         ungroup()
     }
     
     
     # Returns transformed data
     
     vax_factor
     
   })
   
   
   output$vax_factor_plot <- renderggiraph({
     
     
     # Import data from reactive element
     
     data <- vax_factor_data()
     
     
     # Modifies caption based on which x axis is selected
     # Notably, order of citations changes in order to keep them in order from longest to shortest
     
     caption_hover <- "Hover over a point for more info. \n"
     caption_states <- "Each point represents one state in one year.\n\n"
     caption_source <- "Data sources: "
     caption_source1 <- "CDC National Immunization Surveys, 2010-2016\n"
     
     if(years()){
       caption_states <- "Each point represents one state.\n\n"
     }
     
     if (input$factor_x == "per_ins") {
       
       caption_source2 <- "US Census Bureau Health Insurance Historical Tables\nUS Census Bureau Health Insurance In the US 2016\n"
       caption_pts <- paste(caption_hover, caption_states, caption_source, caption_source2, caption_source1, sep = "")
       
     } else if (input$factor_x == "per_pov"){
       
       caption_source2 <- "US Census Bureau Historical Poverty Tables"
       caption_pts <- paste(caption_hover, caption_states, caption_source, caption_source1, caption_source2, sep = "")
       
     }
     
     
     # Labels dynamically generated from lookup tables defined at beginning based on what x/y axes are selected
     
     g <- 
       ggplot(data, aes_string(x = input$factor_x, y = "per_vax")) + 
         geom_smooth(method = "lm", se = FALSE) + 
         theme_bw(base_size = 12) + 
         labs(x = paste(filter(factor_lookup, symbol==input$factor_x)["name"], "(%)"),
              y = paste(filter(vax_lookup, symbol==input$vax_choice)["name"], "Vaccination Rate (%)"),
              caption = caption_pts)
     
     
     # Modifies x and y axes bounds to make sure that 100 or 0 is always shown
     # This done to maintain perspective relative to the metric (insurance/poverty rate) in question
     
     if (input$factor_x == "per_pov") {

       g <-
         g +
         xlim(0, NA) +
         ylim(NA, 100)

     } else {

       g <-
         g +
         xlim(NA, 100) +
         ylim(NA, 100)
     }

     
     # Differentially adds color/interactivity/etc. based on what options are selected
     
     if (years()) {
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
                                    data_id = state), size = 1)
       
       if (input$color_choice == "as.factor(year)"){
         
         g <- 
           g +
           geom_rug(alpha = 0.35) + 
           scale_color_viridis("Years", end = 0.98, discrete = TRUE)
         
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
     
     
     # Import data from reactive element
     
     data <- vax_factor_data()
     
     
     # Generates animated Plotly, animating over years
     # TODO: disable toolbar (zoom, etc.)
     # TODO: disable or reformat hover tooltips
     # Rug plot excluded since Plotly seems to have a bug with changing discrete colors
     # For same reason, color is defined as (the continuous variable) medicaid_num and the scale color is continuous
     
     anim <-
       ggplot(data, aes_string(x = input$factor_x, y = "per_vax", frame = "year", color = "medicaid_num")) +
         geom_point(size = 2.5) +
         theme_bw() +
         scale_color_continuous(guide = FALSE, low = "firebrick", high = "steelblue") +
         labs(x = paste(filter(factor_lookup, symbol==input$factor_x)["name"], "(%)"),
              y = paste(filter(vax_lookup, symbol==input$vax_choice)["name"], "Vaccination Rate (%)"))
     
     
     # Like above, changes axes based on selected x-axis to always keep 0 or 100 shown
     # Again, done for perspective
     
     if (input$factor_x == "per_pov") {
       
       anim <-
         anim +
         xlim(0, NA) +
         ylim(NA, 100)
       
     } else {
       
       anim <-
         anim +
         xlim(NA, 100) +
         ylim(NA, 100)
     }

     
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
                                   pred.labels = paste(filter(factor_lookup, symbol==input$factor_x)["name"]), 
                                   dv.labels = "Percent Vaccinated")
     HTML(vax_factor_table$knitr)
     
     
   })
   
   
   
   output$vax_factor_info <- renderUI({
     
     info <- p("Blarg!")
     
     HTML(paste(info))
     
   })
   
   
   
   ####################################
   # TODO Tab
   ####################################
   
   output$todo <- renderUI({
     
     todoh1 <- h3(strong("Background and Definitions Tab"))
     todoh11 <- p("Define schedule and each vaccine")
     
     todoh2 <- h3(strong("Graph"))
     todoh21 <- p( tags$s("Vax rate vs income"))
     todoh22 <- p("Clean up anim hovers, more detailed plot hovers (vax complete/started rate, ins/pov rate, medicaid expanded) ")
     todoh23 <- p("Explanatory blurbs")
     todoh24 <- p( tags$s("Legend on animation"))
     todoh25 <- p( tags$s("Merge 'Plot' and 'Plot Over Time' to singe tab w/ color options"))
     todoh26 <- p( tags$s("Clean up axis labels"))
     todoh27 <- p( tags$s("Insurance data/income data going to 2008?"))
     todoh28 <- p( tags$s("More vaccines: Varicella (Chickenpox), MMR, PCV13"))
     todoh29 <- p( tags$s('Conditional stats, "average across years", "addtl factor" w/ animate'))
     
     todoh3 <- h3(strong("For Final Version"))
     todoh31 <- p("Factors influencing incomplete vaccines? Option or tab")
     todoh32 <- p("Before/after Medicaid expansion tab violin plots w/ years before/after?")
     
     
     HTML(paste(todoh1, todoh11, 
                todoh2, todoh21, todoh22, todoh23, todoh24, todoh25, todoh26, todoh27, todoh28, todoh29, 
                todoh3, todoh31, todoh32))
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

