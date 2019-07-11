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


# Chose to ahead and define all the inputs for dropdowns ahead of time
# Chose to create "lookup tables" to allow for substitution of input back to formatted text
# This is to simplify axes labelling without needing a bunch of if-else statements

vax_factor <- readRDS("vax_factor.rds")

vax_choices <- tibble("DTaP (Diptheria, Tetanus, Whooping Cough)" = "dtap", 
                      "Hepatitis A" = "hepa", 
                      "Hepatitis B" = "hepb", 
                      "IPV (Polio)" = "polio",
                      "Rotavirus" = "rota",
                      "MMR (Measles, Mumps, Rubella)" = "mmr",
                      "Varicella (Chickenpox)" = "vrc",
                      "PCV (Pneumonia, Meningitis, Sepsis)" = "pcv")

# Make lookup table of all state names

states_list <-
  vax_factor %>% 
  select(state_name) %>% 
  mutate(state_name_pretty = state_name) %>% 
  distinct() %>% 
  mutate(state_name_pretty = str_to_title(state_name_pretty)) %>% 
  spread(state_name_pretty, state_name)


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


# Year choice defined as as.factor() because of later use of an aes_string() for ggplot

color_choices <-
  tibble("Medicaid Expansion Status" = "medicaid",
         "Year" = "as.factor(year)",
         "None of the above" = "none")



ui <-
  navbarPage("Vaccine Explorer", theme = shinytheme("simplex"), 
             
             
    tabPanel("About", htmlOutput("about")),
    
    # Chose to use sidebarPanel with tabs that only for mainPanel
    # This is because for the most part, choices are the same between the two
    # Differences in choices (e.g. Addt'l Factors not a choice for animation) accounted for using conditionalPanels
    
    tabPanel("Explore the Data",
             
             titlePanel("Explore How Various Factors Influence Immunization Rate"),
             sidebarLayout(
              sidebarPanel(
                pickerInput("factor_x", strong("Examine the influence of an outside variable"),
                            choices = factor_choices),
                
                htmlOutput("factor_x_info"), br(), 
                
                pickerInput("states", strong("States Displayed"), 
                            choices = states_list, selected = states_list,
                            options = list(`actions-box` = TRUE), multiple = TRUE), br(), 
                
                pickerInput("vax_choice", strong("Pick a vaccine schedule to analyze"),
                            choices = vax_choices),
                
                htmlOutput("vax_choice_info"), br(), 
                
                # Disables color choice on Animation tab
                
                conditionalPanel(
                  condition = "input.tabs == 'Plot Factors'",
                  pickerInput(
                    "color_choice", strong("Distinguish points by an additional factor"),
                    choices = color_choices)),
                
                
                checkboxInput("factor_incomplete", 'Count vaccine schedules that have been started but are behind schedule as completed?', FALSE),
                
                # Disables option to average on Animation tab
                
                conditionalPanel(
                  condition = "input.tabs == 'Plot Factors'",
                  checkboxInput("factor_years", "Average across years?", FALSE)
                ),
                
                # Disable linear regression summary stats on Animation tab
                
                br(), 
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
     about1_1 <- p(strong("Undoubtedly, the Affordable Care Act has improved health insurance rates across the US, but how---if at all---has it improved healthcare outcomes?"))
     about1_2 <- p("In particular, Vaccine Explorer allows users to get a hands-on look at how Medicaid expansion under the Affordable Care Act has affected vaccination rates across in US in recent years (spoiler alert: it hasn't affected them very much). By using Vaccine Explorer, it quickly becomes evident that Medicaid expansion has drastically improved insurance rates in the vast majority of states that have opted in. At the same time though, Vaccine Explorer also reveals that Medicaid expansion has done little to improve vaccination rates themselves. Lastly, Vaccine Explorer also uncovers that poverty and insurance rates are associated with vaccination rates, and that this association is stronger for some vaccines than others---revealing a way to measure the perceived importance of certain vaccination schedules.")
     
     about2 <- h3(strong("Key Takaways"))
     about2_1 <- p("-States that have opted for Medicaid expansion have seen dramatically improved insurance rates.")
     about2_2 <- p("-Medicaid expansion does not seem to substantially improve vaccination rates.")
     about2_3 <- p("-Insurance rates weakly associated with improved vaccination rates while poverty rates weakly associated with worsened vaccination rates.")
     about2_4 <- p("-Associations stronger in more critical vaccines like Hepatitis B, implying that people tend to opt-out of less critical vaccines.")
     about2_5 <- p("-Generally, vaccination rates in the US have stagnated with the exception of Rotavirus, which has seen a strong uptick in recent years.")
     
     about3 <- h3(strong("Background Information"))
     about3_1 <- p('One major goal of the Affordable Care Act (ACA), commonly known as "Obamacare", was to reduce the number of adults in the US without health insurance. The ACA has tried to accomplish this through two main avenues: the creation of a tightly regulated Health Insurance Marketplace and by allowing states to expand Medicaid coverage to most low-income adults. And while it has generally been accepted that the ACA has improved overall health insurance rates in the US, it remains ambiguous how to attribute those gains and whether or not Medicaid expansion has been beneficial. Additionally, it\'s difficult to determine if the ACA has actually improved healthcare outcomes directly (and if it has, how). These are the questions Vaccine Explorer hopes to answer.')
     
     about8 <- h3(strong("Data Sources"))
     about8_1 <- p("Vaccination rates data from the Centers for Disease Control (CDC) National Immunization Survey Public Use Files, 2010-2016 (child, 19-35 months of age)")
     about8_2 <- p("Medicaid Expansion Insurance data (2013-2016) from the US Census Bureau, Health Insurance in the United States: 2016 - Tables")
     about8_3 <- p("Historic Insurance data (2010-2012) from the US Census Bureau, Health Insurance Historical Tables - HIC Series.")
     about8_4 <- p("Poverty data from the US Census Bureau, Historical Poverty Tables: People and Families - 1959 to 2017")
     about8_5 <- p("States' kindergarten immunization requirements from ProCon.org")
     
     
     about9 <- h3(strong("Source Code"))
     about9_1 <- p("View the source code ", tags$a(href="https://github.com/R-Qiu/Vaccine-Explorer", "here"), ".")
     
     HTML(paste(about1, about1_1, about1_2, br(),
                about2, about2_1, about2_2, about2_3, about2_4, about2_5, br(),
                about3, about3_1, br(), 
                about8, about8_1, about8_2, about8_3, about8_4, about8_5, br(), 
                about9, about9_1))
       
   })
   
   
   
   
   ####################################
   # Factors Tab
   ####################################
   
   
   # These are the checkboxes to combine incomplete data and combien yearly data, respectively
   # Made reactive outside of any given renderXYZ function to avoid redundancy
   
   incomplete <- reactive({input$factor_incomplete})
   years <- reactive({input$factor_years})
   
   # Computes constant lower bound for given vaccine choice to give perspective when alternating between
   # Including or excluding vaccine schedules that are behind as complete
   # Does not filter based on selected states to preserve axes bounds when selecting/unselecting states
   
   lower_bound <- reactive({
        
     print(input$states)
     
     lower_bound <- 
       vax_factor %>% 
       filter(status == "adequate",
              vaccine == input$vax_choice,
              !is.na(eval(parse(text = input$factor_x)))) %>% 
       select(per_vax) %>% 
       arrange(per_vax) %>%
       head(1) %>% 
       pull()
     
     lower_bound
     
   })
   
   # Make reactive data manipulation reactive to avoid performing manipulation twice
   # This way, altered data can be moved imported into each render
   
   vax_factor_data <- reactive({
     
     req(input$states)
    
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
              state_name %in% input$states,
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
   
   
   
   # Graph for first tab
   # Chose static interactive scatter plot due to both axes being continuous
   # ggiraph interactivity chosen over Plotly due to customizability of hover tooltips 
   # Further, ggiraph allows for closer functionality to ggplot, allowing for finer customization
   
   output$vax_factor_plot <- renderggiraph({
     
     
     # Import data from reactive element
     
     data <- vax_factor_data()
     
     lower_bound <- lower_bound()
     
     
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
         ylim(lower_bound, 100)

     } else {

       g <-
         g +
         xlim(NA, 100) +
         ylim(lower_bound, 100)
     }

     
     # Differentially adds color/interactivity/etc. based on what axes and coloring options are selected
     
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
     
     lower_bound <- lower_bound()
     
     
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
         ylim(lower_bound, 100)
       
     } else {
       
       anim <-
         anim +
         xlim(NA, 100) +
         ylim(lower_bound, 100)
     }

     
     anim <-
       anim %>%
       animation_opts(5000, transition = 1000, easing = "elastic", redraw = FALSE)
     
     
     ggplotly(anim)

     
   })
   
   
   # Caption for the animation generated here as an HTML element
   # This is because Plotly animations don't seem to support ggplot captions
   # As a result, this workaround just places an HTML caption below the plot itself
   
   output$anim_caption <- renderUI({
     
     anim_caption_point <- "Each point represents a state in a given year."
     anim_caption_red <- paste( tags$span(style="color:red", "Red"), "indicates a state that has not expanded Medicaid under the Affordable Care Act.")
     anim_caption_blue <- paste( tags$span(style="color:blue", "Blue"), "indicates a state that has expanded Medicaid under the Affordable Care Act.")
     
     HTML(paste(anim_caption_point, anim_caption_red, anim_caption_blue, sep = "<br>"))
     
   })
   
   
   # Controls the output for the summary statistics section 
   # Runs a linear model on the data for simplicity and robustness 
   # Output from sjplot simplified to just the beta for ease of readability
   
   output$vax_factor_stats <- renderUI({
     
     data <- vax_factor_data()
     
     model_input <- formula(paste(" per_vax ~ ", input$factor_x))
     
     vax_factor_model <- lm(model_input, data)
     vax_factor_table <- tab_model(vax_factor_model, show.intercept = FALSE, 
                                   pred.labels = paste(filter(factor_lookup, symbol==input$factor_x)["name"]), 
                                   dv.labels = "Percent Vaccinated")
     HTML(vax_factor_table$knitr)
     
     
   })
   
   
   
   output$factor_x_info <- renderUI({
     
     if (input$factor_x == "per_ins") {
       
       factor_x_info <- p("Insurance rate is computed as percentage of households with health insurance from any source.")
       
     }
     
     if (input$factor_x == "per_pov") {
       
       factor_x_info <- p("Poverty rate is computed as percentage of households below the poverty line")
       
     }
     
     
     HTML(paste(factor_x_info))
     
   })

   
   # Vaccination requirements
   
   output$vax_choice_info <- renderUI({
     
     if (input$vax_choice == "dtap") {
       vax_choice_info <- p("The DTaP vaccine is a combo vaccine which immunizes against diptheria, pertussis (whooping cough), tetanus. The CDC recommends four does of DTaP for infants by 18 months of age.")
       vax_choice_info_school <- p("The DTap vaccine is required by all 50 states and DC for entry into public school kindergarten.")
     }

     if(input$vax_choice == "hepa") {
       vax_choice_info <- p("The CDC recommends three doses of the hepatitis A vaccine for infants before 23 months of age.")
       vax_choice_info_school <- p("The hepatitis A vaccine is required by 13 states and DC for entry into public school kindergarten.")
     }

     if(input$vax_choice == "hepb") {
       vax_choice_info <- p("The CDC recommends two doses of the hepatitis B vaccine for infants before 19 months of age.")
       vax_choice_info_school <- p("The hepatitis B vaccine is required by 43 states and DC for entry into public school kindergarten.")
     }

     if(input$vax_choice == "polio") {
       vax_choice_info <- p("The IPV vaccine is the injected version of the polio vaccine which is recommended by the CDC (over the oral version).The CDC recommends three doses of IPV for infants before 19 months of age.")
       vax_choice_info_school <- p("The IPV vaccine is required by all 50 states and DC for entry into public school kindergarten.")
     }

     if(input$vax_choice == "rota") {
       vax_choice_info <- p("The CDC recommends three doses of the rotavirus vaccine before 23 months of age.")
       vax_choice_info_school <- p("The rotavirus vaccine is required by 5 states for entry into public school kindergarten.")
     }
    
     if(input$vax_choice == "mmr") {
       vax_choice_info <- p("The MMR vaccine is a combo vaccine which immunizes against measles, mumps, and rubella. The CDC recommends one dose of the MMR vaccine before 19 months of age.")
       vax_choice_info_school <- p("The MMR vaccine is required by 50 states and DC for entry into public school kindergarten.")
     }

     if(input$vax_choice == "vrc") {
       vax_choice_info <- p("The varicella vaccine immunizes against chickenpox. The CDC recommends one dose of the varicella vaccine before 19 months of age.")
       vax_choice_info_school <- p("The varicella vaccine is required by 50 states and DC for entry into public school kindergarten.")
     }

     if(input$vax_choice == "pcv") {
       vax_choice_info <- p("The PCV vaccine immunizes against diseases caused by the Strep. Pneumoniae bacteria, which include pneumonia, meningitis, and sepsis. The CDC recommends four doses of the PCV vaccine before 19 months of age.")
       vax_choice_info_school <- p("The PCV vaccine is required by only 1 state (Connecticut) and DC for entry into public school kindergarten.")
     }
     
     
     HTML(paste(vax_choice_info, vax_choice_info_school))
     
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

