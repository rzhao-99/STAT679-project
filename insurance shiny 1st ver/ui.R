library(shiny)
library(shinyBS)
library(tidyverse)
library(collapsibleTree)

file_names <- dir("insurance_data_from_moneygeek/")
states<- str_split(file_names, '.json', simplify = TRUE)[,1]
states <- str_replace_all(states, "_", " ")
all_models <- c( "Electric", "Compact","Sedan","Sports Car","Compact SUV","SUV", "Minivan","Pickup Truck","Luxury Electric","Luxury Compact","Luxury Sedan", "Luxury Sports Car","Luxury Compact SUV","Luxury SUV")


ui <- fluidPage(
  titlePanel("Auto Insurance query"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("age", "Age:", min=18, max=99, value = 25, step=1),
      radioButtons("gender", "Gender:", c(
        "Male" = "M",
        "Female" = "F"
      )),
      selectInput("states", "State:", states, selected="Wisconsin"),
      selectInput("driver_record", "Driving Record:", c("Clean", "Speed", "Accident", "DUI"),"Clean"),
      selectInput("credit_scores", tags$span("Credit Score:",
                                             tags$i(
                                               class = "glyphicon glyphicon-info-sign", 
                                               style = "color:#0072B2;",
                                               title = "Your credit scores would fall into one of the following bands:\n
Excellent: 810+
Good: 750-809
Fair: 690-749
Below Fair: 600-689
Poor: 300-599")),
                  c("Excellent" = "excellent",
                    "Good" = "good",
                    "Fair" = "fair",
                    "Below Fair" = "belowFair",
                    "Poor" = "poor"
                  ), "Good"),
      selectInput("ins_type",tags$span("Liability Insurance:",
                                       tags$i(
                                         class = "glyphicon glyphicon-info-sign", 
                                         style = "color:#0072B2;",
                                         title = "Liability insurance covers medical and legal fees for which you've been held responsible after a carcrash.\n
Example liability limits of 50/100/50 mean: 
$50,000 in bodily injury insurance per person.
$100,000 in bodily injury insurance per accident.
$50,000 in property damage insurance peraccident.
")), c("StateMin", "50/100/50", "100/300/100"), "50/100/50"),
      selectInput("cc", "Comprehensive and Collision Insurance:", c(
        "No, I don't want." = "LO",
        "Yes, extra protect!" = "CC"
      ), "No, I don't want."),
      selectInput("model", "Vehicle Type:", all_models, "Sedan"),
      selectInput("Year", "Vehicle Year:", seq(2005,2021,1), 2018)
    ),
    mainPanel(textOutput("text"),
              collapsibleTreeOutput("plot"),
              verbatimTextOutput("text2"),
              plotOutput("plot2"))
  )
)
