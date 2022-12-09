library(shiny)
library(shinyvalidate)
library(shinyjs)
library(tidyverse)
library(WVPlots)
library(rsconnect)
library(XML)
library(RCurl)
library(rvest)
library(dplyr)
library(stringr)
library(rvest)
cars <- read.csv('USA_cars.csv')
price_data = read.csv('Price_table.csv')
sales_data = read.csv('Sales_table.csv')

maker1 = unique(price_data$Maker)
maker2 = unique(sales_data$Maker)


createLink <- function(year, trim) {
  val = paste0(year, ' ' ,trim)
  sprintf('<a href="https://www.google.com/search?q=%s" target="_blank" class="btn btn-primary">More detail</a>',val)
  
}

cars$link = createLink(cars$Year, cars$Trim)



get_data<-function(make,model,year,body_type,price,fuel_type){
  if (input_provided(make)){
    df=cars %>%
      filter(Make %in% make)
    
    if (input_provided(model)){
      df=df %>%
        filter(Model %in% model)
    }
    if (input_provided(year)){
      df=df %>%
        filter(Year %in% year)
    }
    if (input_provided(body_type)){
      df=df %>%
        filter(Body_type %in% body_type)
    }
    if(input_provided(fuel_type)){
      df=df %>%
        filter(Fuel_type %in% fuel_type)
    }
    df = df %>% filter(df$Price >  price[1], df$Price < price[2])
    return(df)
  }
  else{
    if (input_provided(year)){
      df=cars %>%
        filter(Year %in% year)
      
      if (input_provided(make)){
        df=df %>%
          filter(Make %in% make)
      }
      if (input_provided(model)){
        df=df %>%
          filter(Model %in% model)
      }
      if (input_provided(body_type)){
        df=df %>%
          filter(Body_type %in% body_type)
      }
      df = df %>% filter(df$Price >  price[1], df$Price < price[2])
      return(df)
    }
    if (input_provided(body_type)){
      df=cars %>%
        filter(Body_type %in% body_type)
      
      if (input_provided(year)){
        df=cars %>%
          filter(Year %in% year)
      }
      if (input_provided(make)){
        df=cars %>%
          filter(Make %in% make)
      }
      if (input_provided(model)){
        df=df %>%
          filter(Model %in% model)
      }
      df = df %>% filter(df$Price >  price[1], df$Price < price[2])
      return(df)
    }
  }
  
  return(df)
}


ui <- fluidPage(
  #set title
  titlePanel("Find your dream vehicle!!!"),
  
  tags$head(
    tags$style(HTML(" .shiny-output-error-validation {color: #ff0000;font-weight: bold;}"))),
  
  #fluidRow(
    column(3, wellPanel(
      titlePanel('Basic Search'),
      uiOutput('make_selection'),
      uiOutput('model_selection'),
      uiOutput('year_selection'),
      uiOutput('slider')
      ),
    
      wellPanel(
        titlePanel('Advanced Search'),
        uiOutput('body_type_selection'),
        uiOutput('fuel_type_selection')
      )
    ),
  #),
  mainPanel(
    tabsetPanel(
      tabPanel('Vechile Specification', 
               column(9, dataTableOutput('vehicle_sub'))),
      
      tabPanel('Sales & Price Trend',
               #selectInput("maker1","Maker for price data", maker1, multiple = TRUE),
               #selectInput("maker2","Maker for sales data", maker2, multiple = TRUE),
               plotOutput("plot1"), 
               plotOutput("plot2")
      ),
      
      tabPanel('最后一部分')
    )
  )
  
)



server <- function(input, output) {
  #------------------------------------Specification----------------------------------#
  #make selection
  output$make_selection = renderUI({
    selectInput("Make", "Make", choices = unique(cars$Make), multiple = TRUE)
  })
  
  
  #model selection
  output$model_selection = renderUI({
    selectInput('Model','Model', choice = unique(cars[cars$Make %in%
                                                        input$Make,'Model']), multiple = TRUE)
  })
  
  
  #year selection
  output$year_selection = renderUI({
    if (input_provided(input$Make) == TRUE){
      selectInput('Year','Year',
                  choice = unique(cars[(cars$Make %in% input$Make),]$Year),
                  multiple = TRUE)
    }
    else if (input_provided(input$Make) & input_provided(input$Model)){
      selectInput('Year','Year',
                  choice = unique(cars[(cars$Make %in% input$Make)&
                                         (cars$Model  %in% input$Model),'Year']),
                  multiple = TRUE)
      
    }
    else if (input_provided(input$Body_type)){
      selectInput('Year','Year',
                  choice = unique(cars[(cars$Body_type %in% input$Body_type),'Year']),
                  multiple = TRUE)
      
    }
    else if(input_provided(input$Make) & input_provided(input$Body_type)){
      selectInput('Year','Year',
                  choice = unique(cars[(cars$Make %in% input$Make)&
                                         (cars$Body_type %in% input$Body_type),'Year']),
                  multiple = TRUE)
      
    }
    else if(input_provided(input$Make) & input_provided(input$Model) & input_provided(input$Body_type)){
      selectInput('Year','Year',
                  choice = unique(cars[(cars$Make %in% input$Make)&
                                         (cars$Model  %in% input$Model) &
                                         (cars$Body_type %in% input$Body_type),'Year']),
                  multiple = TRUE)
    }
    else{
      selectInput('Year','Year',choice = unique(cars$Year),multiple = TRUE)
    }
    selectInput('Year','Year',choice = unique(cars$Year),multiple = TRUE)
    
  })
  
  
  #body type selection
  output$body_type_selection = renderUI({
    if (input_provided(input$Make)){
      selectInput('Body_type','Body Type',
                  choice = unique(cars[(cars$Make %in% input$Make),]$Body_type),
                  multiple = TRUE)
    }
    else if(input_provided(input$Year)){
      selectInput('Body_type','Body Type',
                  choice = unique(cars[(cars$Year %in% input$Year),]$Body_type),
                  multiple = TRUE)
    }
    else if(input_provided(input$Make) & input_provided(input$Year)){
      selectInput('Body_type','Body Type',
                  choice = unique(cars[(cars$Make %in% input$Make)&
                                         (cars$Model == input$Year),]$Body_type),
                  multiple = TRUE)
    }
    else if (input_provided(input$Model) & input_provided(input$Make)){
      selectInput('Body_type','Body Type',
                  choice = unique(cars[(cars$Make %in% input$Make)&
                                         (cars$Model  %in% input$Model),]$Body_type),
                  multiple = TRUE)
    }
    else if (input_provided(input$Make) & input_provided(input$Model) & input_provided(input$Year)){
      selectInput('Body_type','Body Type',
                  choice = unique(cars[(cars$Make %in% input$Make)&
                                         (cars$Model  %in% input$Model) &
                                         (cars$Year %in% input$Year),]$Body_type),
                  multiple = TRUE)
    }
    
    else{
      selectInput('Body_type','Body Type',
                  choice = unique(cars$Body_type), multiple = TRUE)
    }
  })
  
  
  
  #Slider input for price
  output$slider = renderUI({
    #单一option
    if (input_provided(input$Make)){
      cur_df = cars[cars$Make %in% input$Make,]
      sliderInput('Price','Price', min = min(cur_df$Price), max = max(cur_df$Price), value = c(min(cur_df$Price),max(cur_df$Price)) ,pre = '$', sep = ',', animate = TRUE)
    }
    else if(input_provided(input$Year)){
      cur_df = cars[cars$Year %in% input$Year,]
      sliderInput('Price','Price', min = min(cur_df$Price),  max = max(cur_df$Price), value = c(min(cur_df$Price),max(cur_df$Price)) ,pre = '$', sep = ',', animate = TRUE)
    }
    else if(input_provided(input$Body_type)){
      cur_df = cars[cars$Body_type %in% input$Body_type,]
      sliderInput('Price','Price',min = min(cur_df$Price), max = max(cur_df$Price), value =c(min(cur_df$Price),max(cur_df$Price)) ,pre = '$', sep = ',', animate = TRUE)
    }
    #双option
    else if(input_provided(input$Make) & input_provided(input$Model)){
      cur_df = cars[(cars$Make %in% input$Make)&
                      (cars$Model  %in% input$Model),]
      sliderInput('Price','Price',min = min(cur_df$Price), max = max(cur_df$Price), value = c(min(cur_df$Price),max(cur_df$Price)) ,pre = '$', sep = ',', animate = TRUE)
    }
    else if(input_provided(input$Make) & input_provided(input$Year)){
      cur_df = cars[(cars$Make %in% input$Make)&
                      (cars$Year %in% input$Year),]
      sliderInput('Price','Price', min = min(cur_df$Price), max = max(cur_df$Price), value = c(min(cur_df$Price),max(cur_df$Price)) ,pre = '$', sep = ',', animate = TRUE)
    }
    else if(input_provided(input$Make) & input_provided(input$Body_type)){
      cur_df = cars[(cars$Make %in% input$Make)&
                      (cars$Body_type %in% input$Body_type),]
      sliderInput('Price','Price', min = min(cur_df$Price), max = max(cur_df$Price), value = c(min(cur_df$Price),max(cur_df$Price)),pre = '$', sep = ',', animate = TRUE)
    }
    else if(input_provided(input$Year) & input_provided(input$Body_type)){
      cur_df = cars[(cars$Year %in% input$Year)&
                      (cars$Body_type %in% input$Body_type),]
      sliderInput('Price','Price' ,min = min(cur_df$Price), max = max(cur_df$Price), value = c(min(cur_df$Price),max(cur_df$Price)), pre = '$', sep = ',', animate = TRUE)
    }
    #三option
    else if(input_provided(input$Make) & input_provided(input$Model) &
            input_provided(input$Year)){
      cur_df = cars[(cars$Make %in% input$Make)&
                      (cars$Model  %in% input$Model)&
                      (cars$Year %in% input$Year),]
      sliderInput('Price','Price',min = min(cur_df$Price), max = max(cur_df$Price),c(min(cur_df$Price),max(cur_df$Price)) ,pre = '$', sep = ',', animate = TRUE)
    }
    else if(input_provided(input$Make) & input_provided(input$Model) &
            input_provided(input$Body_type)){
      cur_df = cars[(cars$Make %in% input$Make)&
                      (cars$Model  %in% input$Model)&
                      (cars$Body_type %in% input$Body_type),]
      sliderInput('Price','Price', min = min(cur_df$Price),  max = max(cur_df$Price), value =c(min(cur_df$Price),max(cur_df$Price)),pre = '$', sep = ',', animate = TRUE)
    }
    else if(input_provided(input$Make) & input_provided(input$Year) &
            input_provided(input$Body_type)){
      cur_df = cars[(cars$Make %in% input$Make)&
                      (cars$Year %in% input$Year)&
                      (cars$Body_type %in% input$Body_type),]
      sliderInput('Price','Price', min = min(cur_df$Price), max = max(cur_df$Price),value = c(min(cur_df$Price),max(cur_df$Price)), pre = '$', sep = ',', animate = TRUE)
    }
    #四option
    else if(input_provided(input$Make) & input_provided(input$Year) &
            input_provided(input$Body_type) & input_provided(input$Model)){
      cur_df = cars[(cars$Make %in% input$Make)&
                      (cars$Model  %in% input$Model)&
                      (cars$Year %in% input$Year)&
                      (cars$Body_type %in% input$Body_type),]
      sliderInput('Price','Price',min = min(cur_df$Price),  max = max(cur_df$Price),  value = min(cur_df$Price) ,pre = '$', sep = ',', animate = TRUE)
    }
    else{
      sliderInput('Price','Price',min = min(cars$Price), 
                  max = max(cars$Price), value = c(min(cars$Price),max(cars$Price)) , pre = '$', sep = ',', animate = TRUE)
    }
    
  })
  
  
  
  df<-reactive({
    get_data(input$Make,input$Model,input$Year, input$Body_type, input$Price,input$fuel)
  })
  
  #input_provided(input$price) |
  output$vehicle_sub<- renderDataTable({
    if((input_provided(input$Make) | input_provided(input$Model)|
       input_provided(input$Body_type)|
       input_provided(input$Year)) == TRUE){
      df()
    }
    else {
      cars
    }
    #df()
    
  }, escape = FALSE)
  
  
  output$fuel_type_selection = renderUI ({
    if(input_provided(input$Make) == FALSE){
      selectInput('fuel','Fuel Type', choice = unique(cars$Fuel_type), multiple = TRUE)
    }
    else{
      cur_df = cars[cars$Make %in% input$Make, ]
      selectInput('fuel','Fuel Type', choice = unique(cur_df$Fuel_type), multiple = TRUE)
    }
  })
  
  
  #------------------------------------Specification----------------------------------#
  
  #------------------------------------Sales & Price Trend----------------------------------#
  
  
  current_data1 <- reactive({
    data <- price_data %>%
      filter(Maker %in% input$Make)
  })
  
  current_data2 <- reactive({
    data <- sales_data %>%
      filter(Maker %in% toupper(input$Make))
  })
  
  output$plot1 <- renderPlot({
    ggplot(current_data1(), aes(x=Year, y=Entry_price, col=Genmodel)) + 
      geom_line() +
      scale_x_continuous("price",breaks=c(1998,2005,2013,2021), labels=c("1998","2005","2013","2021"), limits = c(1998,2021))
  })
  
  output$plot2 <- renderPlot({
    ggplot(current_data2(), aes(x=year, y=sales, col=Genmodel)) + 
      geom_line() +
      scale_x_continuous("sales",breaks=c(1998,2005,2013,2021), labels=c("1998","2005","2013","2021"), limits = c(1998,2021))
  })

  
  
  
  
  #------------------------------------Sales & Price Trend----------------------------------#
  
}






































shinyApp(ui, server)