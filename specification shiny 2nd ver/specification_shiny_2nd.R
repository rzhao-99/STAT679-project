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
cars <- read.csv('./USA_cars.csv')
#grouped_cars <- read.csv('grouped_cars.csv')






createLink <- function(year, trim) {
  # sprintf('<a href="https://www.google.com/#q=%s" target="_blank" class="btn btn-primary">More detail</a>',val)
  
  val = paste0(year, ' ' ,trim)
  sprintf('<a href="https://www.google.com/search?q=%s" target="_blank" class="btn btn-primary">More detail</a>',val)
  
}

cars$link = createLink(cars$Year, cars$Trim)



get_data<-function(make,model,year,body_type,price){
  if (input_provided(make) == TRUE){
    df=cars %>%
      filter(Make %in% make)
    
    if (input_provided(model) == TRUE){
      df=df %>%
        filter(Model %in% model)
    }
    if (input_provided(year) == TRUE){
      df=df %>%
        filter(Year %in% year)
    }
    if (input_provided(body_type) == TRUE){
      df=df %>%
        filter(Body_type %in% body_type)
    }
    df = df %>% filter(df$Price >  price[1], df$Price < price[2])
    return(df)
  }
  else{
    if (input_provided(year) == TRUE){
      df=cars %>%
        filter(Year %in% year)
      
      if (input_provided(make) == TRUE){
        df=df %>%
          filter(Make %in% make)
      }
      if (input_provided(model) == TRUE){
        df=df %>%
          filter(Model %in% model)
      }
      if (input_provided(body_type) == TRUE){
        df=df %>%
          filter(Body_type %in% body_type)
      }
      df = df %>% filter(df$Price >  price[1], df$Price < price[2])
      return(df)
    }
    if (input_provided(body_type) == TRUE){
      df=cars %>%
        filter(Body_type %in% body_type)
      
      if (input_provided(year) == TRUE){
        df=cars %>%
          filter(Year %in% year)
      }
      if (input_provided(make) == TRUE){
        df=cars %>%
          filter(Make %in% make)
      }
      if (input_provided(model) == TRUE){
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
  
  fluidRow(
    column(3, wellPanel(
      titlePanel('Basic Search'),
      textOutput('hint'),
      uiOutput('make_selection'),
      uiOutput('model_selection'),
      uiOutput('year_selection')
    ),
    
    wellPanel(
      titlePanel('Advanced Search'),
      uiOutput('body_type_selection'),
      uiOutput('slider')
    ),
    textOutput('range')
    ),
    column(9, dataTableOutput('vehicle_sub'))
  )
  
)



server <- function(input, output) {
  # output$range = renderText({
  #   #temp = cars %>% filter(between(Price, 10000, 20000))
  #   #print(unique(temp$Make))
  #   print(cbind(input$Price[1], input$Price[2]))
  # })
  
  #Text output
  output$hint = renderText({
    print('please select make before model' )
  })
  
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
                  choice = unique(cars[(cars$Make == input$Make),]$Year),
                  multiple = TRUE)
    }
    else if (input_provided(input$Make) & input_provided(input$Model)){
      selectInput('Year','Year',
                  choice = unique(cars[(cars$Make == input$Make)&
                                         (cars$Model == input$Model),'Year']),
                  multiple = TRUE)
      
    }
    else if (input_provided(input$Body_type)){
      selectInput('Year','Year',
                  choice = unique(cars[(cars$Body_type == input$Body_type),'Year']),
                  multiple = TRUE)
      
    }
    else if(input_provided(input$Make) & input_provided(input$Body_type)){
      selectInput('Year','Year',
                  choice = unique(cars[(cars$Make == input$Make)&
                                         (cars$Body_type == input$Body_type),'Year']),
                  multiple = TRUE)
      
    }
    else if(input_provided(input$Make) & input_provided(input$Model) & input_provided(input$Body_type)){
      selectInput('Year','Year',
                  choice = unique(cars[(cars$Make == input$Make)&
                                         (cars$Model == input$Model) &
                                         (cars$Body_type == input$Body_type),'Year']),
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
                  choice = unique(cars[(cars$Make == input$Make),]$Body_type),
                  multiple = TRUE)
    }
    else if(input_provided(input$Year)){
      selectInput('Body_type','Body Type',
                  choice = unique(cars[(cars$Year == input$Year),]$Body_type),
                  multiple = TRUE)
    }
    else if(input_provided(input$Make) & input_provided(input$Year)){
      selectInput('Body_type','Body Type',
                  choice = unique(cars[(cars$Make == input$Make)&
                                         (cars$Model == input$Year),]$Body_type),
                  multiple = TRUE)
    }
    else if (input_provided(input$Model) & input_provided(input$Make)){
      selectInput('Body_type','Body Type',
                  choice = unique(cars[(cars$Make == input$Make)&
                                         (cars$Model == input$Model),]$Body_type),
                  multiple = TRUE)
    }
    else if (input_provided(input$Make) & input_provided(input$Model) & input_provided(input$Year)){
      selectInput('Body_type','Body Type',
                  choice = unique(cars[(cars$Make == input$Make)&
                                         (cars$Model == input$Model) &
                                         (cars$Year == input$Year),]$Body_type),
                  multiple = TRUE)
    }
    
    else{
      selectInput('Body_type','Body Type',
                  choice = unique(cars$Body_type), multiple = TRUE)
    }
    # selectInput('Body_type','Body Type',
    #       choice = unique(cars$Body_type), multiple = TRUE)
  })
  
  
  
  #Slider input for price
  output$slider = renderUI({
    #单一option
    if (input_provided(input$Make)){
      cur_df = cars[cars$Make == input$Make,]
      sliderInput('Price','Price', min = min(cur_df$Price), max = max(cur_df$Price), value = c(min(cur_df$Price),max(cur_df$Price)) ,pre = '$', sep = ',', animate = TRUE)
    }
    else if(input_provided(input$Year)){
      cur_df = cars[cars$Year == input$Year,]
      sliderInput('Price','Price', min = min(cur_df$Price),  max = max(cur_df$Price), value = c(min(cur_df$Price),max(cur_df$Price)) ,pre = '$', sep = ',', animate = TRUE)
    }
    else if(input_provided(input$Body_type)){
      cur_df = cars[cars$Body_type == input$Body_type,]
      sliderInput('Price','Price',min = min(cur_df$Price), max = max(cur_df$Price), value =c(min(cur_df$Price),max(cur_df$Price)) ,pre = '$', sep = ',', animate = TRUE)
    }
    #双option
    else if(input_provided(input$Make) & input_provided(input$Model)){
      cur_df = cars[(cars$Make == input$Make)&
                      (cars$Model == input$Model),]
      sliderInput('Price','Price',min = min(cur_df$Price), max = max(cur_df$Price), value = c(min(cur_df$Price),max(cur_df$Price)) ,pre = '$', sep = ',', animate = TRUE)
    }
    else if(input_provided(input$Make) & input_provided(input$Year)){
      cur_df = cars[(cars$Make == input$Make)&
                      (cars$Year == input$Year),]
      sliderInput('Price','Price', min = min(cur_df$Price), max = max(cur_df$Price), value = c(min(cur_df$Price),max(cur_df$Price)) ,pre = '$', sep = ',', animate = TRUE)
    }
    else if(input_provided(input$Make) & input_provided(input$Body_type)){
      cur_df = cars[(cars$Make == input$Make)&
                      (cars$Body_type == input$Body_type),]
      sliderInput('Price','Price', min = min(cur_df$Price), max = max(cur_df$Price), value = c(min(cur_df$Price),max(cur_df$Price)),pre = '$', sep = ',', animate = TRUE)
    }
    else if(input_provided(input$Year) & input_provided(input$Body_type)){
      cur_df = cars[(cars$Year == input$Year)&
                      (cars$Body_type == input$Body_type),]
      sliderInput('Price','Price' ,min = min(cur_df$Price), max = max(cur_df$Price), value = c(min(cur_df$Price),max(cur_df$Price)), pre = '$', sep = ',', animate = TRUE)
    }
    #三option
    else if(input_provided(input$Make) & input_provided(input$Model) &
            input_provided(input$Year)){
      cur_df = cars[(cars$Make == input$Make)&
                      (cars$Model == input$Model)&
                      (cars$Year == input$Year),]
      sliderInput('Price','Price',min = min(cur_df$Price), max = max(cur_df$Price),c(min(cur_df$Price),max(cur_df$Price)) ,pre = '$', sep = ',', animate = TRUE)
    }
    else if(input_provided(input$Make) & input_provided(input$Model) &
            input_provided(input$Body_type)){
      cur_df = cars[(cars$Make == input$Make)&
                      (cars$Model == input$Model)&
                      (cars$Body_type == input$Body_type),]
      sliderInput('Price','Price', min = min(cur_df$Price),  max = max(cur_df$Price), value =c(min(cur_df$Price),max(cur_df$Price)),pre = '$', sep = ',', animate = TRUE)
    }
    else if(input_provided(input$Make) & input_provided(input$Year) &
            input_provided(input$Body_type)){
      cur_df = cars[(cars$Make == input$Make)&
                      (cars$Year == input$Year)&
                      (cars$Body_type == input$Body_type),]
      sliderInput('Price','Price', min = min(cur_df$Price), max = max(cur_df$Price),value = c(min(cur_df$Price),max(cur_df$Price)), pre = '$', sep = ',', animate = TRUE)
    }
    #四option
    else if(input_provided(input$Make) & input_provided(input$Year) &
            input_provided(input$Body_type) & input_provided(input$Model)){
      cur_df = cars[(cars$Make == input$Make)&
                      (cars$Model == input$Model)&
                      (cars$Year == input$Year)&
                      (cars$Body_type == input$Body_type),]
      sliderInput('Price','Price',min = min(cur_df$Price),  max = max(cur_df$Price),  value = min(cur_df$Price) ,pre = '$', sep = ',', animate = TRUE)
    }
    else{
      sliderInput('Price','Price',min = min(cars$Price), 
                  max = max(cars$Price), value = c(min(cars$Price),max(cars$Price)) , pre = '$', sep = ',', animate = TRUE)
    }
    
    
    # sliderInput('Price','Price', value = min(cars$Price) ,min = min(cars$Price), max = max(cars$Price), pre = '$', sep = ',', animate = TRUE)
  })
  
  
  
  
  
  
  df<-reactive({
    get_data(input$Make,input$Model,input$Year, input$Body_type, input$Price)
  })
  
  output$vehicle_sub<- renderDataTable({
    df()
  }, escape = FALSE)
  
  
}


shinyApp(ui, server)