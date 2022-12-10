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
library(rjson)
library(collapsibleTree)
library(colorspace)
library(shinyBS)
library(RColorBrewer)

cars <- read.csv('USA_cars.csv')
price_data = read.csv('Price_table.csv')
sales_data = read.csv('Sales_table.csv')

maker1 = unique(price_data$Maker)
maker2 = unique(sales_data$Maker)

# Insurance part info for UI

all_models <- c( "Electric", "Compact","Sedan","Sports Car","Compact SUV","SUV", "Minivan","Pickup Truck","Luxury Electric","Luxury Compact","Luxury Sedan", "Luxury Sports Car","Luxury Compact SUV","Luxury SUV")
file_names <- dir("insurance_data_from_moneygeek/")
states<- str_split(file_names, '.json', simplify = TRUE)[,1]
states <- str_replace_all(states, "_", " ")

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
      ),
      
      wellPanel(
        titlePanel("Auto Insurance query"),
        uiOutput('age'),
        uiOutput('gender'),
        uiOutput('states'),
        uiOutput('driver_record'),
        uiOutput('credit_scores'),
        uiOutput('ins_type'),
        uiOutput('cc'),
        uiOutput('model'),
        uiOutput('Year')
    )
    ),

  mainPanel(
    tabsetPanel(
      tabPanel('Vechile Specification',
               column(9, dataTableOutput('vehicle_sub'))),

      tabPanel('Sales & Price Trend',
               #selectInput("maker1","Maker for price data", maker1, multiple = TRUE),
               #selectInput("maker2","Maker for sales data", maker2, multiple = TRUE),
               plotOutput("plot1", brush = "plot1_brush"),
               plotOutput("plot2", brush = "plot2_brush"),
               dataTableOutput("table1"),
               dataTableOutput("table2")
      ),
      # Insurance part
      tabPanel('Insurance Calculator', 
               textOutput("tree_text"),
               collapsibleTreeOutput("tree_plot"),
               plotOutput("bar_plot")
      )
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

  reset_selection <- function(x, brush) {
  brushedPoints(x, brush, allRows = TRUE)$selected_
}

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

  selected1 <- reactiveVal(rep(TRUE, nrow(price_data)))
  selected2 <- reactiveVal(rep(TRUE, nrow(sales_data)))

  observeEvent(
    input$plot1_brush,
    selected1(reset_selection(price_data, input$plot1_brush))
  )

  observeEvent(
    input$plot2_brush,
    selected2(reset_selection(sales_data, input$plot2_brush))
  )

  output$table1<- renderDataTable({
    filter(price_data, selected1())
  })
  output$table2<- renderDataTable({
    filter(sales_data, selected2())
  })


  #------------------------------------Sales & Price Trend----------------------------------#

  #---------------------------------------   Insurance    ----------------------------------#
  output$age = renderUI({
    sliderInput("tree_age", "Age:", min=18, max=99, value = 25, step=1)
  })
  
  output$gender = renderUI({
    radioButtons("tree_gender", "Gender:", c(
      "Male" = "M",
      "Female" = "F"
    ))
  })
    
  output$states = renderUI({
    selectInput("tree_states", "State:", states, selected="Wisconsin")
  })
  
  output$driver_record = renderUI({
    selectInput("tree_driver_record", "Driving Record:", c("Clean", "Speed", "Accident", "DUI"),"Clean")
  })
  
  output$credit_scores = renderUI({
    selectInput("tree_credit_scores", tags$span("Credit Score:",
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
                ), "Good")
  })
  
  output$ins_type = renderUI({
    selectInput("tree_ins_type",tags$span("Liability Insurance:",
                                     tags$i(
                                       class = "glyphicon glyphicon-info-sign", 
                                       style = "color:#0072B2;",
                                       title = "Liability insurance covers medical and legal fees for which you've been held responsible after a carcrash.\n
Example liability limits of 50/100/50 mean: 
$50,000 in bodily injury insurance per person.
$100,000 in bodily injury insurance per accident.
$50,000 in property damage insurance peraccident.
")), c("StateMin", "50/100/50", "100/300/100"), "50/100/50")
  })
  
  output$cc = renderUI({
    selectInput("tree_cc", "Comprehensive and Collision Insurance:", c(
      "No, I don't want." = "LO",
      "Yes, extra protect!" = "CC"
    ), "No, I don't want.")
  })
  
  output$model = renderUI({
    selectInput("tree_model", "Vehicle Type:", all_models, "Sedan")
  })
  
  output$Year = renderUI({
    selectInput("tree_Year", "Vehicle Year:", seq(2005,2021,1), 2018)
  })
    
  
  # INPUT 
  
  ## USER
  ## AGE 
  ## M/F
  ## Driving Record: clean speed accident DUI
  ## State: WI 50+1
  ## Liability Coverage: State MIN / 50/100/50 / 100/300/100
  ## Comprehensive and Collision Insurance: LO / CC
  ## Credit Scores
  
  
  ## VEHICLE
  ## BODY TYPE
  ## YEAR
  
  # OUTPUT
  ## MIN 460
  ## AVG 700
  ## MAX 1100
  
  # credit scores
  # Excellent 810
  # Good 750-809
  # Fair 690-749
  # Below Fair: 600-689
  # Poor: 300-599
  find_creditscores <- function(result, credit){
    return(unlist(result$creditScores[[credit]]))
  }
  
  # Age:
  # 19-20:
  # 21-22:
  # 23-25:
  # 26-29:
  # 30+: 1.0
  
  find_age <- function(result, age){
    if (age>=19 && age <= 20){
      return(result$nonCoreAge$`19-20`)
    }else if(age >= 21 && age <= 22){
      return(result$nonCoreAge$`21-22`)
    }else if(age>=23 && age <=25){
      return(result$nonCoreAge$`23-25`)
    }else if(age>=26 && age <= 29){
      return(result$nonCoreAge$`26-29`)
    }else{
      return(1)
    }
  }
  
  # body_types + Year
  # all_models = c()
  # for(i in c(1:14)){
  #   k = result$modelYear[[i]]
  #   all_models = c(all_models, k$type)
  # }

  
  find_car_inf <- function(result, model, year){
    year = as.character(year)
    for (i in c(1:length(result$modelYear))){
      if (result$modelYear[[i]]$type == model){
        break
      }
    }
    year_list = names(result$modelYear[[i]])
    for (j in c(1:(length(year_list)-1))){
      if (year == year_list[j]){
        return(result$modelYear[[i]][[year]])
      }
    }
    if (as.numeric(year) < as.numeric(year_list[1])){
      return(result$modelYear[[i]][[year_list[1]]])
    }else{
      return(result$modelYear[[i]][[year_list[length(year_list) - 1]]])
    }
  }
  
  # Insurance Type
  # Age+M/F+
  # c(StateMin, 50/100/50, 100/300/100) + 
  # c(CC, LO) + 
  # c(Clean, Speed, Accident, DUI)
  
  find_insurance_type <- function(result, age, gender, ins_type, cc, driver_record, company){
    if (age >= 65){
      age = 70
    }else{
      age = 40
    }
    query <- paste0(age, gender, ins_type, cc, driver_record)
    if(company == FALSE){
      val = unlist(result$profiles[[query]])
      if (is.null(val)){
        return(list(-1))
      }
      return(list(c("Overall", val)))
    }
    res = list()
    for (i in c(1:length(result$companies))){
      val = unlist(result$companies[[i]][[query]])
      if (is.null(val)){
        return(list(-1))
      }
      name = result$companies[[i]]$company$name
      res[[i]] = c(name, val)
    }
    return(res)
  }
  
  # level = 1 => min, 2 => max, 3 => avg
  calculate_price <- function(result, base_price_str, age, credit_scores, model, year, level){
    base_price_str = str_replace_all(base_price_str, ",", "")
    base_price_str = substr(base_price_str, 2, nchar(base_price_str))
    price = as.numeric(base_price_str)
    
    price = price * as.numeric(find_age(result, age))
    price = price * as.numeric(find_creditscores(result,credit_scores)[level])
    price = price * as.numeric(find_car_inf(result, model, year))
    return(price)
  }
  
  # define colors
  pal = sequential_hcl(50, h1=-4, h2=80, c1=100, c2=47, l1=55, l2=96, p1=1.0, rev = TRUE)
  map2color<-function(x,pal,limits=NULL){
    if(is.null(limits)) limits=range(x)
    pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
  }
  
  # min/max/avg company/overall names price
  generate_cur_data <- function(state, age, gender, ins_type, cc, driver_record, credit_scores, model, year){
    file_name = str_replace_all(state, " ", "_")
    file_path = paste0("insurance_data_from_moneygeek/", file_name, ".json")
    result <- rjson::fromJSON(file = file_path)
    
    # Overall price
    overall = find_insurance_type(result, age, gender, ins_type, cc, driver_record, FALSE)
    if (overall[[1]][1] == -1){
      return(-1)
    }
    level = 1
    val = calculate_price(result, overall[[1]][level+1], age, credit_scores, model, year, level)
    data <- as.data.frame(matrix(c(paste("min:", val), "Average", overall[[1]][1], val), nrow = 1))
    colnames(data) <- c("MinMaxAvg", "CompanyOverall", "Company_names", "Price")
    level = 2
    val = calculate_price(result, overall[[1]][level+1], age, credit_scores, model, year, level)
    data[nrow(data)+1,] <- c(paste("max:", val), "Average", overall[[1]][1], val)
    level = 3
    val = calculate_price(result, overall[[1]][level+1], age, credit_scores, model, year, level)
    data[nrow(data)+1,] <- c(paste("avg:", val), "Average", overall[[1]][1], val)
    
    # Prices for each company
    companies = find_insurance_type(result, age, gender, ins_type, cc, driver_record, TRUE)
    if (companies[[1]][1] == -1){
      return(-1)
    }
    for (i in c(1:length(companies))){
      company = companies[[i]]
      level = 1
      val = calculate_price(result, company[level+1], age, credit_scores, model, year, level)
      data[nrow(data)+1,] <- c(paste("min:", val), "Companies", company[1],val )
      level = 2
      val = calculate_price(result, company[level+1], age, credit_scores, model, year, level)
      data[nrow(data)+1,] <- c(paste("max:", val), "Companies", company[1], val)
      level = 3
      val = calculate_price(result, company[level+1], age, credit_scores, model, year, level)
      data[nrow(data)+1,] <- c(paste("avg:", val), "Companies", company[1], val)
    }
    data$Price = as.numeric(data$Price)
    return(data)
  }
  
  
  select_data <- function(data, selected){
    new_data = data %>%
      filter(Company_names %in% selected)
    new_data$MinMaxAvg = substr(new_data$MinMaxAvg,1,3)
    new_data
  }
  
  order_fun_1 <- function(X){
    if (X == "Min"){
      1
    }
    if (X == "Avg"){
      2
    }
    3
  }
  
  plot_bar <- function(data){
    data %>%
      mutate(level = factor(MinMaxAvg, levels = c("min","avg","max"))) %>%
      ggplot(aes(fill = level, y = Price, x = reorder(Company_names, Price, sum))) + 
      geom_bar(position="dodge",stat='identity')+ 
      labs(x = "Company Names/Overall")
  }
  
  last_input = ""
  
  data <- reactive({
    age = input$tree_age
    state = input$tree_states
    gender = input$tree_gender
    ins_type = input$tree_ins_type
    cc = input$tree_cc
    driver_record = input$tree_driver_record
    credit_scores = input$tree_credit_scores
    model = input$tree_model
    year = input$tree_Year
    generate_cur_data(state, age, gender, ins_type, cc, driver_record, credit_scores, model, year)
  })
  
  output$tree_text <- renderText({
    age = input$tree_age
    state = input$tree_states
    gender = input$tree_gender
    ins_type = input$tree_ins_type
    cc = input$tree_cc
    driver_record = input$tree_driver_record
    credit_scores = input$tree_credit_scores
    model = input$tree_model
    year = input$tree_Year
    cur_data = data()
    if (length(cur_data) == 1){
      return("We don't have enough information in this setting, please try another settings.")
    }
    paste("What you have chosen: ",age,state,gender,ins_type, cc, driver_record, credit_scores,model, year)
  })
  
  output$tree_plot <- renderCollapsibleTree({
    cur_data = data()
    if (length(cur_data) == 1){
      return(NULL)
    }
    ## Compute the mean 
    group_mean <- cur_data %>% 
      filter(substr(MinMaxAvg,1,3) == "avg") %>%
      mutate(Company_names = Company_names, mean = Price, .keep="none")
    overall_mean <- group_mean %>% filter(Company_names == "Overall")
    company_mean <- group_mean %>% filter(Company_names != "Overall")
    
    
    collapsibleTree(
      cur_data,
      hierarchy = c("CompanyOverall", "Company_names", "MinMaxAvg"),
      root = c("Price"),
      fill = c(
        # The root
        "seashell",
        # CompanyOverall
        map2color(c(1,1), pal),
        # Company average,
        map2color(c(overall_mean$mean, company_mean$mean), pal),
        # min max avg
        map2color(cur_data$Price, pal)
      ),
      attribute = c("Price"),
      aggFun = mean,
      inputId = "node"
    )
  })
  
  output$bar_plot <- renderPlot({
    cur_data = data()
    if (length(cur_data) == 1){
      return(NULL)
    }
    
    age = input$tree_age
    state = input$tree_states
    gender = input$tree_gender
    ins_type = input$tree_ins_type
    cc = input$cc
    driver_record = input$tree_driver_record
    credit_scores = input$tree_credit_scores
    model = input$tree_model
    year = input$tree_Year
    
    val = input$node$Company_names
    if (!is.null(val) && val != "Overall" && is.null(input$node$MinMaxAvg)){
      if (val %in% selected){
        selected <<- selected[-which(selected == val)]
      }else{
        selected <<- c(selected,val)
      }
    }
    
    if (last_input != paste(age,state,gender,ins_type, cc, driver_record, credit_scores,model, year)){
      last_input <<- paste(age,state,gender,ins_type, cc, driver_record, credit_scores,model, year)
      selected <<- c("Overall")
    }
    
    cur_data <- select_data(cur_data, selected)
    plot_bar(cur_data)
  })
  
  #---------------------------------------   Insurance    ----------------------------------#
}


shinyApp(ui, server)