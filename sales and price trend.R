library(ggplot2)
library(tidyverse)
library(shiny)
library(data.table)





price_data = read.csv('Price_table.csv')
sales_data = read.csv('Sales_table.csv')
sales_data = select(sales_data, -Genmodel_ID)
colnames(sales_data) = c("Maker","Genmodel","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001")
sales_data = as.data.table(sales_data)
sales_data = melt.data.table(sales_data, variable.name = "year", value.name = 'sales')
sales_data$year = as.numeric(as.character(sales_data$year))





maker1 = unique(price_data$Maker)
maker2 = unique(sales_data$Maker)




ui <- fluidPage(
  titlePanel("Select maker and you will see its genmodels' sales and price trend"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("maker1","Maker for sales data", maker1, multiple = TRUE),
      selectInput("maker2","Maker for price data", maker2, multiple = TRUE)
    ),
    
    mainPanel(plotOutput("plot1"), plotOutput("plot2"))
    
  ),
  
)



server <- function(input, output){
  
  current_data1 <- reactive({
    data <- price_data %>%
      filter(Maker %in% input$maker1)
  })
  
  current_data2 <- reactive({
    data <- sales_data %>%
      filter(Maker %in% input$maker2)
  })
  
  output$plot1 <- renderPlot({
    ggplot(current_data1(), aes(x=Year, y=Entry_price, col=Genmodel)) + 
      geom_line() +
      scale_x_continuous("price",breaks=c(1998,2005,2013,2021), labels=c("1998","2005","2013","2021"), limits = c(1998,2021))
  })
  
  output$plot2 <- renderPlot({
    ggplot(current_data2(), aes(x=year, y=sales, col=Genmodel)) + 
      geom_line() +
      scale_x_continuous("price",breaks=c(1998,2005,2013,2021), labels=c("1998","2005","2013","2021"), limits = c(1998,2021))
  })
  
}

shinyApp(ui, server)