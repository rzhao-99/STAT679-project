library(shiny)
library(tidyverse)
library(ggplot2)
library(rjson)
library(collapsibleTree)
require(colorspace)
library(shinyBS)



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


# Use for debug
l <- list()
l[[1]] <- c(-1)
l[[2]] <- c(1)


selected = c()
last_input = ""

server <- function(input, output){
  data <- reactive({
    age = input$age
    state = input$states
    gender = input$gender
    ins_type = input$ins_type
    cc = input$cc
    driver_record = input$driver_record
    credit_scores = input$credit_scores
    model = input$model
    year = input$Year
    generate_cur_data(state, age, gender, ins_type, cc, driver_record, credit_scores, model, year)
  })
  
  # Used for Debug
  output$text <- renderText({
    age = input$age
    state = input$states
    gender = input$gender
    ins_type = input$ins_type
    cc = input$cc
    driver_record = input$driver_record
    credit_scores = input$credit_scores
    model = input$model
    year = input$Year
    cur_data = data()
    if (length(cur_data) == 1){
      return("We don't have enough information in this setting, please try another settings.")
    }
    paste("What you have chosen: ",age,state,gender,ins_type, cc, driver_record, credit_scores,model, year)
  })
  
  output$plot <- renderCollapsibleTree({
    cur_data = data()
    if (length(cur_data) == 1){
      return(NULL)
    }
    collapsibleTree(
      cur_data,
      hierarchy = c("CompanyOverall", "Company_names", "MinMaxAvg"),
      root = c("Price"),
      attribute = c("Price"),
      aggFun = mean,
      inputId = "node"
    )
  })
  
  # Used for Debug
  output$text2 <- renderPrint({
    if (length(l) == 1){
      l[[2]] <- c(-1)
    }
    l[[1]] <<- l[[2]]
    l[[2]] <<- input$node$Company_names
    str(selected)
  })
  
  output$plot2 <- renderPlot({
    cur_data = data()
    if (length(cur_data) == 1){
      return(NULL)
    }
    
    age = input$age
    state = input$states
    gender = input$gender
    ins_type = input$ins_type
    cc = input$cc
    driver_record = input$driver_record
    credit_scores = input$credit_scores
    model = input$model
    year = input$Year
    
    if (last_input != paste(age,state,gender,ins_type, cc, driver_record, credit_scores,model, year)){
      last_input <<- paste(age,state,gender,ins_type, cc, driver_record, credit_scores,model, year)
      selected <<- c("Overall")
    }
    
    val = input$node$Company_names
    if (!is.null(val) && val != "Overall"){
      if (val %in% selected){
        selected <<- selected[-which(selected == val)]
      }else{
        selected <<- c(selected,val)
      }
    }
    cur_data <- select_data(cur_data, selected)
    plot_bar(cur_data)
  })
}
