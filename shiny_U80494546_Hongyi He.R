library(tidyverse)
library(zoo)
library(shiny)
library(shinydashboard)
library(rsconnect)

#load the data
data <- read.csv("data.csv")
data <- as.tibble(data)
data_h <- data
data$Time <- as.Date(data$Time)

#load avg_data
data_avg <- read.csv("data_avg.csv")
data_avg <- as.tibble(data_avg)

#load veg data
data_veg <- read.csv("veg_data3.csv")
data_veg <- as.tibble(data_veg)
data_veg <- data_veg %>% filter(label == "RESTRICTED USE CHEMICAL", Unit.of.Measurement==" MEASURED IN LB")


# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(    
    sidebarMenu(
      menuItem("Temperature", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Vegetable", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = "Select the range of year",
                  status="success",
                  solidHeader = TRUE,
                  sliderInput("max", 
                              label = "Range of year:",
                              min = 1987, max = 2017, value = c(2016,2017)),
                  hr(),
                  radioButtons("Type", "Type of temperature:",
                               c("Air Temperature" = 'ATMP',
                                 "Water Temperature" = 'WTMP')),
                  hr(),
                  helpText("Data from the NOAA.")
                ),
                
                tabBox(
                  title = "Berling Sea Temperature",
                  
                  id = "tabset1",
                  
                  side = "left", 
                  
                  height = "250px",
                  
                  selected = "Daily air temperature",
                  
                  tabPanel("Daily",
                           status="success",
                           plotOutput("graph")),
                  
                  tabPanel("Yearly",
                           status="success",
                           plotOutput("graph2")),
                  
                  tabPanel("Regression line",
                           status="success",
                           plotOutput("graph3")
                  )
                )
              )
      ),
      
      tabItem(tabName = "widgets",
              fluidRow(
                box(
                  selectInput("a", "Choose a commodity:",choices = c(2006,2010,2014,2016)),
                  selectInput("b", "Choose a commodity:",choices = c("BROCCOLI","CAULIFLOWER"))
               ),
                box(
                  title = "Chemcials of vegetable compared with their toxicity",
                  status= "success",
                  solidHeader = TRUE,
                  plotOutput("graph4"),              
                  hr(),
                  helpText("Data from EPA"),
                  verbatimTextOutput("dateText")
                 )
              )
            )
      )
    )
  )

server <- function(input, output) {
  
  a <- reactive({
    filter(data, YYYY>input$max[1]) %>% filter(YYYY<input$max[2]) %>% filter(hh==12)
  })
  
  b <- reactive({
    filter(data_avg, YYYY>input$max[1]) %>% filter(YYYY<input$max[2]) 
  })
  
  c <- reactive({
    filter(data_h, YYYY>input$max[1]) %>% filter(YYYY<input$max[2])
  })
  
  d <- reactive({
    filter(data_veg, Year==input$a) %>% filter(Commodity == input$b)
  })
  
  output$graph <- renderPlot({
    if(input$Type=="ATMP"){
      ggplot(a(), aes(x = Time)) + 
        geom_line(aes(y = ATMP), colour="green", size = 0.5) + 
        ylab(label="Celsius degrees") + 
        xlab("Time")
    }else if(input$Type=="WTMP"){
      ggplot(a(), aes(x = Time)) + 
        geom_line(aes(y = WTMP), colour="green", size = 0.5) + 
        ylab(label="Celsius degrees") + 
        xlab("Time")
    }
  })
  
  output$graph2 <- renderPlot({
    if(input$Type=="ATMP"){
      ggplot(b(), aes(x = YYYY)) + 
        geom_smooth(aes(y = avg_atmp), colour="blue", size = 0.5) + 
        ylab(label="Celsius degrees") + 
        xlab("Time")
    }else if(input$Type=="WTMP"){
      ggplot(b(), aes(x = YYYY)) + 
        geom_smooth(aes(y = avg_wtmp), colour="blue", size = 0.5) +
        ylab(label="Celsius degrees") + 
        xlab("Time")
    }
  })
  
  output$graph3 <- renderPlot({
    if(input$Type=="ATMP"){
      ggplot(a(),aes(x=Time,y=ATMP))+
        geom_point()+geom_smooth(method="lm")+
        ggtitle("reg ATMP")
    }else if(input$Type=="WTMP"){
      ggplot(a(),aes(x=Time,y=WTMP))+
        geom_point()+
        geom_smooth(method="lm")+
        ggtitle("reg WTMP")
    }
  })
  
  output$graph4 <- renderPlot({
    ggplot(d(), mapping=aes(x= Name, y=Value )) + 
      geom_bar(stat="identity", position="dodge",aes(fill=Name)) + 
      coord_flip()+
      labs(y = "Values(LB) ",x = "Chemical Name")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

