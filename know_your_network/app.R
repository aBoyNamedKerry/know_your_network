#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(readxl)
library(sf)
library(tidyverse)
library(magrittr)
library(janitor)
library(DT)

srn<- st_read("../Outputs/birmingham_srn.shp")
#srn <- st_read("./Data/network.shp")
#traffic_A38M <- read.csv('../Data/A38(M)_traffic.csv', skip = 3)
traffic_A5 <- read.csv('../Data/A5_traffic.csv', skip = 3)
traffic_M6 <- read.csv('../Data/M6_traffic.csv', skip = 3)

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "blue",
                    
  dashboardHeader(title = "Know your network!"),
  
  dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")), 
    menuItem('Analysis', tabName = 'analysis', icon = icon('bar-chart-o'))
  )
                   ), # end of dashboard sidebar
  
  dashboardBody(
  
    tabItems(
      
      tabItem(tabName = 'dashboard', 
              
        fluidPage(
              
              # Application title
              titlePanel("Event planner"),
              
              
              # Sidebar with a selectInput 
              sidebarLayout(
                
                sidebarPanel(
                   h3("Choose event date range"),
                  
                  #data range input
                  dateRangeInput(inputId = "data_range", label = "Date Range", 
                                 start = "2018-06-01", end = "2018-08-31"),
                  
                  selectInput(inputId = "segment", label = "Select segment", 
                              choices =  srn$ROA_NUMBER
                                  )
               
                ), # end of sidebarPanel
                
                # Show a plot of the map
                mainPanel(
                 # splitLayout(cellWidths = c("60%", "40%"),
                              
                  leafletOutput("map"),
                  
                  dataTableOutput("events_table")
                  #)#split layout end
                
                )# end main panel
              
              )# end side panel
            
            )# end fluid page
      
          ),# end tabItem
    
      tabItem(tabName = 'analysis', 
               
        fluidRow(
        
          # tab/page title
          titlePanel('Analysis'),
          
          sidebarLayout(
            
            sidebarPanel(
              
              h3('Select day of the week. '),
              
              # data input
              selectInput(inputId = 'segment2', label = 'Select Segment', 
                          choices = c('A5', 'M6')),
              
              selectInput(inputId = 'weekday', label = 'Weekday', 
                          choices = c('Monday' = 0, 'Tuesday' = 1, 'Wednesday' = 2, 
                                      'Thursday' = 3, 'Friday' = 4, 'Saturday' = 5, 
                                      'Saunday' = 6), 
                          selected = 0, 
                          multiple = TRUE),
              
              sliderInput(inputId = 'time', label = 'Time', 
                          min = 0, max = 24, value = c(9, 11), post = ':00')
              
            ), 
            
            mainPanel(
              
              plotOutput('traffic_flow')
              
            ) # end of sidebarPanel
            
          ) # end of sidebarLayout
          
        ) # end of fluidPage
        
      ) # end of tabItem
      
    ) # end tabItems
  
  )# dashboard body
                    
)# End of dashboard page

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$map <- renderLeaflet({
      # generate bins based on input$bins from ui.R
     srn_pop <- paste0("Road Number: ",
                       srn$ROA_NUMBER,
                       "<br>",
                       "Location: ",
                       srn$LOCATION)
     
     srn_col<- colorFactor(c("red", "green"), as.factor(srn$Jan_01))
      
                                      leaflet(srn) %>%
                                        addProviderTiles(providers$CartoDB.Positron)%>%
                                        addPolygons(stroke = TRUE, fillOpacity = 0, weight = 1,
                                                    color = ~srn_col(srn$Jan_01),
                                                    popup = ~srn_pop) 
                                      
                                      
   })
   
   output$events_table<- renderDataTable ({
     
     srn %>%
     datatable()
     
   })
   
   output$traffic_flow <- renderPlot({
     # select segment
     if (input$segment2 == 'A5'){
       data = traffic_A5
     } else {
       data = traffic_M6
     }
     
     # obtaining days
     id = input$weekday
     
     # plot barchart of number of vehicles every 15 minutes
     selected_day <- data[data$Day.Type.ID %in% id, ]
     aggr <- aggregate(selected_day$Total.Carriageway.Flow, list(selected_day$Local.Time), mean)
     colnames(aggr) <- c('time_of_day', 'traffic_flow')
     attach(aggr)
     barplot(traffic_flow, width = .25, space = 0, names.arg = time_of_day, xlim = c(0, 24))
     
     start_time = input$time[1]
     end_time = input$time[2]
     abline(v = start_time, col = 'blue', lwd = 2)
     abline(v = end_time, col = 'blue', lwd = 2)
   })
   
 }

# Run the application 
shinyApp(ui = ui, server = server)

