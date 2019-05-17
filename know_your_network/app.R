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

srn<- st_read("C:/Users/fooli/OneDrive/Documents/R/know_your_network/Outputs/birmingham_srn.shp")
#srn <- st_read("./Data/network.shp")

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "blue",
                    
                    dashboardHeader(title = "Know your network!"),
                    
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
                      )
                                       ), # end of dashboard sidebar
                    
                    dashboardBody(
                     
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
   
 }

# Run the application 
shinyApp(ui = ui, server = server)

