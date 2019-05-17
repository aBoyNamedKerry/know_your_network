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

srn<- st_read("./Outputs/birmingham_srn.shp")

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "blue",
                    
                    dashboardHeader(title = "Know your network!"),
                    
                    dashboardSidebar(
                      sidebarMenu(id="tabs",
                                  sidebarMenuOutput("menu") # we use this for the server to render the dashboard tabs there - we can select by default that way
                      )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        
                        tabItem(tabName = "dashboard", selected = TRUE,
                                
                                fluidPage(
                                  
                                  # Application title
                                  titlePanel("Business data by local authority"),
                                  
                                  
                                  # Sidebar with a selectInput 
                                  sidebarLayout(
                                    sidebarPanel(
                                      
                                      h3("Choose event date range"),
                                      #data range input
                                      dateRangeInput(inputId = "data_range", label = "Date Range", 
                                                     start = "2018-06-01", end = "2018-08-31")
                                      
                                      
                                   
                                    ), # end of sidebarPanel
                                    
                                    # Show a plot of the map
                                    mainPanel(
                                      splitLayout(cellWidths = c("60%", "40%"),
                                                  
                                      leafletOutput("map"),
                                      
                                      dataTableOutput("events_table")
                                      )#split layout end
                                    )# end main panel
                                  )# end side panel
                                )# end fluid page
                        ),#end of tabItem1
                        
                        tabItem(tabName = "enterpriseSize",
                                
                                fluidPage(
                                  
                                  h4("To be updated")
                                  
                                )# end of fluid page tabItem 2
                                
                        )#end of tabItem 2
                      )# end of tabItems
                    )# dashboard body
)# End of dashboard page

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

