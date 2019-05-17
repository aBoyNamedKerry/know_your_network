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


srn <- st_read("../Outputs/birmingham_srn.shp")
events <- read.csv("../Data/events_next_week_birmingham.csv")
#srn <- st_read("../Data/network.shp")

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "blue",
                    
                    dashboardHeader(title = "Know your network!"),
                    
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
                      )
                                       ), # end of dashboard sidebar
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "dashboard",
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
                                                  choices =  srn$SECT_LABEL, 
                                                  selected =  srn$SECT_LABEL[1]
                                                      )
                                   
                                    ), # end of sidebarPanel
                                    
                                    # Show a plot of the map
                                    mainPanel(
                                     # splitLayout(cellWidths = c("60%", "40%"),

                                      box(leafletOutput("map"), width = 12, height = "450px"),
                                      
                                      box(dataTableOutput("events_table"),
                                          width = 12)

                                      #)#split layout end
                                    )# end main panel
                                  )# end side panel
                                )# end fluid page
                        ) #end of tabitem 
                      ) # end of tabitems
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
   m <- leaflet(srn) %>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addPolylines(stroke = TRUE, fillOpacity = 0, weight = 1,
                  color = ~srn_col(srn$Jan_01),
                  popup = ~srn_pop)
   m %>%
      addMarkers(lng=events$venue.location.lon, lat=events$venue.location.lat, popup=events$headline)
   })

   output$events_table<- renderDataTable ({

     srn %>% select(ROA_NUMBER,SECT_LABEL, LOCATION) %>%
     datatable()
   })
 }

# Run the application 
shinyApp(ui = ui, server = server)
