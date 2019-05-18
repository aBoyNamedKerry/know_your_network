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


srn <- st_read("../Outputs/birmingham_srn_wider.shp")
events <- read_csv("../Data/events_next_week_birmingham.csv")
source("api_call.R")


# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "blue",
                    
                    dashboardHeader(title = "Know your network!"),
                    
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Events", tabName = "events", icon = icon("dashboard"))
                      )
                                       ), # end of dashboard sidebar
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "events",
                        fluidPage(
                                  # Application title
                                  titlePanel("Event planner"),
                                  # Sidebar with a selectInput 
                                  sidebarLayout(
                                    sidebarPanel(
                                       h3("Choose event date range"),
                                      #data range in
                                      dateRangeInput(inputId = "date_range", label = "Date Range", 
                                                     start = min(events$startDate), 
                                                     end = max(events$startDate)),
                                      
                                      selectInput(inputId = "segment", label = "Select segment", 
                                                  choices =  srn$SECT_LABEL, 
                                                  selected =  srn$SECT_LABEL[1]
                                                      ),
                                      selectInput(inputId = "hour", label = "Select hour",
                                                  choices = c(0,1:23), selected = 12)
                                   
                                    ), # end of sidebarPanel
                                    
                                    # Show a plot of the map
                                    mainPanel(
                                     # splitLayout(cellWidths = c("60%", "40%"),

                                      box(leafletOutput("map"), width = 12, height = "420px"), 
                                      
                                      
                                      box(column(dataTableOutput("events_table"),
                                                 width = 12),
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
   
  #create reactive events object
  events_react<- reactive({
    
    df<- events %>% filter(startDate>= input$date_range[1],
                           startDate<= input$date_range[2])


  
    # df<- get_events(date_from = input$date_range[1],
    #                 date_to = input$date_range[2])
    # df %>%
    #   dplyr::select(headline, startDate, venue.id, venue.location.lat,
    #          venue.location.lon) %>% filter(!is.na(venue.location.lon))
    # df
    # 
  })
    
  output$map <- renderLeaflet({

   # generate bins based on input$bins from ui.R
   srn_pop <- paste0("Road Number: ",
                     srn$ROA_NUMBER,
                     "<br>",
                     "Section: ",
                     srn$SECT_LABEL,
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
      addMarkers(lng=events_react()$venue.location.lon, lat=events_react()$venue.location.lat,
                 popup=paste0(events_react()$headline, "<br>", events_react()$startDate))
   })

   output$events_table<- renderDataTable ({

     events_react() %>% select(headline, startDate, venue.id) %>%
     datatable()
   })
 }

# Run the application 
shinyApp(ui = ui, server = server)
