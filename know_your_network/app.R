library(shiny)
library(shinydashboard)
library(leaflet)
library(readxl)
library(sf)
library(tidyverse)
library(magrittr)
library(janitor)
library(DT) 
library(png)


srn <- st_read("../Outputs/birmingham_srn_wider.shp") 

srn%<>%
  mutate(Jan_01 = 
           sample(x = c("Y", "N"), size = nrow(srn), 
                  replace = TRUE, prob = c(0.95, 0.05)))
#logo <- readPNG('../Data/kyn.png')
#srn <- st_read("./Data/network.shp")
#events <- read_csv("../Data/events_next_week_birmingham.csv")

source("api_call.R")
#traffic_A38M <- read.csv('../Data/A38(M)_traffic.csv', skip = 3)
traffic_A5 <- read.csv('../Data/A5_traffic.csv', skip = 3)
traffic_M6 <- read.csv('../Data/M6_traffic.csv', skip = 3)
planned_works <- read.csv('../Data/planned_works.csv', stringAsFactors = FALSE)
colnames(planned_works) <- c('startDate', 'startTime', 'endDate', 'endTime', 'workType', 'description')
planned_works$startDate <- as.Date(as.character(planned_works$startDate))
planned_works$endDate <- as.Date(as.character(planned_works$endDate))

# Define UI for application that draws a histogram
DBheader <- dashboardHeader(title = "Know your network!")

ui <- dashboardPage(skin = "blue",
                    DBheader,
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("Events Planner", tabName = "events", icon = icon("dashboard")),
                            menuItem('Analysis', tabName = 'analysis', icon = icon('bar-chart-o')), 
                            menuItem('Conflict Management', tabName = 'evaluation', icon = icon('calendar'))
                        )
                    ), # end of dashboard sidebar
                    dashboardBody(
                      #add CSS
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
                        tabItems(
                            tabItem(tabName = "events",
                            fluidPage(
                                # Application title
                                titlePanel("Event Planner"),
                                # Sidebar with a selectInput 
                                sidebarLayout(
                                    sidebarPanel(
                                        h3("Choose event date range"),
                                        #data range in
                                        dateRangeInput(inputId = "date_range",
                                                       label = "Date Range",
                                                       start = Sys.Date(),
                                                       end = Sys.Date() + 3,
                                                       min = Sys.Date()
                                                       ),
                                        selectInput(inputId = 'area',
                                                    label = 'Select area',
                                                    c('Birmingham', 'Manchester', 'Portsmouth')
                                                    ),
                                        selectInput(inputId = "segment", label = "Select segment", 
                                                    choices =  srn$SECT_LABEL, 
                                                    selected =  srn$SECT_LABEL[1]
                                                        ),
                                        selectInput(inputId = "s_hour", label = "Select Start Hour",
                                                    choices = c(0,1:23), selected = 12),
                                        selectInput(inputId = "e_hour", label = "Select End Hour",
                                                    choices = c(0,1:23), selected = 14),
                                        textInput(inputId = 'work_type', label = 'Work Type', value = 'minor maintenance', width = '100%', placeholder = 'Type of the Road Work.'),
                                        actionButton(inputId = 'book', label = 'Book the Road Work Schedule')
                                    ), # end of sidebarPanel

                                    # Show a plot of the map
                                    mainPanel(
                                        box(leafletOutput("map"), width = 12, height = "420px"), 
                                        box(column(dataTableOutput("events_table"),
                                                   width = 12),
                                            width = 12)
                                    )# end main panel
                                )# end side panel
                            )# end fluid page
                        ), #end of tabitem
                        
                        tabItem(tabName = 'analysis',
                            fluidPage(
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
                                                                'Sunday' = 6),
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
                        ), # end of tabItem
                        
                        tabItem(tabName = 'evaluation',
                            fluidPage(
                              # tab/page title
                              titlePanel('Conflict Management'),
                              sidebarLayout(
                                  sidebarPanel(
                                      h3('Select the Date.'),
                                      # data input
                                      dateInput(inputId = "cal_date", label = "Date Range", 
                                                min = Sys.Date(),
                                                format = 'dd-mm-yyyy')
                                  ),
                                  mainPanel(
                                      box(column(dataTableOutput('calendar'), width = 12), 
                                          width = 12)
                                  ) # end of sidebarPanel
                                ) # end of sidebarLayout
                            ) # end of fluidPage
                        ) # end of tabItem
                    ) # end of tabitems
                ) # dashboard body
) # End of dashboard page


# Define server logic required to draw a histogram
server <- function(input, output) {

  #create reactive events object
  events_react <- reactive({
    df <- get_events(location = input$area,
                     date_from = input$date_range[1],
                     date_to = input$date_range[2])
    df
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
        events_react() %>% select(headline, title, startDate, endDate, venue.name, venue.address.streetAddress) %>%
        datatable()
    })

    observeEvent(input$book, {
        start_date <- as.character(input$date_range[1])
        end_date <- as.character(input$date_range[2])
        start_time <- paste(as.character(input$s_hour), ':00', sep = '')
        end_time <- paste(as.character(input$e_hour), ':00', sep = '')
        work_type <- input$work_type
        desc <- as.character(input$segment)
        road_work_info <- c(start_date, start_time, end_date, end_time, work_type, desc)
        planned_works <- rbind(planned_works, road_work_info)
        write.csv(planned_works, file = '../Data/planned_works.csv', row.names = FALSE)
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

    output$calendar <- renderDataTable({
        events_on_the_day <- events %>% filter(startDate <= input$cal_date & endDate >= input$cal_date) %>%
            select(c(startDate, startTime = 'startTimeString', endDate, endTime = 'endTimeString', title, description)) %>%
            as.data.frame()
        works_on_the_day <- planned_works %>% filter(startDate <= input$cal_date & endDate >= input$cal_date) %>% 
            select(c(startDate, startTime, endDate, endTime, title = 'workType', description)) %>%
            as.data.frame()
        all_on_the_day <- as.data.frame(rbind(events_on_the_day, works_on_the_day))
        # colnames(all_on_the_day) <- c('Start Date', 'Start Time', 'End Date', 'End Time', 'Event Title/Work Type', 'Description')
    })

}


# Run the application 
shinyApp(ui = ui, server = server)

