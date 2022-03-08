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
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(ggmap)
library(maps)
library(mapdata)
library(ggthemes)
library(sp)
library(stringr)
library(plyr)
library(dplyr)
library(DT)

require(scales)
options(scipen=10000)

temp = list.files(pattern="*1.csv")
allData2 <- lapply(temp, read.csv)
allData3 <- do.call(rbind, allData2)
temp2 = list.files(pattern="*CTALL.csv")
allDataLL <- lapply(temp2, read.csv)
allDataLL3 <- do.call(rbind, allDataLL)
allDataLL3Unique <- distinct(allDataLL3,MAP_ID, .keep_all= TRUE)
newtable <- merge(allData3,allDataLL3Unique, by.x  = "station_id", by.y="MAP_ID") 
newtable$lubridateDate <- mdy(newtable$date)
newTableSortedRides <- newtable[order(newtable$rides),]  #sort by rides
augustEntries <- subset(newtable, lubridateDate == "2021-08-23")
sortedAug <- augustEntries[order(augustEntries$stationname),]

x = str_split(sortedAug$Location[1], ",", n = 2)
sortedAug[c('First', 'Last')] <- str_split_fixed(sortedAug$Location, ', ', 2)
sortedAug$Lat <- as.numeric(gsub('[(]','', sortedAug$First))
sortedAug$Lon <- as.numeric(gsub('[)]','', sortedAug$Last))

#print(gsub('[(]','', sortedAug$First))
#print(sortedAug)


cities <- read.csv(textConnection("
City,Lat,Long,Pop
Boston,42.3601,-71.0589,645966
Hartford,41.7627,-72.6743,125017
New York City,40.7127,-74.0059,8406000
Philadelphia,39.9500,-75.1667,1553000
Pittsburgh,40.4397,-79.9764,305841
Providence,41.8236,-71.4222,177994
"))

# Define UI for application that draws a histogram
# ui <-
  # fluidPage(
  # 
  #   # Application title
  #   titlePanel("Old Faithful Geyser Data"),
  # 
  #   # Sidebar with a slider input for number of bins 
  #   sidebarLayout(
  #       sidebarPanel(
  #           sliderInput("bins",
  #                       "Number of bins:",
  #                       min = 1,
  #                       max = 50,
  #                       value = 30)
  #       ),
  # 
  #       # Show a plot of the generated distribution
  #       mainPanel(
  #          plotOutput("distPlot"),
  #          leafletOutput("mymap")
  #       )
  #   )
    
    
    ui <- dashboardPage(
      dashboardHeader(),
      dashboardSidebar(
        sidebarMenu(
          menuItem("", tabName = "cheapBlankSpace", icon = NULL),
          menuItem("", tabName = "cheapBlankSpace", icon = NULL),
          menuItem("", tabName = "cheapBlankSpace", icon = NULL),
          menuItem("", tabName = "cheapBlankSpace", icon = NULL),
          menuItem("", tabName = "cheapBlankSpace", icon = NULL),
          menuItem("", tabName = "cheapBlankSpace", icon = NULL),
          menuItem("", tabName = "cheapBlankSpace", icon = NULL),
          menuItem("", tabName = "cheapBlankSpace", icon = NULL),
          menuItem("", tabName = "cheapBlankSpace", icon = NULL),
          menuItem("", tabName = "cheapBlankSpace", icon = NULL),
          menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
        dateInput('date',
                  label = 'Date input: yyyy-mm-dd',
                  value = "2021-08-23"
        ),
        selectInput("mapTheme", h3("Map Theme"), 
                    choices = list("Default" = 1,
                                   "Dark" = 2,
                                   "GeoWorld" = 3), selected = 1),
        # actionButton("reset", "Reset Map"),
        actionButton("reset_button", "Reset Map"),
        selectInput("alphabetmaxmin", h3("Order of Display"), 
                    choices = list("Alphabetical" = 1,
                                   "Min-Max" = 2), selected = 1),
        selectInput("barChartTableMain", h3("Map Theme"), 
                    choices = list("Barchart" = 1,
                                   "Table" = 2), selected = 1)
      ),
      dashboardBody(
        mainPanel(
          verbatimTextOutput("dateText"),
          # conditionalPanel(
          #   condition = "input.barChartTableMain == '1'",
          #   plotOutput("distPlot")
          # )
          # , conditionalPanel(
          #   condition = "input.barChartTableMain == '2'",
          #   DTOutput("tbBarchart")
          # ),
           plotOutput("distPlot"),
          leafletOutput("mymap", height=500, width=300),
          DTOutput("tbBarchart")
        )
      )
    )
    
    

# Define server logic required to draw a histogram
server <- function(input, output) {

  justReactiveDateSelection <- reactive({
    defaultDate = input$date
    # print(nameOfPlace)
     subset(newtable, newtable$lubridateDate == defaultDate)
  })
  
  # justReactiveDateSelectionSortedOrder <- reactive({
  #   defaultDate = input$date
  #   # print(nameOfPlace)
  #   subset(newTableSortedRides, newTableSortedRides$lubridateDate == defaultDate)
  # })  
  

    output$distPlot <- renderPlot({
      bar1 <- justReactiveDateSelection()
      if(input$alphabetmaxmin == 1) {
        
        # bar1 <- bar1[order(bar1$stationname),]
        ggplot(bar1, aes(x=stationname, y=rides))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Stations", title=paste("Entries in CTA for ", input$date))+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      } else{
        # bar1 <- justReactiveDateSelectionSortedOrder()
        # bar1 <- bar1[order(bar1$rides),]
        # print(bar1)
        ggplot(bar1, aes(x=reorder(stationname, rides), y=rides))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Stations", title=paste("Entries in CTA for ", input$date))+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      }
        # generate bins based on input$bins from ui.R
      
    })
    
    # output$mymap <- renderLeaflet({
    #   leaflet(sortedAug) %>% addTiles() %>%
    #     # addCircles(lng = ~Lon, lat = ~Lat, weight = 1,
    #     #            radius = ~sqrt(rides) * 30, 
    #     #            popup = ~stationname
    #     # )
    #     addMarkers(~Lon, ~Lat, popup = ~as.character(rides),label = ~as.character(stationname)) %>%
    #      addProviderTiles(providers$Stamen.Toner)
    # })
    
    output$tbBarchart = renderDT({
      bar1 <- justReactiveDateSelection()
      dfbar <- data.frame(
        stationname = bar1$stationname,
        rides = bar1$rides
      )
      #print(dfbar)
      dfbar <- dfbar[order(dfbar$stationname),]
      datatable(dfbar,options  = list(lengthMenu = c(7,7)), rownames= FALSE)
    })
    
    
    output$mymap <- renderLeaflet({
      sortedReactive <- justReactiveDateSelection()
      sortedReactive <- sortedReactive[order(sortedReactive$stationname),]
      x = str_split(sortedReactive$Location[1], ",", n = 2)
      sortedReactive[c('First', 'Last')] <- str_split_fixed(sortedReactive$Location, ', ', 2)
      sortedReactive$Lat <- as.numeric(gsub('[(]','', sortedReactive$First))
      sortedReactive$Lon <- as.numeric(gsub('[)]','', sortedReactive$Last))
      #newYears <-  justOneYearReactive()
      if(input$mapTheme == 1) {
        leaflet(sortedReactive) %>% addTiles() %>%
          # addCircles(lng = ~Lon, lat = ~Lat, weight = 1,
          #            radius = ~sqrt(rides) * 30, 
          #            popup = ~stationname
          # )
          addMarkers(~Lon, ~Lat, popup = ~as.character(paste(stationname, ": ", rides)),label = ~as.character(stationname))
        # ggplot(df1, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title="Entries in UIC-Halsted from 2001-2021")+scale_y_continuous(labels=comma)
      } else if(input$mapTheme == 2) {
        leaflet(sortedReactive) %>% addTiles() %>%
          # addCircles(lng = ~Lon, lat = ~Lat, weight = 1,
          #            radius = ~sqrt(rides) * 30, 
          #            popup = ~stationname
          # )
          addMarkers(~Lon, ~Lat, popup = ~as.character(paste(stationname, ": ", rides)),label = ~as.character(stationname)) %>%
          addProviderTiles(providers$Stamen.Toner)
      } else {
        leaflet(sortedReactive) %>% addTiles() %>%
          # addCircles(lng = ~Lon, lat = ~Lat, weight = 1,
          #            radius = ~sqrt(rides) * 30, 
          #            popup = ~stationname
          # )
          addMarkers(~Lon, ~Lat, popup = ~as.character(paste(stationname, ": ", rides)),label = ~as.character(stationname)) %>%
          addProviderTiles(providers$Esri.NatGeoWorldMap)
      }
    })
    
    observe({
      input$reset_button
      # sortedReactive <- justReactiveDateSelection()
      # sortedReactive <- sortedReactive[order(sortedReactive$stationname),]
      # x = str_split(sortedReactive$Location[1], ",", n = 2)
      # sortedReactive[c('First', 'Last')] <- str_split_fixed(sortedReactive$Location, ', ', 2)
      # sortedReactive$Lat <- as.numeric(gsub('[(]','', sortedReactive$First))
      # sortedReactive$Lon <- as.numeric(gsub('[)]','', sortedReactive$Last))
      # # leafletProxy("mymap")
      # leaflet(sortedReactive) %>% addTiles() %>%
      #   # addCircles(lng = ~Lon, lat = ~Lat, weight = 1,
      #   #            radius = ~sqrt(rides) * 30, 
      #   #            popup = ~stationname
      #   # )
      #   addMarkers(~Lon, ~Lat, popup = ~as.character(paste(stationname, ": ", rides)),label = ~as.character(stationname)) %>%
      #   addProviderTiles(providers$Esri.NatGeoWorldMap)
      output$mymap <- renderLeaflet({
        sortedReactive <- justReactiveDateSelection()
        sortedReactive <- sortedReactive[order(sortedReactive$stationname),]
        x = str_split(sortedReactive$Location[1], ",", n = 2)
        sortedReactive[c('First', 'Last')] <- str_split_fixed(sortedReactive$Location, ', ', 2)
        sortedReactive$Lat <- as.numeric(gsub('[(]','', sortedReactive$First))
        sortedReactive$Lon <- as.numeric(gsub('[)]','', sortedReactive$Last))
        #newYears <-  justOneYearReactive()
          if(input$mapTheme == 1) {
            leaflet(sortedReactive) %>% addTiles() %>%
              # addCircles(lng = ~Lon, lat = ~Lat, weight = 1,
              #            radius = ~sqrt(rides) * 30, 
              #            popup = ~stationname
              # )
              addMarkers(~Lon, ~Lat, popup = ~as.character(paste(stationname, ": ", rides)),label = ~as.character(stationname))
            # ggplot(df1, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title="Entries in UIC-Halsted from 2001-2021")+scale_y_continuous(labels=comma)
          } else if(input$mapTheme == 2) {
            leaflet(sortedReactive) %>% addTiles() %>%
              # addCircles(lng = ~Lon, lat = ~Lat, weight = 1,
              #            radius = ~sqrt(rides) * 30, 
              #            popup = ~stationname
              # )
              addMarkers(~Lon, ~Lat, popup = ~as.character(paste(stationname, ": ", rides)),label = ~as.character(stationname)) %>%
              addProviderTiles(providers$Stamen.Toner)
          } else {
            leaflet(sortedReactive) %>% addTiles() %>%
              # addCircles(lng = ~Lon, lat = ~Lat, weight = 1,
              #            radius = ~sqrt(rides) * 30, 
              #            popup = ~stationname
              # )
              addMarkers(~Lon, ~Lat, popup = ~as.character(paste(stationname, ": ", rides)),label = ~as.character(stationname)) %>%
              addProviderTiles(providers$Esri.NatGeoWorldMap)
          }
      })
      
    })

    
    output$dateText  <- renderText({
      paste("Date is", as.character(input$date), "and is a", weekdays(input$date))
    })
    
    # 
    # observe({
    #   click1 <- input$mymap_marker_click
    #   if(is.null(click1))
    #     return()
    #   print(click1)
      # dataTableProxy("table01") %>%
      #   selectRows(which(qSub()$id == clickId)) %>%
      #   selectPage(which(input$table01_rows_all == clickId) %/% input$table01_state$length + 1)
    # })
    
    # observeEvent(input$reset,{
    #   renderLeaflet(leaflet() %>% addTiles() %>% setView(41.8781, 87.6298, zoom = 17))
    # })
    
    # output$myMap = renderLeaflet(leaflet() %>% addTiles() %>% setView(41.8781, 87.6298, zoom = 17))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
