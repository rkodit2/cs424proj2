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
#newtable$lubridateDate <- mdy(newtable$date)
#print(year(newtable$lubdridateDate))
#newtable$year <- year(newtable$lubdridateDate)
lubridateDate <- mdy(newtable$date)
newtable$lubridateDate <- lubridateDate
newtable$month <- month(lubridateDate)
newtable$day <- day(lubridateDate)
newtable$year <- year(lubridateDate)
newtable$weekday <- weekdays(newtable$lubridateDate)

newTableSortedRides <- newtable[order(newtable$rides),]  #sort by rides
augustEntries <- subset(newtable, lubridateDate == "2021-08-23")
sortedAug <- augustEntries[order(augustEntries$stationname),]

uniqueStationNames <- sort(unique(newtable$stationname))
#print(head(uniqueStationNames))

x = str_split(sortedAug$Location[1], ",", n = 2)
sortedAug[c('First', 'Last')] <- str_split_fixed(sortedAug$Location, ', ', 2)
sortedAug$Lat <- as.numeric(gsub('[(]','', sortedAug$First))
sortedAug$Lon <- as.numeric(gsub('[)]','', sortedAug$Last))

counter <- reactiveValues(countervalue = 0, counterdate = as.Date("2021-08-23"), counterfinalday = as.Date("2021-08-23"), counterprevbuttonpressed = -1)

months <- month.abb
months_no <- c(1:12)
yearList = c(2001:2021)

weekday_list <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

pages <- c("Home","StationData","About Page")

stationClicked = "Austin-Forest Park"


selectInput("page1", "Select the page", pages, selected = "Home")

nameOfPlace1 = "UIC-Halsted"
nameOfPlace2 = "UIC-Halsted"




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
          menuItem("", tabName = "cheapBlankSpace", icon = NULL),
          menuItem("", tabName = "cheapBlankSpace", icon = NULL),
          menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
        # selectInput("mapTheme", h3("Map Theme"), 
        #             choices = list("Default" = 1,
        #                            "Dark" = 2,
        #                            "GeoWorld" = 3), selected = 1),
        # actionButton("reset", "Reset Map"),
        actionButton("reset_button", "Reset Map"),
        selectInput("alphabetmaxmin", h3("Order of Display"), 
                    choices = list("Alphabetical" = 1,
                                   "Min-Max" = 2), selected = 1),
        selectInput("chart1", h3("Bar/Table Theme"), 
                    choices = list("Barchart" = 1,
                                   "Table" = 2), selected = 1),
        selectInput("page1", "Select the page", pages, selected = "Home"),
        actionButton("prev_button","Previous Day"),
        actionButton("next_button","Next Day"),
        dateInput('date',
                  label = 'Date input: yyyy-mm-dd',
                  value = "2021-08-23"
        )
        
      ),
      dashboardBody(
        conditionalPanel(
          condition = "input.page1 == 'Home'",
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
          # plotOutput("distPlot"),
          #leafletOutput("mymap", height=500, width=300),
         # DTOutput("tbBarchart")
            # plotOutput("hist2")
        fluidRow(
          column(6,
                 fluidRow(
                   plotOutput("distPlot", height=400)
                 ),
                 fluidRow(
                   leafletOutput("mymap", height=400)
                 )),
          column(3,
                 fluidRow(
                   box(title = "Entries from 2001-2021 for Station", solidHeader = TRUE, status = "primary", width = 12,
                       conditionalPanel(
                         condition = "input.chart1 == '1'",
                         plotOutput("hist1", height=400)
                       )
                       , conditionalPanel(
                         condition = "input.chart1 == '2'",
                         DTOutput("tb1", height=400)
                       )
                   )
                 ),
                 fluidRow(
                   box(title = "Entries for Months for Station", solidHeader = TRUE, status = "primary", width = 12,
                       conditionalPanel(
                         condition = "input.chart1 == '1'",
                         plotOutput("hist2", height = 400)
                       )
                       , conditionalPanel(
                         condition = "input.chart1 == '2'",
                         DTOutput("tb2", height = 400)
                       )
                   )
                 ),
          ),
          column(3,
                 fluidRow(
                   box(title = "Entries for Weekdays for Station", solidHeader = TRUE, status = "primary", width = 12,
                       conditionalPanel(
                         condition = "input.chart1 == '1'",
                         plotOutput("hist3", height = 400)
                       )
                       , conditionalPanel(
                         condition = "input.chart1 == '2'",
                         DTOutput("tb3", height = 400)
                       )
                   )
                 ),
                 fluidRow(
                   box(title = "Entries throughout an Year for Station", solidHeader = TRUE, status = "primary", width = 12,
                       conditionalPanel(
                         condition = "input.chart1 == '1'",
                         plotOutput("hist4", height = 400)
                       )
                       , conditionalPanel(
                         condition = "input.chart1 == '2'",
                         DTOutput("tb4", height = 400)
                       )
                   )
                 ),
          ),
        ),
        )
      ),
      conditionalPanel(
        condition = "input.page1 == 'About Page'",
        column(12,
               h3("The data is from City of Chicago https://www.evl.uic.edu/aej/424/22Sproject1.html#:~:text=https%3A//data.cityofchicago.org/Transportation/CTA%2DRidership%2DL%2DStation%2DEntries%2DDaily%2DTotals/5neh%2D572f
                and the app is made by Soel Mullenkuzhiyil Sunny last updated on 02/12/2022 and was made to compare the CTA entries between O'hare Airport, UIC-Halsted and Dempster stops.")
        )
      )
      ),
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
  
  justReactivePrevDay <- reactive({
    defaultDate = input$date - days(counter$countervalue)
    # print(nameOfPlace)
    subset(newtable, newtable$lubridateDate == defaultDate)
  })
  
  observeEvent(
    input$prev_button,{
      if(counter$counterprevbuttonpressed == 0) {
        counter$countervalue <- 1
        counter$counterdate <- input$date
        counter$counterfinalday <- counter$counterdate - days(counter$countervalue)
      } else {
      counter$countervalue <- counter$countervalue + 1
      counter$counterdate <- input$date
      counter$counterfinalday <- counter$counterdate - days(counter$countervalue)
      }
      # counter$counterdate <- input$date
      counter$counterprevbuttonpressed = 1
      
      

       print(counter$counterdate - days(counter$countervalue))
       print(paste("Input value prec", input$date))
      # output$dateText  <- renderText({
      #   paste("Date is", as.character(input$date - days(counter$countervalue)), "and is a", weekdays(input$date - days(counter$countervalue)))
      # })

    })
  
  observeEvent(
    input$next_button,{
      if(counter$counterprevbuttonpressed == 1) {
        counter$countervalue <- 1
        counter$counterdate <- input$date
        counter$counterfinalday <- counter$counterdate + days(counter$countervalue)
      } else {
      counter$countervalue <- counter$countervalue + 1
      counter$counterfinalday <- counter$counterdate + days(counter$countervalue)
      counter$counterdate <- input$date
      }
      counter$counterprevbuttonpressed == 0

      
      
      
      print(counter$counterdate + days(counter$countervalue))
      print(paste("Input value next", input$date))
      # output$dateText  <- renderText({
      #   paste("Date is", as.character(input$date - days(counter$countervalue)), "and is a", weekdays(input$date - days(counter$countervalue)))
      # })
      
    })
  
  observeEvent(
    input$date,{
      counter$countervalue <- 0
      counter$counterdate <- input$date
      counter$counterfinalday <- input$date
      # print(counter$counterdate - days(counter$countervalue))
      # print(paste("Input value", input$date))
      # output$dateText  <- renderText({
      #   paste("Date is", as.character(input$date - days(counter$countervalue)), "and is a", weekdays(input$date - days(counter$countervalue)))
      # })
      
    })
  

    output$distPlot <- renderPlot({
      bar1 <- subset(newtable, newtable$lubridateDate == counter$counterfinalday)
      if(input$alphabetmaxmin == 1) {
        
        # bar1 <- bar1[order(bar1$stationname),]
        ggplot(bar1, aes(x=stationname, y=rides))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Stations", title=paste("Entries in CTA for ", counter$counterfinalday))+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
      } else{
        # bar1 <- justReactiveDateSelectionSortedOrder()
        # bar1 <- bar1[order(bar1$rides),]
        # print(bar1)
        ggplot(bar1, aes(x=reorder(stationname, rides), y=rides))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Stations", title=paste("Entries in CTA for ", counter$counterfinalday))+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
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
    
    
    # output$mymap <- renderLeaflet({
    #   sortedReactive <- justReactiveDateSelection()
    #   sortedReactive <- sortedReactive[order(sortedReactive$stationname),]
    #   x = str_split(sortedReactive$Location[1], ",", n = 2)
    #   sortedReactive[c('First', 'Last')] <- str_split_fixed(sortedReactive$Location, ', ', 2)
    #   sortedReactive$Lat <- as.numeric(gsub('[(]','', sortedReactive$First))
    #   sortedReactive$Lon <- as.numeric(gsub('[)]','', sortedReactive$Last))
    #   #newYears <-  justOneYearReactive()
    #   if(input$mapTheme == 1) {
    #     leaflet(sortedReactive) %>% addTiles() %>%
    #       setView(lng = 144, lat = -37, zoom = 3) %>%
    #       # addCircles(lng = ~Lon, lat = ~Lat, weight = 1,
    #       #            radius = ~sqrt(rides) * 30, 
    #       #            popup = ~stationname
    #       # )
    #       addMarkers(~Lon, ~Lat, popup = ~as.character(paste(stationname, ": ", rides)),label = ~as.character(stationname))
    #     # ggplot(df1, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title="Entries in UIC-Halsted from 2001-2021")+scale_y_continuous(labels=comma)
    #   } else if(input$mapTheme == 2) {
    #     leaflet(sortedReactive) %>% addTiles() %>%
    #       # addCircles(lng = ~Lon, lat = ~Lat, weight = 1,
    #       #            radius = ~sqrt(rides) * 30, 
    #       #            popup = ~stationname
    #       # )
    #       addMarkers(~Lon, ~Lat, popup = ~as.character(paste(stationname, ": ", rides)),label = ~as.character(stationname)) %>%
    #       addProviderTiles(providers$Stamen.Toner)
    #   } else {
    #     leaflet(sortedReactive) %>% addTiles() %>%
    #       # addCircles(lng = ~Lon, lat = ~Lat, weight = 1,
    #       #            radius = ~sqrt(rides) * 30, 
    #       #            popup = ~stationname
    #       # )
    #       addMarkers(~Lon, ~Lat, popup = ~as.character(paste(stationname, ": ", rides)),label = ~as.character(stationname)) %>%
    #       addProviderTiles(providers$Esri.NatGeoWorldMap)
    #   }
    # })
    
    observe({
      input$reset_button
      output$mymap <- renderLeaflet({
        sortedReactive <- justReactiveDateSelection()
        sortedReactive <- sortedReactive[order(sortedReactive$stationname),]
        x = str_split(sortedReactive$Location[1], ",", n = 2)
        sortedReactive[c('First', 'Last')] <- str_split_fixed(sortedReactive$Location, ', ', 2)
        sortedReactive$Lat <- as.numeric(gsub('[(]','', sortedReactive$First))
        sortedReactive$Lon <- as.numeric(gsub('[)]','', sortedReactive$Last))
        #newYears <-  justOneYearReactive()
            leaflet(sortedReactive) %>% addTiles() %>%
              # setView(lng = -87.6403, lat = 41.8787, zoom = 10) %>%
              setView(lng = median(sortedReactive$Lon), lat = median(sortedReactive$Lat), zoom = 10) %>%
              addProviderTiles("OpenStreetMap", group="bg1") %>%
              addProviderTiles("Esri.NatGeoWorldMap", group="bg2") %>%
              addProviderTiles("Stamen.Toner", group="bg3") %>%
              
              # Add the control widget
              addLayersControl(baseGroups = c("bg1","bg2", "bg3"), 
                               options = layersControlOptions(collapsed = FALSE)) %>%
              # addCircles(lng = ~Lon, lat = ~Lat, weight = 1,
              #            radius = ~sqrt(rides) * 30, 
              #            popup = ~stationname
              # )
              addMarkers(~Lon, ~Lat, layerId=~as.character(stationname), popup = ~as.character(paste(stationname, ": ", rides)),label = ~as.character(stationname))
            # ggplot(df1, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title="Entries in UIC-Halsted from 2001-2021")+scale_y_continuous(labels=comma)
      })
      
    })
    
    
    justOneYearReactive1 <- reactive({
      #print(paste("Station name is:",stationClicked))
      subset(newtable, newtable$year == year(input$date) & newtable$stationname == "UIC-Halsted")
    })
    
    
    justOneYearReactive2 <- reactive({
      #print(paste("Station name is:",stationClicked))
      subsetStation <- subset(newtable, newtable$stationname == "UIC-Halsted")
      
      yearList = c(2001:2021)
      df1 <- data.frame(
        Year = yearList,
        Entries = c(0)
      )
      
      m1 = 1
      for(i in yearList) {
        subsetStationsubset <- subset(subsetStation, year == i)
        #sum(dfUICHalsted$rides)
        sumEntries <- sum(subsetStationsubset$rides)
        df1[m1,2] = sumEntries
        #print(df1[m,2])
        m1=m1+1
      }
      
      df1
    })
    
    # output$dateText  <- renderText({
    #   paste("Date is", as.character(input$date), "and is a", weekdays(input$date),"and station is ", click$id)
    # })
    
    output$hist2 <- renderPlot({
      ny1 <- justOneYearReactive1()
      
      
      df3 <- data.frame(
        Months = months,
        Months_no = months_no,
        Entries = c(0)
      )
      df3$Months <- factor(df3$Months, levels = month.abb)
      
      
      m3 = 1
      for(i in months_no) {
        ny1Subset <- subset(ny1, month == i)
        #print(ny1Subset)
        #sum(dfUICHalsted$rides)
        sumEntries <- sum(ny1Subset$rides)
        #print(sumEntries)
        df3[m3,3] = sumEntries
        #print(df1[m,2])
        m3=m3+1
      }
      titlePlot <- paste("Entries in Months for", ny1$stationname, "in", ny1$year, sep = " ")
      ggplot(df3, aes(x=Months, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Months", title=titlePlot)+scale_y_continuous(labels=comma)
      
      
    })
    
    output$tb2 = renderDT({
      ny1 <- justOneYearReactive1()
      
      
      df3 <- data.frame(
        Months = months,
        #Months_no = months_no,
        Entries = c(0)
      )
      df3$Months <- factor(df3$Months, levels = month.abb)
      
      
      m3 = 1
      for(i in months_no) {
        ny1Subset <- subset(ny1, month == i)
        #print(ny1Subset)
        #sum(dfUICHalsted$rides)
        sumEntries <- sum(ny1Subset$rides)
        #print(sumEntries)
        df3[m3,2] = sumEntries
        #print(df1[m,2])
        m3=m3+1
      }
      datatable(df3,options  = list(lengthMenu = c(7,7)))
      #df3 options  = list(lengthMenu = c(6,6))
    })
    
    output$hist3 <- renderPlot({
      #newYears <-  justOneYearReactive()
      ny1 <- justOneYearReactive1()
      
      df4 <- data.frame(
        Weekday = weekday_list,
        Entries = c(0)
      )
      df4$Weekday <- factor(df4$Weekday, levels = weekday_list, labels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))
      m4 = 1
      for(i in weekday_list) {
        weekday1Subset <- subset(ny1, weekday == i)
        #sum(dfUICHalsted$rides)
        sumEntries <- sum(weekday1Subset$rides)
        df4[m4,2] = sumEntries
        m4=m4+1
      }
      titlePlot <- paste("Entries in Weekdays for", ny1$stationname, "in", ny1$year, sep = " ")
      ggplot(df4, aes(x=Weekday, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Weekdays", title=titlePlot)+scale_y_continuous(labels=comma)
      
      #ny1 <- justOneYearReactive1()
      # ggplot(ny1, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title="Entries in UIC-Halsted from 2001-2021")
    })
    
    output$tb3 = renderDT({
      ny1 <- justOneYearReactive1()
      
      df4 <- data.frame(
        Weekday = weekday_list,
        Entries = c(0)
      )
      df4$Weekday <- factor(df4$Weekday, levels = weekday_list, labels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))
      m4 = 1
      for(i in weekday_list) {
        weekday1Subset <- subset(ny1, weekday == i)
        #sum(dfUICHalsted$rides)
        sumEntries <- sum(weekday1Subset$rides)
        df4[m4,2] = sumEntries
        m4=m4+1
      }
      #df4 
      datatable(df4,options  = list(lengthMenu = c(7,7)))
      
    })
    
    output$hist4 <- renderPlot({
      #newYears <-  justOneYearReactive()
      ny1 <- justOneYearReactive1()
      
      
      titlePlot <- paste("Entries in throughout the year for", ny1$stationname, "in", ny1$year, sep = " ")
      ggplot(ny1, aes(x=lubridateDate, y=rides))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Throughout Year", title=titlePlot)+scale_y_continuous(labels=comma)
    })
    
    output$tb4 = renderDT({
      ny1 <- justOneYearReactive1()
      
      df4 <- data.frame(
        Day = ny1$lubridateDate,
        Entries = ny1$rides
      )
      datatable(df4,options  = list(lengthMenu = c(7,7)))
      
      
    })
    
    output$hist1 <- renderPlot({
      #newYears <-  justOneYearReactive()
      ny1 <- justOneYearReactive2()
      ggplot(ny1, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title="Entries in UIC-Halsted from 2001-2021")+scale_y_continuous(labels=comma)
      
      #ggplot(df2, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title="Entries in UIC-Halsted from 2001-2021")
    })
    
    output$tb1 = renderDT({
      ny1 <- justOneYearReactive2()
      datatable(ny1,options  = list(lengthMenu = c(7,7)))
      # ny1,  options  = list(lengthMenu = c(7,7))
    })
    

    
    
    observe({
      click <- input$mymap_marker_click
      if (is.null(click))
        return()
      
      
      stationClicked = click$id
      print(stationClicked)
      text <-
        paste("Lattitude ",
              click$lat,
              "Longtitude ",
              click$lng)
      
      justOneYearReactive1 <- reactive({
        #print(paste("Station name is:",stationClicked))
        subset(newtable, newtable$year == year(input$date) & newtable$stationname == click$id)
      })
      
      
      justOneYearReactive2 <- reactive({
        #print(paste("Station name is:",stationClicked))
       subsetStation <- subset(newtable, newtable$stationname == click$id)
       
       yearList = c(2001:2021)
       df1 <- data.frame(
         Year = yearList,
         Entries = c(0)
       )
       
       m1 = 1
       for(i in yearList) {
         subsetStationsubset <- subset(subsetStation, year == i)
         #sum(dfUICHalsted$rides)
         sumEntries <- sum(subsetStationsubset$rides)
         df1[m1,2] = sumEntries
         #print(df1[m,2])
         m1=m1+1
       }
       
       df1
      })
      
      # output$dateText  <- renderText({
      #   paste("Date is", as.character(input$date), "and is a", weekdays(input$date),"and station is ", click$id)
      # })
      
      output$hist2 <- renderPlot({
        ny1 <- justOneYearReactive1()


        df3 <- data.frame(
          Months = months,
          Months_no = months_no,
          Entries = c(0)
        )
        df3$Months <- factor(df3$Months, levels = month.abb)


        m3 = 1
        for(i in months_no) {
          ny1Subset <- subset(ny1, month == i)
          #print(ny1Subset)
          #sum(dfUICHalsted$rides)
          sumEntries <- sum(ny1Subset$rides)
          #print(sumEntries)
          df3[m3,3] = sumEntries
          #print(df1[m,2])
          m3=m3+1
        }
        titlePlot <- paste("Entries in Months for", ny1$stationname, "in", ny1$year, sep = " ")
        ggplot(df3, aes(x=Months, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Months", title=titlePlot)+scale_y_continuous(labels=comma)


      })
      
      output$tb2 = renderDT({
        ny1 <- justOneYearReactive1()
        
        
        df3 <- data.frame(
          Months = months,
          #Months_no = months_no,
          Entries = c(0)
        )
        df3$Months <- factor(df3$Months, levels = month.abb)
        
        
        m3 = 1
        for(i in months_no) {
          ny1Subset <- subset(ny1, month == i)
          #print(ny1Subset)
          #sum(dfUICHalsted$rides)
          sumEntries <- sum(ny1Subset$rides)
          #print(sumEntries)
          df3[m3,2] = sumEntries
          #print(df1[m,2])
          m3=m3+1
        }
        datatable(df3,options  = list(lengthMenu = c(7,7)))
        #df3 options  = list(lengthMenu = c(6,6))
      })
      
      output$hist3 <- renderPlot({
        #newYears <-  justOneYearReactive()
        ny1 <- justOneYearReactive1()
        
        df4 <- data.frame(
          Weekday = weekday_list,
          Entries = c(0)
        )
        df4$Weekday <- factor(df4$Weekday, levels = weekday_list, labels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))
        m4 = 1
        for(i in weekday_list) {
          weekday1Subset <- subset(ny1, weekday == i)
          #sum(dfUICHalsted$rides)
          sumEntries <- sum(weekday1Subset$rides)
          df4[m4,2] = sumEntries
          m4=m4+1
        }
        titlePlot <- paste("Entries in Weekdays for", ny1$stationname, "in", ny1$year, sep = " ")
        ggplot(df4, aes(x=Weekday, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Weekdays", title=titlePlot)+scale_y_continuous(labels=comma)
        
        #ny1 <- justOneYearReactive1()
        # ggplot(ny1, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title="Entries in UIC-Halsted from 2001-2021")
      })
      
      output$tb3 = renderDT({
        ny1 <- justOneYearReactive1()
        
        df4 <- data.frame(
          Weekday = weekday_list,
          Entries = c(0)
        )
        df4$Weekday <- factor(df4$Weekday, levels = weekday_list, labels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))
        m4 = 1
        for(i in weekday_list) {
          weekday1Subset <- subset(ny1, weekday == i)
          #sum(dfUICHalsted$rides)
          sumEntries <- sum(weekday1Subset$rides)
          df4[m4,2] = sumEntries
          m4=m4+1
        }
        #df4 
        datatable(df4,options  = list(lengthMenu = c(7,7)))
        
      })
      
      output$hist4 <- renderPlot({
        #newYears <-  justOneYearReactive()
        ny1 <- justOneYearReactive1()
        
        
        titlePlot <- paste("Entries in throughout the year for", ny1$stationname, "in", ny1$year, sep = " ")
        ggplot(ny1, aes(x=lubridateDate, y=rides))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Throughout Year", title=titlePlot)+scale_y_continuous(labels=comma)
      })
      
      output$tb4 = renderDT({
        ny1 <- justOneYearReactive1()
        
        df4 <- data.frame(
          Day = ny1$lubridateDate,
          Entries = ny1$rides
        )
        datatable(df4,options  = list(lengthMenu = c(7,7)))
        
        
      })
      
      output$hist1 <- renderPlot({
        #newYears <-  justOneYearReactive()
        ny1 <- justOneYearReactive2()
        titlePlot <- paste("Entries in", click$id, "from 2001-2021", sep = " ")
        ggplot(ny1, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title=titlePlot)+scale_y_continuous(labels=comma)
       
        #ggplot(df2, aes(x=Year, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Year", title="Entries in UIC-Halsted from 2001-2021")
      })
      
      output$tb1 = renderDT({
        ny1 <- justOneYearReactive2()
        datatable(ny1,options  = list(lengthMenu = c(7,7)))
        # ny1,  options  = list(lengthMenu = c(7,7))
      })
      
      output$dateText  <- renderText({
        paste("Date is", as.character(counter$counterfinalday), "and is a", weekdays(counter$counterfinalday), "and station is" , click$id)
      })
      
      # leafletProxy(mapId = "mymap") %>%
      #   clearPopups() %>%
      #   addPopups(dat = click, lat = ~lat, lng = ~lng, popup = text)
      
      # map$clearPopups()
      # map$showPopup(click$latitude, click$longtitude, text)
    })
    
    
    output$dateText  <- renderText({
      paste("Date is", as.character(counter$counterfinalday), "and is a", weekdays(counter$counterfinalday), " and default station is UIC-Halsted")
    })
    
    
# 
#     output$hist2 <- renderPlot({
#       ny1 <- justOneYearReactive1()
#       
#       
#       df3 <- data.frame(
#         Months = months,
#         Months_no = months_no,
#         Entries = c(0)
#       )
#       df3$Months <- factor(df3$Months, levels = month.abb)
#       
#       
#       m3 = 1
#       for(i in months_no) {
#         ny1Subset <- subset(ny1, month == i)
#         #print(ny1Subset)
#         #sum(dfUICHalsted$rides)
#         sumEntries <- sum(ny1Subset$rides)
#         #print(sumEntries)
#         df3[m3,3] = sumEntries
#         #print(df1[m,2])
#         m3=m3+1
#       }
#       titlePlot <- paste("Entries in Months for", ny1$stationname, "in", ny1$year, sep = " ")
#       ggplot(df3, aes(x=Months, y=Entries))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Entries", x="Months", title=titlePlot)+scale_y_continuous(labels=comma)
#       
#       
#     })
    

    

    
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
