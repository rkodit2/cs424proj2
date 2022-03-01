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
augustEntries <- subset(newtable, lubridateDate == "2021-08-23")
sortedAug <- augustEntries[order(augustEntries$stationname),]

x = str_split(sortedAug$Location[1], ",", n = 2)
sortedAug[c('First', 'Last')] <- str_split_fixed(sortedAug$Location, ', ', 2)
sortedAug$Lat <- as.numeric(gsub('[(]','', sortedAug$First))
sortedAug$Lon <- as.numeric(gsub('[)]','', sortedAug$Last))

#print(gsub('[(]','', sortedAug$First))
print(sortedAug)


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
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           leafletOutput("mymap")
        )
    )
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
      ggplot(sortedAug, aes(x=stationname, y=rides))+geom_bar(stat="identity", fill="#1f78b4")+labs(y = "Total Rides", x="Stations", title="Entries in CTA for August 23, 2021")+scale_y_continuous(labels=comma)+theme(axis.text.x = element_text(angle = 90))
    })
    output$mymap <- renderLeaflet({
      leaflet(sortedAug) %>% addTiles() %>%
        # addCircles(lng = ~Lon, lat = ~Lat, weight = 1,
        #            radius = ~sqrt(rides) * 30, 
        #            popup = ~stationname
        # )
        addMarkers(~Lon, ~Lat, popup = ~as.character(rides), label = ~as.character(stationname))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
