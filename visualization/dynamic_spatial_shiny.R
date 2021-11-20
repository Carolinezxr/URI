library(sf)           # Objects and functions for geospatial data
library(rgdal)        # Functions for spatial data input/output
library(ggplot2)      # Graphing functions
library(dplyr)        # Functions for processing tabular data
library(tidyr)        # Functions for processing tabular data
library(scales)       # Additional graphics functions
library(RColorBrewer) # Color ramps for graphs and maps
library(gridExtra)    # Functions for arranging multiple plots on a page
library(readr)        # Functions for reading data
library('plotly')
library("gapminder")
library(shiny)    # for shiny apps
library(leaflet)  # renderLeaflet function
library(maptools) 
library(reshape2)
library(sp)


data<-read.csv("E:\\data\\app.csv")
beat_map<-st_read("E:\\data\\amityrosado\\Policing-GIS\\Merged without O'Hare.shp") 
beat_map<-beat_map[,c(3,9)]

ui<-fluidPage(
  titlePanel("Chicago City Crime Reports"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("date","Date of Crime",min=as.Date("2018/08/01",format = "%Y/%m/%d"),max=as.Date("2019/07/31",format = "%Y/%m/%d"),value=as.Date("2018/09/12",format = "%Y/%m/%d"),step = 1)
      # selectInput(
      #   inputId = "dateselected",
      #   label = "Select date",
      #   choices = seq.Date(from = as.Date("2018/08/01",format = "%Y/%m/%d"), by = "day", length.out = 365)
      # )
    ),
    mainPanel(plotlyOutput("crimeplot"))
  )
)


server<-function(input,output){

  
  output$crimeplot<-renderPlotly({
    
    datafiltered <- data[which(data$date == input$date), ]
    orderbeats <- match(beat_map$beat_num, datafiltered$beat_num)
    beat_map <- left_join(beat_map, datafiltered[orderbeats, ], by = c("beat_num" = "beat_num"))
    
    
    p<-ggplot(beat_map) +  geom_sf(aes(fill = beat_map$reports_num))+  
      scale_fill_gradient(low="white", high="red")+
      theme_bw(base_family = "STKaiti")+
      ggtitle(input$date)
    
    ggplotly(p)
    

  })
  
}

shinyApp(ui = ui, server = server)

