library(shiny)
library(shinythemes)
library(leaflet)
library(tigris)
library(acs)
library(maptools)
library(base)
library(dygraphs)
library(png)
library(plotly)
library(rgeos)
library(tidyverse)
library(rgdal)
library(ggthemes)
library(tmap)



library(shiny)
#Lanc pop data
Lanc.Pop<-read.csv("https://raw.githubusercontent.com/J-Thorsen/Lancaster/master/ACS_16_5YR_B01003_Pop.csv")

#shapefile data for PA counties

County<-tracts(state="Pennsylvania", cb=TRUE)

#Subset to include Lancaster Co
LancCo<-County[County$COUNTYFP =="071", ]

#merge SPDF with Pop data
Lanc.Merge<-merge(LancCo, Lanc.Pop, by.x= "GEOID", by.y = "GEO.id2")

#map labels
bins <- c(0, 1771, 4063, 5215, 6673, 10640, Inf)
pal <- colorBin("YlOrRd", domain = Lanc.Merge$HD01_VD01, bins = bins)
labels <- sprintf(
  "<strong>%s</strong><br/>%g people",
  Lanc.Merge$GEO.display.label, Lanc.Merge$HD01_VD01
) %>% lapply(htmltools::HTML)





# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Lancaster, PA Demographics"),
   
   
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("mymap")
      )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$mymap <- renderLeaflet({
     map<-leaflet(Lanc.Merge)%>%
       setView(lng = -76.305386, lat = 40.041220, zoom = 10) %>%
       addTiles()%>%
       addPolygons(
         fillColor=~pal(HD01_VD01),
         weight = 1,
         opacity = 1.0,
         fillOpacity = 0.7,
         label = labels,
         color="white")%>%
       addLegend(pal = pal, values = ~HD01_VD01, opacity = 0.7, title = "Population",
                 position = "bottomright")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

