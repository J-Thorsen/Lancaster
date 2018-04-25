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
library(dplyr)
library(shiny)
library(lubridate)

#Lanc pop data
Lanc.Pop<-read.csv("https://raw.githubusercontent.com/J-Thorsen/Lancaster/master/ACS_16_5YR_B01003_Pop.csv")


#Bureau of Labor Stats Data - Unemployment Data

lancoJobs2016<-read.csv("https://raw.githubusercontent.com/J-Thorsen/Lancaster/master/2016.annual%2042071%20Lancaster%20County%2C%20Pennsylvania.csv")

lancoJobs2016$industry_title<-str_remove_all(lancoJobs2016$industry_title, "NAICS")

lancoJobs2016$industry_title<-gsub('[[:digit:]]+', '', lancoJobs2016$industry_title)

lanc.indicators2016<-lancoJobs2016%>%
   select(industry_title,own_title, annual_avg_estabs_count, annual_avg_emplvl, avg_annual_pay, lq_annual_avg_estabs_count)
#arrange data by # of establishments
lanc.indicators2016<-arrange(lanc.indicators2016, desc(annual_avg_estabs_count))


lancoJobs2015<-read.csv("https://raw.githubusercontent.com/J-Thorsen/Lancaster/master/2015.annual%2042071%20Lancaster%20County%2C%20Pennsylvania.csv")
lancoJobs2014<-read.csv("https://raw.githubusercontent.com/J-Thorsen/Lancaster/master/2014.annual%2042071%20Lancaster%20County%2C%20Pennsylvania.csv")
lancoJobs2013<-read.csv("https://raw.githubusercontent.com/J-Thorsen/Lancaster/master/2013.annual%2042071%20Lancaster%20County%2C%20Pennsylvania.csv")

#creat one data set will all data from 2013-16 annual averages
lancJobs.all<-bind_rows(lancoJobs2016, lancoJobs2015)
lancJobs.all<-bind_rows(lancJobs.all, lancoJobs2014)
lancJobs.all<-bind_rows(lancJobs.all, lancoJobs2013)  

lancJobs.all$month<-1
lancJobs.all$day<-1
lancJobs.all<-unite(lancJobs.all, Date, c("month","day", "year"), sep="-")
lancJobs.all$Date<-mdy(lancJobs.all$Date)

#total averages for total across  sectors
totals<-filter(lancJobs.all, industry_code=="10")

totals$own_code<-as.factor(totals$own_code)

estbs<-filter(totals, own_code=="5")




#Up to date unemployment data
lan.dat<-read.csv("https://raw.githubusercontent.com/J-Thorsen/Lancaster/master/SeriesReport-LancasterCo.csv")
berks.dat<-read.csv("https://raw.githubusercontent.com/J-Thorsen/Lancaster/master/SeriesReport-berksCo.csv")
chester.dat<-read.csv("https://raw.githubusercontent.com/J-Thorsen/Lancaster/master/SeriesReport-ChesterCo.csv")
dauphin.dat<-read.csv("https://raw.githubusercontent.com/J-Thorsen/Lancaster/master/SeriesReport-DaulphinCo.csv")
lebanon.dat<-read.csv("https://raw.githubusercontent.com/J-Thorsen/Lancaster/master/SeriesReport-LebanonCo.csv")
york.dat<-read.csv("https://raw.githubusercontent.com/J-Thorsen/Lancaster/master/SeriesReport-YorkCo.csv")

em.dat<-bind_rows(lan.dat, berks.dat)
em.dat<-bind_rows(em.dat, chester.dat)
em.dat<-bind_rows(em.dat, dauphin.dat)
em.dat<-bind_rows(em.dat, lebanon.dat)
em.dat<-bind_rows(em.dat, york.dat)


em.dat$day<-1
em.dat<-unite(em.dat, Date, c("Period","day", "Year"), sep="-")
em.dat$Date<-mdy(em.dat$Date)




##Boroughs
borough<-block_groups(state=42, county=071)


pop<-read.csv("https://raw.githubusercontent.com/J-Thorsen/Lancaster/master/ACS_16_5YR_B01003_pop_with_ann.csv")
race<-read.csv("https://raw.githubusercontent.com/J-Thorsen/Lancaster/master/ACS_16_5YR_B02001_Race_with_ann.csv")
income<-read.csv("https://raw.githubusercontent.com/J-Thorsen/Lancaster/master/ACS_16_5YR_B19013_MedianIncomewith_ann.csv")
coords<-read.csv("https://raw.githubusercontent.com/J-Thorsen/Lancaster/master/Boroughs.csv")
#Population:
pop<-pop[-1,]
pop$HD01_VD01 <- as.numeric(as.character(pop$HD01_VD01))
pop.sp<-merge(borough,pop, by.x= "GEOID" , by.y = "GEO.id2")


bins3 <- c(0, 1000, 2000, 3000, 4000, 5000, 6000)
pal3 <- colorBin("YlOrRd", domain = pop.sp$HD01_VD01, bins = bins3)
labels <- sprintf(
  "<strong>%s</strong><br/>%g people",
  pop.sp$GEO.display.label, pop.sp$HD01_VD01
) %>% lapply(htmltools::HTML)


#Median Income
income<-income[-1,]
income$HD01_VD01 <- as.numeric(as.character(income$HD01_VD01))
income.sp<-merge(borough,income, by.x= "GEOID" , by.y = "GEO.id2")



bins4 <- c(0, 20000, 40000, 60000, 80000, 100000, 120000, 140000, 160000 )
pal4 <- colorBin("YlOrRd", domain = income.sp$HD01_VD01, bins = bins4)
labels2 <- sprintf(
  "<strong>%s</strong><br/>%g avg income",
  income.sp$GEO.display.label, income.sp$HD01_VD01
) %>% lapply(htmltools::HTML)











# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
   
   # Application title
   titlePanel("Lancaster County, PA"),
   
   
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel(
            h6("Lancaster County Economic Data"),
            h4("Unemployment Rate by County"),
            h6("The graph displays unemployment rate of Lancaster County and its surrounding counties. Data complied from Department of Labor Satistics 2012-2018. Click the county name's on the the legend to remove or add data to the graph."),
            plotlyOutput("UnemploymentPlot", width = 800),
            
            
            h4("Annual Average Salaries in Lancaster County"),
            h6('The graph below displays average salary from the private and public sectors. The data complied from Bureau of Labor Statistics - Quarterly Census of Employment and Wages'),
            plotlyOutput("Salary", width= 800),
            
            
            h4("Annual Average Establishment Count - Private Sector in Lancaster County"),
            h6("The graph below displays annual average establishment count for the private sector in Lancaster County. The data complied from Bureau of Labor Statistics - Quarterly Census of Employment and Wages"),
            plotlyOutput("Estabs", width = 800),
            
            h4("Lancaster County Labor Statistics by Industry"), 
            h6("Below is searchable data displaying the Quarterly Census of Employment and Wages provided by The Bureau of Labor Statistics for Lancaster County."),
            DT::dataTableOutput('ex1')
            ),
            
          
          tabPanel(
            h6("Lancaster County Borough Demographics"),
        h4("Lancaster County Population by Block Group"),
        h6("The map displays data complied from 2016 ACS 5 Year Estimates- Table - B01003. Click on markers to display Borough name"),
        leafletOutput("mymap", width= 800),
        
        h4("Lancaster County Median Income by Block Group"),
        h6("The map displays data complied from 2016 ACS 5 Year Estimates- Table - B19013. Click on markers to display Borough name"),
        leafletOutput("income", width=800)
          )
         
        )
         
         
       
         
      )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$mymap <- renderLeaflet({
     map<-leaflet(pop.sp)%>%
       setView(lng = -76.305386, lat = 40.041220, zoom = 9) %>%
       addTiles()%>%
       addPolygons(
         fillColor=~pal3(HD01_VD01),
         weight = 1,
         opacity = 1.0,
         fillOpacity = 0.7,
         label = labels,
         color="white")%>%
       addLegend(pal = pal3, values = ~HD01_VD01, opacity = 0.7, title = "Population",
                 position = "bottomright")%>%
        addProviderTiles(providers$CartoDB.Positron)%>%
       addCircles(lng= coords$Long, lat= coords$Lat, color="Black", radius= 8, opacity= .8, popup=coords$Borough)
   })
     
   output$income<-renderLeaflet({
     map2<-leaflet(income.sp)%>%
       setView(lng = -76.305386, lat = 40.041220, zoom = 9) %>%
       addTiles()%>%
       addPolygons(
         fillColor=~pal4(HD01_VD01),
         weight = 1,
         opacity = 1.0,
         fillOpacity = 0.7,
         label = labels2,
         color="white")%>%
       addLegend(pal = pal4, values = ~HD01_VD01, opacity = 0.7, title = "Median Income",
                 position = "bottomright")%>%
       addProviderTiles(providers$CartoDB.Positron)%>%
       addCircles(lng= coords$Long, lat= coords$Lat, color="Black", radius= 8, opacity= .8, popup=coords$Borough)
   })
     
    output$UnemploymentPlot<- renderPlotly({
      a<-plot_ly(em.dat, x= ~Date, y = ~unemployment.rate, color = ~County, mode="lines+scatter")%>%
        layout(
          title = 'Unemployment Rates of Lancaster and Surrounding Counties',
          yaxis=list(title='Unemployment Rate(%)'))
      a
      
   })
    
    
  
      
    output$Salary<- renderPlotly({
      plots<-plot_ly(totals, x=~Date, y=~avg_annual_pay, color=~own_title, mode="lines+markers")%>%
        add_trace()%>%
        layout(yaxis=list(title='Average Salary'))
        
      
      plots
      })
      
    output$Estabs<-renderPlotly({
      p.estbs<-plot_ly(estbs, x=~Date, y=~annual_avg_estabs_count, color=~own_title, mode="lines+markers")%>%
        add_trace()%>%
        layout(yaxis=list(title='Annual Average Establishment Counts'))
      
      p.estbs
      })
  
      output$ex1 <- DT::renderDataTable(
        DT::datatable(lanc.indicators2016, options = list(pageLength = 25))
      )
    
}

# Run the application 
shinyApp(ui = ui, server = server)

