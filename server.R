library(shiny)
library(maps)
library(dplyr)



shinyServer(function(input,output){
  
  data(world.cities)
  
  # adding variable ind, which is set to 1 and will 
  # serve as a "count" variable for the different cities
  # in world.cities
  x<-mutate(world.cities,ind=1)
  
  # grouping by country
  x<-group_by(x,country.etc)
  
  # total contains the number of cities for each country
  y<-summarize(x,total=sum(ind))
  
  # merging the two datasets to have the information of the
  # world.cities dataset available together with the 
  # information about the number of cities for each country
  cities<-merge(x,y,by="country.etc")
  
  # we only keep those countries with at least 150 city entries
  cities<-subset(cities,total>149)
  
  # grouping the dataset by country
  byCountry<-group_by(cities,country.etc)
  
  # ordering by country and city with descending population
  s<-(arrange(byCountry,desc(pop)))
  
  # creating data frame with information about countries,
  # cities and the respective longitude and latitude
  a<-as.data.frame(list(name=s$name,lat=s$lat,long=s$long,country=as.character(s$country.etc)))
  
  s<-reactive ({
    a[a$country==input$country,]
  })

 sel<-reactive( {
   as.data.frame(list(lat=s()$lat,long=s()$long))
 })
  #selectedData<-t[,c("lat","long")]
  
  clusters <- reactive({
    kmeans(sel(), input$clusters)
  })
  
  v<- reactive(
  {
    s()[1:input$citanz,]
  })
  
  
 
  output$plot1 <- renderPlot({
    par(mfrow=c(1,2)
       # ,mar = c(4.1,3.1, 0, 0)
        ) 
    plot(sel()$long, sel()$lat,xlab="Longitude",ylab="Latitude",
                           col = clusters()$cluster,
                           pch = 20, cex = 3)
    #points(clusters()$centers, pch = 4, cex = 4, lwd = 4,col="blue")
    plot(v()$long, v()$lat,xlab="Longitude",ylab="Latitude",
        col = clusters()$cluster,
         pch = 20, cex = 3)
    text(v()$long,v()$lat,v()$name,cex=0.6, pos=4, col="black")
 
  })
  
  #output$text1 <- renderText({
  #  as.data.frame(sel())
  #})
  })