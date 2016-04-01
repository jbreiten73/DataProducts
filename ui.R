# DataProducts

library(shiny)
library(maps)
library(dplyr)

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

shinyUI(pageWithSidebar(
  headerPanel('Countries of the World: k-means Clustering'),
  sidebarPanel(
    selectInput('country', 'Country', unique(as.character(a$country))),
    numericInput('clusters', 'Cluster count', 3,
                 min = 1, max = 9),
    numericInput('citanz', 'Numbers of Cities', 25,
                 min = 15, max =150 )
  ),
  mainPanel(
    plotOutput('plot1')
  )
))

