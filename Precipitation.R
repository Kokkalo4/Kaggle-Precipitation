#libraries
library(plyr)
library(dplyr)
library(ggplot2)
library(Hmisc)
library(plotly)

#load data
precip <- read.csv("c:/users/alex/desktop/R/exercise data/Precipitation in Syracuse NY/Precipitation_Syracuse_NY.csv", header = T)

#check structure
str(precip)
describe(precip)
head(precip)
tail(precip)

#convert date variable from "integer" to "date"
precip$Date <- as.Date(as.character(precip$Date), "%Y%m%d")

#create variables of the week and month of each observation:
precip$Month <- as.Date(cut(precip$Date,
                         breaks = "month"))
precip$Week <- as.Date(cut(precip$Date,
                        breaks = "week",
                        start.on.monday = FALSE))

#Precipitation Frequency
ggplot(precip, aes(precip$Precipitation , fill = precip$Precipitation)) + geom_bar() +
  scale_fill_discrete(name="Precipitation") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(x= "Precipitation",y= "Frequency" , title = "Precipitation Level Frequency")


ggplot(precip, aes(precip$AvgPrecipitation, fill = precip$AvgPrecipitation)) + geom_bar() +
  scale_fill_discrete(name="Precipitation") +
  labs(x= "Average Precipitation",y= "Frequency" , title = "Average Precipitation Level Frequency")

#Precipitation Distribution
plot_ly(x = precip$Date , y = precip$Precipitation, type = "bar") %>% 
  layout(title = "Precipitation over Syracuse NY",
         xaxis = list(title = "Date"), 
         yaxis = list(title = "Precipitation"))

plot_ly(x = precip$Date , y = precip$AvgPrecipitation, type = "bar") %>% 
  layout(title = "Average Precipitation over Syracuse NY",
         xaxis = list(title = "Date"), 
         yaxis = list(title = "Average Precipitation"))

#Precipitation over Months
plot_ly(x = precip$Month , y = precip$Precipitation , type = "bar") %>% 
  layout(title = "Precipitation over Syracuse NY",
         xaxis = list(title = "Date"), 
         yaxis = list(title = "Precipitation"))


plot_ly(x = precip$Month , y = precip$AvgPrecipitation , type = "bar") %>% 
  layout(title = "Average Precipitation over Syracuse NY",
         xaxis = list(title = "Date"), 
         yaxis = list(title = "Average Precipitation"))

#Comparison Precipitation and Average Precipitation by Month
plot_ly(x = precip$Month , y = precip$Precipitation, type = "bar" , name = "Precipitation") %>% 
  layout(title = "Precipitation over Syracuse NY",
         xaxis = list(title = "Date"))%>% 
  add_trace(
  x = precip$Month,
  y = precip$AvgPrecipitation,
  name = "Average Precipitation",
  type = "bar")

#---------------------------------------------------------------------------------------------------------------------



library(data.table)
dtpre=data.table(date = as.IDate(as.Date(as.character(precip$Date),"%Y%m%d"))
                 ,precip[-1])
dtpre[, list(mean = mean(AvgPrecipitation),max = max(AvgPrecipitation)), 
      by = year(date)]
dtpre[, list(mean = mean(AvgPrecipitation),max = max(AvgPrecipitation)), 
      by = month(date)]


library(lubridate)
precip$year=year(precip$Date)
precip$day=yday(precip$Date)#year day (1-365)
ggplot(precip[precip$year==2012,c(1,3)],aes(Date,AvgPrecipitation))+geom_line()

ggplot(precip,aes(day,AvgPrecipitation))+geom_line()+facet_grid(year~.)


precip
