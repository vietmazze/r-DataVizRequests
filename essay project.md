---
title: "Essay Graphs for r/Datavizrequest user"
author: "Duy Nguyen"
date: "August 10, 2017"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(readxl)
library(stringr)
library(lubridate)
library(chron)
```

##Reading excel file
```{r import,cache=TRUE}
test<-read_excel("Undergrad Essays Written.xlsx",1,col_names=FALSE,skip=1)

head(test)
```
##Fixing the data
```{r fix,results = "hide", cache=TRUE}
colnames(test)<-c("Year","Date","Time")

test<-test%>%
  separate(Year,into=c("Year","Semester"),sep=",")

test$Year[[1]]<-("1.0")
test$Year[[10]]<-("1.0")
test$Year[[20]]<-("2.0")
test$Year[[32]]<-("2.0")
test$Year[[49]]<-("3.0")
test$Year[[59]]<-("3.0")
test$Year[[71]]<-("4.0")
test$Year[[90]]<-("4.0")

test$Semester[[1]]<-("1")
test$Semester[[10]]<-("2")
test$Semester[[20]]<-("1")
test$Semester[[32]]<-("2")
test$Semester[[49]]<-("1")
test$Semester[[59]]<-("2")
test$Semester[[71]]<-("1")
test$Semester[[90]]<-("2")

test<-test%>%
  fill(Year)
test<-test%>%
  fill(Semester)
test$Year<-as.numeric(test$Year)
test$Semester<-as.numeric(test$Semester)
test<-as.tibble(test, na.rm=TRUE)


```

```{r data}
head(test)
```

##Converting to datetime variable
```{r time}
test$Time<-as.numeric(test$Time)
test$Time<-chron::times(test$Time)


test<- test%>% 
  unite(datetime,Date,Time,sep=" ")

test<-test[-21,]
test$datetime<-parse_datetime(test$datetime,"%Y-%m-%d %H:%M:%S")
```

# Looking at tidy data
```{r data2}
test
```

#Graphing histogram and boxplot for data

```{r graphing}
ggplot(test,aes(hour(datetime)))+
  geom_histogram(bins=40,color="black",fill="steelblue1")+
  labs(title="Essays", x="Hour",y="Number",caption="created by DQN")+
  facet_grid(paste("year",Year)~paste("semester",Semester))+
  theme_bw()

![alt text](https://github.com/vietmazze/r-DataVizRequests/blob/master/graph1%20essay%20project.jpeg "Histogram")


ggplot(test,aes(y=hour(datetime),x=Year,group=Semester))+
  geom_boxplot(color="blue")+
  facet_grid(paste("year",Year)~paste("semester",Semester))+
  labs(title="Summary of time used the most to do essay",x="Year",y="Hour")+
  theme_bw()
```
![alt text](https://github.com/vietmazze/r-DataVizRequests/blob/master/boxplot%20for%20essay%20project.jpeg "Boxplot")
