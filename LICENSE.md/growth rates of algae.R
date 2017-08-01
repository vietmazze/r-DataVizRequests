

install.packages("XML")
install.packages("RCurl")
install.packages("rlist")
library(XML)
library(RCurl)
library(tidyverse)
library(rlist)
library(stringr)
# Getting the data from URL
theurl <- getURL("http://www.aquatext.com/tables/algaegrwth.htm",.opts = list(ssl.verifypeer = FALSE) )
tables <- readHTMLTable(theurl)
tables <- list.clean(tables, fun = is.null, recursive = FALSE)
n.rows<- unlist(lapply(tables, function(t) dim(t)[1]))
beta<-tables[[which.max(n.rows)]]
beta

#Tidy data

col_names<-beta[,1] %>%
  str_replace(" ","_") %>%
  str_replace("\n",'') %>%
  str_replace("    ","") %>%
  str_replace("\\(.*\\)|\\.", "") %>%
  str_replace("\'\'","")
data<-as.tibble(t(beta[,-1]))

  
colnames(data)<-col_names


data<-data[,-2]


#Adding temperature

data<-data %>%
  mutate(
    temperature=c(5,5,10,10,25,25,30,30)
  )
 #Alternative to adding temperature 
data$temperature <- unlist(lapply(c(5, 10, 25, 30), rep, 2))

# Mutate and gather variables in correct order
data1<- data %>%
  mutate_all(as.numeric) %>%
  gather(2:20,key="Species",value="growth") %>%
  arrange(temperature,`Light_ Intensity `) %>%
  mutate(light_intensity= as.factor(`Light_ Intensity `))
  
data1<-data1[,c(3,1,2,4)]
data1

#GRAPH RELATIONSHIP FOR GROWTH BETWEEN TEMPERATURE AND LIGHT INTENSITY
ggplot(data1,aes(x=temperature,y=growth,color=light_intensity))+
  geom_smooth(method="lm",alpha=0.5)+
  geom_point()+
  scale_x_continuous(breaks = c(5, 10, 25, 30)) +
  facet_wrap(~ Species, ncol = 4) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Growth rate of algae by species",
       x = "Temperature (degrees celsius)",
       y = "Growth rate (divisions per day)",
       color = "Light intensity (lux)")


ggplot(data1,aes(x=temperature,y=growth,color=light_intensity))+
  geom_bar(aes(fill=factor(temperature), group=temperature),
stat="identity", position="dodge",
color="black", alpha=.7)+
  facet_wrap(~Species, ncol=4)
 
