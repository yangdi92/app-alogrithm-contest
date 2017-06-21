library(readr)
library(dplyr)
setwd('D:/Ñ¸À×ÏÂÔØ/tencent/pre')
train<- read_csv('train.csv')

hours<-function(x){
  str<-as.character(x)
  hour<-substr(str,3,4)
  hour<-as.integer(hour)
  return(hour)
}

days<-function(x){
  str<-as.character(x)
  day<-substr(str,1,2)
  day<-as.integer(day)
  return(day)
}

min<-function(x){
  str<-as.character(x)
  min<-substr(str,5,6)
  min<-as.integer(min)
  return(min)
}

train$hours<-hours(train$clickTime)
train$days<-days(train$clickTime)
train$min<-min(train$clickTime)

dt<- train %>% group_by(days,hours,min) %>% 
     summarise(click=n(),install = sum(label),p= sum(label)/n())
dt$time<-c(1:nrow(dt))  
  
library(ggplot2)
dt<-train %>%filter(days==17) %>% group_by(hours)%>% 
  summarise(click=n(),install = sum(label),p= sum(label)/n())
for(i in c(18:30)){
  tmp<-train %>%filter(days==i) %>% group_by(hours)%>% 
    summarise(click=n(),install = sum(label),p= sum(label)/n())
  dt<-rbind(dt,tmp)
}

dt<- train %>% group_by(days)%>% 
  summarise(click=n(),install = sum(label),p= sum(label)/n())
dt %>% ggplot(mapping = aes(x=days,y=click)) +geom_bar(stat = 'identity')

# dt %>% ggplot(mapping = aes(x=hours,y=install)) + geom_line() +
     #  geom_vline(xintercept = c(24,48),color='red') 
dt$hours<-c(1:nrow(dt))
interval<- c(1:13)*24
dt %>% ggplot(mapping = aes(x=time,y=p)) + geom_line()+theme_bw()
         
