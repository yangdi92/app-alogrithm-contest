library(dplyr)
library(readr)
setwd('D:/Ñ¸À×ÏÂÔØ/tencent/pre')
feature1<-read_csv("feature1.csv")
ad<- read_csv('ad.csv')
feature1<- feature1 %>% left_join(ad,by ='creativeID')
user_app_actions<-read_csv('user_app_actions.csv')
feature1<-feature1%>% left_join(user_app_actions,by=c("userID","appID"))
feature1$installTime[is.na(feature1$installTime)] = 0
feature1<-feature1 %>% mutate(install_or_not = if_else((clickTime-as.integer(installTime))<0, 1,0))
feature1<- feature1 %>% select(-installTime)
feature1<-feature1[,vars]
write_csv(feature1,'feature1.csv')


feature2<-read_csv('feature2.csv')

vars<-names(feature2)
user_app_actions<-read_csv('user_app_actions.csv')
feature1<-feature1%>% left_join(user_app_actions,by=c("userID","appID"))
feature1$installTime[is.na(feature1$installTime)] = 0
feature1<-feature1 %>% mutate(install_or_not = if_else((clickTime-as.integer(installTime))<0, 1,0))
feature1<- feature1 %>% select(-installTime)
