llibrary(readr)
library(dplyr)
setwd('D:/Ñ¸À×ÏÂÔØ/tencent/pre')
feature4<-read_csv("feature4.csv")
app_categories<-read_csv('app_categories.csv')
ad<- read_csv('ad.csv')
position <-read_csv('position.csv')
feature4<- feature4 %>% left_join(ad,by ='creativeID') %>% 
  left_join(app_categories,by='appID')%>%
  left_join(position,by = 'positionID')

label4<-read_csv('label4.csv')
label4<- label4 %>% left_join(ad,by ='creativeID') %>% 
  left_join(app_categories,by='appID')%>%
  left_join(position,by = 'positionID')
rm(ad,app_categories,position);gc()

# feature enginnering from the perspective of user

## count the history statistics by group age
label4<- feature4 %>% group_by(age)%>% 
  summarise(age_n=sum(label),age_per = n()/n_distinct(userID),age_p = sum(label)/n())%>%
  right_join(label4,by = 'age')
## count the history statistics by group gender
label4<- feature4 %>% group_by(gender)%>% 
  summarise(gen_n=sum(label),gen_per = n()/n_distinct(userID),gen_p = sum(label)/n())%>%
  right_join(label4,by = 'gender')
## count the history statistics by group education
label4<- feature4 %>% group_by(education)%>% 
  summarise(edu_n=sum(label),edu_per = n()/n_distinct(userID),edu_p = sum(label)/n())%>%
  right_join(label4,by = 'education')

## count the historical statistics by group marriagestatus
label4<- feature4 %>% group_by(marriageStatus)%>% 
  summarise(mar_n=sum(label),mar_per = n()/n_distinct(userID),mar_p = sum(label)/n())%>%
  right_join(label4,by = 'marriageStatus')
## count the historical statistics by group hometown
label4<- feature4 %>% group_by(hometown)%>% 
  summarise(hom_n=sum(label),hom_per = n()/n_distinct(userID),hom_p = sum(label)/n())%>%
  right_join(label4,by = 'hometown')
## count the historical statistics by group residence
label4<- feature4 %>% group_by(residence)%>% 
  summarise(res_n=sum(label),res_per = n()/n_distinct(userID),res_p = sum(label)/n())%>%
  right_join(label4,by = "residence")

## count the historical statistics by group home_or_not
label4<- feature4 %>% group_by(home_or_not)%>% 
  summarise(hon_n=sum(label),hon_per = n()/n_distinct(userID),hon_p = sum(label)/n())%>%
  right_join(label4,by = 'home_or_not')

#  feature engineering from the perspective of ad position

## count the historical statistics by group sitesetID
label4<- feature4 %>% group_by(sitesetID)%>% 
  summarise(sit_n=sum(label),sit_per = n()/n_distinct(userID),sit_p = sum(label)/n())%>%
  right_join(label4,by = 'sitesetID')
## count the historical statistcs by group positionID
label4<- feature4 %>% group_by(positionID)%>% 
  summarise(pos_n=sum(label),pos_per = n()/n_distinct(userID),pos_p = sum(label)/n())%>%
  right_join(label4,by = "positionID")
## count the historical statistics by group positiontype
label4<- feature4 %>% group_by(positionType)%>% 
  summarise(pty_n=sum(label),pty_per = n()/n_distinct(userID),pty_p = sum(label)/n())%>%
  right_join(label4,by = "positionType")

# feature enginnering from the perspective of add 

## count the historical statistics by group creativeID
label4<- feature4 %>% group_by(creativeID)%>% 
  summarise(cre_n=sum(label),cre_per = n()/n_distinct(userID),cre_p = sum(label)/n())%>%
  right_join(label4,by = "creativeID")

## count the historical statistics by group camgaignID

label4<- feature4 %>% group_by(camgaignID)%>% 
  summarise(cam_n=sum(label),cam_per = n()/n_distinct(userID),cam_p = sum(label)/n())%>%
  right_join(label4,by = "camgaignID")

## count the historical statistics by group adID
label4<- feature4 %>% group_by(adID)%>% 
  summarise(ad_n=sum(label),ad_per = n()/n_distinct(userID),ad_p = sum(label)/n())%>%
  right_join(label4,by = "adID")
## count the historical ststistics by group appID

label4<- feature4 %>% group_by(appID)%>% 
  summarise(app_n=sum(label),app_per = n()/n_distinct(userID),app_p = sum(label)/n())%>%
  right_join(label4,by = "appID")
## count the historical statistics  group advertiserID

label4<- feature4 %>% group_by(advertiserID)%>% 
  summarise(adv_n=sum(label),adv_per = n()/n_distinct(userID),adv_p = sum(label)/n())%>%
  right_join(label4,by = "advertiserID")
## count the historical statistics by group appPlatform
label4<- feature4 %>% group_by(appPlatform)%>% 
  summarise(apla_n=sum(label),apla_per = n()/n_distinct(userID),apla_p = sum(label)/n())%>%
  right_join(label4,by = "appPlatform")
## count the historical statistics by group appCategory
label4<- feature4 %>% group_by(appCategory)%>% 
  summarise(acat_n=sum(label),acat_per = n()/n_distinct(userID),acat_p = sum(label)/n())%>%
  right_join(label4,by = "appCategory")

# feature enginnering from the perspective connection
## count the historical statistics by connectionType
label4<- feature4 %>% group_by(connectionType)%>% 
  summarise(con_n=sum(label),con_per = n()/n_distinct(userID),con_p = sum(label)/n())%>%
  right_join(label4,by = "connectionType")
## count the historical statistics by telecomsOperator

label4<- feature4 %>% group_by(telecomsOperator)%>% 
  summarise(tel_n=sum(label),tel_per = n()/n_distinct(userID),tel_p = sum(label)/n())%>%
  right_join(label4,by = "telecomsOperator")

# feature enginnering from the perspective of (userID, ad)

# count the history statistics by group userID and creativeID
label4<- feature4%>% group_by(creativeID,userID)%>%
  summarise(u_crea_n=n(),u_crea_n1=sum(label),u_crea_p=sum(label)/n()) %>%
  right_join(label4,by=c("creativeID","userID"))

# count the history statistics by group userID and camgaignID
label4<-feature4%>% group_by(camgaignID,userID)%>%
  summarise(u_cam_n=n(),u_cam_n1=sum(label),u_cam_p=sum(label)/n()) %>%
  right_join(label4,by=c("camgaignID","userID"))
# count the history statistics by group userID and advertiserID
label4<- feature4%>% group_by(advertiserID,userID)%>%
  summarise(u_adv_n=n(),u_adv_n1=sum(label),u_adv_p=sum(label)/n()) %>%
  right_join(label4,by=c("advertiserID","userID"))
# count the history statistics by group userID and appID
label4<-feature4%>% group_by(appID,userID)%>%
  summarise(u_app_n=n(),u_app_n1=sum(label),u_app_p=sum(label)/n()) %>%
  right_join(label4,by=c("appID","userID"))

## count the history statistics by group userID and appcategory
label4<- feature4%>% group_by(userID,appCategory)%>%
  summarise(u_acat_n=n(),u_acat_n1=sum(label),u_acat_p=sum(label)/n()) %>%
  right_join(label4,by=c("appCategory","userID"))


# feature enginnering from the perspective of (userI, positionID)

## count the history statistics by group userID and positionID
label4<-feature4%>% group_by(userID,positionID)%>%
  summarise(u_pos_n=n(),u_pos_n1=sum(label),u_pos_p=sum(label)/n()) %>%
  right_join(label4,by=c('userID','positionID'))
# count the history statistics by group (userID sitesetID)
label4<-label4%>% group_by(userID,sitesetID)%>%
  summarise(u_sit_n=n(),u_sit_n1=sum(label),u_sit_p=sum(label)/n()) %>%
  right_join(label4,by=c('userID','sitesetID'))

# whether the user have installed the app

label4<-feature4 %>% filter(label ==1) %>%
  group_by(userID,appID) %>% summarise(installed1 = sum(label)) %>%
  right_join(label4,by=c('userID','appID'))

user_app_actions<-read_csv('user_app_actions.csv')
label4<-user_app_actions%>%filter(installTime<270000)%>% 
  group_by(userID,appID)%>%
  summarise(installed2 = n())  %>%
  right_join(label4,by=c('userID','appID'))

# a<-label4 %>% select(userID,appID,clickTime,label,installed1,installed2) %>% 
# filter(label==0 & installed2 ==1)
write_csv(label4,'dt4.csv')