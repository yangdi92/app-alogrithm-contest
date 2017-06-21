library(readr)
library(dplyr)
setwd('D:/Ñ¸À×ÏÂÔØ/tencent/pre')
ad<-read_csv('ad.csv')
app_categories<-read_csv('app_categories.csv')
user<-read_csv('user.csv')
position<-read_csv('position.csv')
# user_app_actions<-read_csv('user_app_actions.csv')
# user_installedapps<-read_csv('user_installedapps.csv')
test<-read_csv('test.csv')
dt<-left_join(test,ad,by="creativeID") %>%left_join(user,by='userID') %>%
  left_join(position,'positionID') %>% left_join(app_categories,by='appID')

# extract features from user.csv
age_f<-function(x){
  case_when(
    x==0 ~ 0,
    x>=1&x<=10 ~ 1,
    x>10&x<=28 ~2,
    x>28&x<=45 ~ 3,
    x>45&x<=60 ~ 4,
    x>60 ~ 5
    
  )
}
user_feature<-user %>% mutate_at(c(2),age_f)
f1<-function(x){trunc(x/100)}
user_feature<-user_feature%>%mutate_at(c('hometown'),f1)
user_feature<-user_feature%>%mutate_at(c('residence'),f1)
user_feature<-user_feature %>% mutate(home_or_not = as.integer(hometown==residence))
# write_csv(user_feature,'user_feature.csv')
# user_feature<-read_csv('user_feature.csv')
dt<- test %>% left_join(user_feature,by='userID') 
weekday_f<-function(x) {trunc(x/10000)%% 7}
dt$weekday = sapply(dt$clickTime,weekday_f)
hour_f<-function(x){
  t <- as.integer(substr(as.character(x),3,4))
  if(t>=0&t<6) 
    hour = 1
  else if (t>=6 & t<18)
    hour = 2
  else 
    hour = 3
}
dt$hours = sapply(dt$clickTime,hour_f)
write_csv(dt,'sub_user_features.csv')
