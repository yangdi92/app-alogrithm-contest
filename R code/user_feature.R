library(readr)
library(dplyr)
setwd('D:/Ñ¸À×ÏÂÔØ/tencent/pre06')
ad<-read_csv('ad.csv')
app_categories<-read_csv('app_categories.csv')
user<-read_csv('user.csv')
position<-read_csv('position.csv')
# user_app_actions<-read_csv('user_app_actions.csv')
# user_installedapps<-read_csv('user_installedapps.csv')
train<-read_csv('train.csv')
dt<-left_join(train,ad,by="creativeID") %>%left_join(user,by='userID') %>%
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
dt<- train %>% left_join(user_feature,by='userID') 
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
write_csv(user_feature,'user_feature.csv')
# compuet conversiontime 
# temp<-dt%>%filter(is.na(conversionTime)==FALSE) 
# temp$timegap = trunc(temp$conversionTime/10000)-trunc(temp$clickTime/10000)
# gap <-temp %>% group_by(weekday) %>% 
#       summarise(n=n(),n1 = sum(timegap!=0),ave=sum(timegap)/sum(timegap!=0))
# write_csv(gap,'gap.csv')

# partition the sample by time
feature8<-dt %>% filter(clickTime>232359)
write_csv(feature8,'feature8.csv')
rm(feature8)
feature_interval = 7
label_interval = 1
n_set <-15-feature_interval
x_names<-paste('feature',as.character(c(1:n_set)),sep = '')
y_names<-paste('label',as.character(c(1:n_set)),sep = '')
pathx<-paste(x_names,'csv',sep = '.')
pathy<-paste(y_names,'csv',sep = '.')
star_time<- 170000
interval_time<-feature_interval*10000
end_time<-star_time + interval_time
for(i in 1:n_set){
  tmp<-dt %>% filter(clickTime>= star_time&clickTime<end_time)
  write_csv(tmp,path = pathx[i])
  #tmp<-dt %>% filter(clickTime>= end_time&clickTime<end_time+10000)
  #write_csv(tmp,path = pathy[i])
  star_time = star_time + 10000
  end_time = end_time+10000
  if(end_time>300000)
    break
}

# 
# tmp<- dt%>%left_join(ad,by='creativeID')%>%
#       left_join(user_app_actions,by=c('userID','appID'))
# 
# T1<-tmp%>% filter(is.na(installTime)==T) %>% 
#         select(userID,appID,clickTime,installTime,conversionTime,label) %>%
#          filter(label==1)
