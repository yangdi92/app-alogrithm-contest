user_frequecny<- nrow(dt)/n_distinct(dt$userID)
dt0<-left_join(train,ad,by="creativeID") %>%left_join(user,by='userID') %>%
  left_join(position,'positionID') %>% left_join(app_categories,by='appID')
tmp<-dt0%>%group_by(age) %>% summarise(click = n(),install=sum(label),p = sum(label)/n())

entropy= -log(mean(dt$label))*mean(dt$label)-log(1-mean(dt$label))*(1-mean(dt$label))
N<-nrow(dt)
grain_ratio<-function(tmp){
  pk<-tmp$click/N
  p2<- tmp$p*log(tmp$p)+(1-tmp$p)*log(1-tmp$p)
  cg<-sum(pk*p2)
  had<- (-sum(pk*log(pk)))
  ratio<- ((entropy+cg)/had)
  return(ratio)
}

grain_ratio(tmp)
library(ggplot2)
# tmp %>% ggplot(mapping=aes(x=age,y=p,fill=age))+geom_bar(stat = 'identity')
 tmp %>% ggplot(mapping=aes(x=age,y=click))+geom_line(color='red')
 
 tmp<-dt%>%group_by(weekday) %>% summarise(click = n(),install=sum(label),p = sum(label)/n())       
tmp$weekday<-as.factor(tmp$weekday)
tmp %>% ggplot(mapping=aes(x=weekday,y=p,fill=weekday))+geom_bar(stat = 'identity')+
        guides(fill=F)
         
