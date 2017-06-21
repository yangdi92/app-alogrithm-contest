library(lightgbm)
library(readr)
library(dplyr)
setwd('D:/Ñ¸À×ÏÂÔØ/tencent/pre')

train_index<-sample.int(nrow(dt),trunc(0.8*nrow(dt)),replace = F)
test_index<-setdiff(1:nrow(dt),train_index)
train<-dt[train_index, ]
test<-dt[test_index, ]
train<-read_csv('dt1.csv')
test<-read_csv('dt2.csv')
vars_cate<-c("creativeID","positionID","connectionType","telecomsOperator","age" ,
             "gender","education","marriageStatus", "haveBaby","hometown","residence",
             "home_or_not","weekday" ,"hours")
vars_con<-setdiff(names(train),vars_cate)
vars<-c(vars_cate,vars_con)
train<-train[, vars]
test<-test[, var]
train$installed1<-as.integer(train$installed1)
test$installed1<-as.integer(test$installed1)
# valid<- train[sub,]
# train<- train[-sub,]
train_x<-train%>%select(-conversionTime,-clickTime,-label,-u_sit_n,-u_sit_n1,-u_sit_p)
train_y<-train%>% select(label)
test_x<-test%>%select(-conversionTime,-clickTime,-label,-u_sit_n,-u_sit_n1,-u_sit_p)
test_y<-test%>% select(label)
# valid_x<-valid%>%select(-conversionTime,-clickTime,-label,-u_sit_n,-u_sit_n1,-u_sit_p)
# valid_y<-valid%>% select(label)


dtrain <- lgb.Dataset(data = as.matrix(train_x),label = as.matrix(train_y))
dtest <- lgb.Dataset(data = as.matrix(test_x),label = as.matrix(test_y))
params <- list(objective = "binary",
               eval = "binary_logloss",
               nlearning_rate=0.01,
               eval_freq =1,
               verbose =1,
               feature_fraction = 0.5,
               bagging_fraction = 0.5,
               max_depth = 8,
               bagging_freq = 1,
               lambda_l1 = 1,
               is_unbalance = T)
               #categorical_feature=as.list(0,45,46,51,52,55))
valids <- list(train = dtrain, test = dtest)
model <- lgb.train(params,dtrain,nrounds = 500,valids = valids,early_stopping_rounds =20)


# pre<-predict(model,as.matrix(valid_x))
# dt<-data.frame(y=valid_y$label,x = pre)
# model_log<-glm(y~x,data = dataframe,family = binomial)
# # impo<-lgb.importance(model,0.7)
# pre <-predict(model,as.matrix(test_x))
# dt_test<-data.frame(y=test_y$label,x=pre)
# pre_lr<-predict(model_log,dt_test['pre'])
# 
# logloss<- function(p,y){
#   -mean(y*log(p+1e-8)+(1-y)*log(1-p+1e-8))
# }
# 
# loss<-logloss(pre,as.matrix(test_y))
# submisson<-read_csv('subm_features.csv')
# submisson<-submisson%>%select(-clickTime,-label)
# # pre_test <-predict(model,as.matrix(test_x))
# pre <-predict(model,as.matrix(submisson))
# # pre<-predict(bst,sub)
# mydata<-data.frame(instanceID=c(1:length(pre)),prob = pre)
