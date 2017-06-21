library(lightgbm)
library(readr)
library(dplyr)
setwd('D:/Ñ¸À×ÏÂÔØ/tencent/pre')
path<-paste(paste('dt',c(1:7),'.csv',sep=''))
subm<-read_csv('subm_features.csv')
vars_cate<-c("creativeID","positionID","connectionType","telecomsOperator","age" ,
             "gender","education","marriageStatus", "haveBaby","hometown","residence",
             "home_or_not","weekday" ,"hours")
vars_con<-setdiff(names(subm),vars_cate)
vars<-c(vars_cate,vars_con)
subm<-subm[, vars]

subm$installed1<-as.integer(subm$installed1)
subm<-subm%>%select(-instanceID,-userID,-clickTime,-label,-u_sit_n,-u_sit_n1,-u_sit_p)

pre_df<-as.data.frame(matrix(0,nrow(subm),7))
test_error<-list()

i=1 
dt<-read_csv(path[i])
set.seed(123)
train_index<-sample.int(nrow(dt),trunc(0.8*nrow(dt)),replace = F)
test_index<-setdiff(1:nrow(dt),train_index)
train<-dt[train_index, ]
test<-dt[test_index, ]
vars_cate<-c("creativeID","positionID","connectionType","telecomsOperator","age" ,
             "gender","education","marriageStatus", "haveBaby","hometown","residence",
             "home_or_not","weekday" ,"hours")
vars_con<-setdiff(names(train),vars_cate)
vars<-c(vars_cate,vars_con)
train<-train[, vars]
test<-test[, vars]
train$installed1<-as.integer(train$installed1)
test$installed1<-as.integer(test$installed1)
train_x<-train%>%select(-userID,-conversionTime,-clickTime,-label,-u_sit_n,-u_sit_n1,-u_sit_p)
train_y<-train%>% select(label)
test_x<-test%>%select(-userID,-conversionTime,-clickTime,-label,-u_sit_n,-u_sit_n1,-u_sit_p)
test_y<-test%>% select(label)
# valid_x<-valid%>%select(-conversionTime,-clickTime,-label,-u_sit_n,-u_sit_n1,-u_sit_p)
# valid_y<-valid%>% select(label)
rm(train,test);gc()
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
               is_unbalance = F,
               categorical_feature=as.list(1:14))
valids <- list(train = dtrain, test = dtest)
model <- lgb.train(params,dtrain,nrounds = 500,valids = valids,early_stopping_rounds =20)
test_error[i]<-model$eval_valid()
pre_test<-predict(model,as.matrix(test_x))
df_test<-data.frame(y=test_y$label,x=pre_test)
lr<-glm(y~x,data = df_test)
pre<-predict(model,as.matrix(subm))
df_subm<-data.frame(x=pre)
pre_ca<-predict.glm(lr,df_subm[1],type = 'response')
pre_df[, i]<-pre_ca

rm(train_x,train_y,test_x,test_y,model,dtest,dtrain);gc()

write_csv(pre_df,'ensemble.csv')
write(test_error,'testerror.txt')
weight<-NULL
for(i in 1:7){
  weight[i]<- test_error[[i]]$value
}

weight_norm<-weight/sum(weight)

pre<-apply(pre_df*weight,1,sum)

sf<-data.frame(instanceID=c(1:length(pre)),prob=pre)
write_csv(sf,'submission.csv')
