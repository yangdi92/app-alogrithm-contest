library(xgboost)
library(readr)
library(dplyr)
setwd('D:/я╦ювобть/tencent/pre')
dt<-read_csv('dt1.csv')
train<-read_csv('dt1.csv')
test<-read_csv('dt3.csv')
# train_index<-sample.int(nrow(dt),trunc(0.8*nrow(dt)),replace = F)
# test_index<-setdiff(1:nrow(dt),train_index)
# train<-dt[train_index, ]
# test<-dt[test_index, ]
train_x<-train%>%select(-conversionTime,-clickTime,-label)
train_y<-train%>% select(label)
test_x<-test%>%select(-conversionTime,-clickTime,-label)
test_y<-test%>% select(label)

dtrain <- xgb.DMatrix(data = as.matrix(train_x),label = as.matrix(train_y),missing = NA)
dtest <- xgb.DMatrix(data = as.matrix(test_x),label = as.matrix(test_y),missing = NA)
watchlist <- list(eval = dtest,train = dtrain)

param <- list(max_depth = 6, 
              eta = 0.05, 
              silent = 1, 
              nthread = 2,
              verbose = 1,
              subsample = 0.5,
              colsample_bytree = 0.5,
              missing = NA,
              gamma = 1,
              max_delta_step = 1,
              objective = "binary:logistic", 
              eval_metric = "logloss")
bst <- xgb.train(param, dtrain, nrounds = 200, watchlist)

# xgb.save(bst,'xgb_10498')
# bst1<-xgb.load('xgb_10498')  
submisson<-read_csv('subm_features.csv')
submisson<-submisson%>%select(-clickTime,-label)
sub<- xgb.DMatrix(as.matrix(submisson),missing = NA)
pre<-predict(bst,sub)
mydata<-data.frame(instanceID=c(1:length(pre)),prob = pre)

write_csv(mydata,'submission.csv')
