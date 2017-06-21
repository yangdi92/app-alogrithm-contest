# -*- coding: utf-8 -*-
"""
Created on Sat Jun  3 17:20:03 2017

@author: yangdi
"""
import pandas as pd
# import numpy as np
from scipy import sparse
from sklearn.preprocessing import OneHotEncoder
from sklearn.linear_model import LogisticRegression
#from sklearn.model_selection import train_test_split
import scipy as sp
def logloss(act, pred):
  epsilon = 1e-15
  pred = sp.maximum(epsilon, pred)
  pred = sp.minimum(1-epsilon, pred)
  ll = sum(act*sp.log(pred) + sp.subtract(1,act)*sp.log(sp.subtract(1,pred)))
  ll = ll * -1.0/len(act)
  return ll
data_root = r"D:\迅雷下载\tencent\pre06"
dfTrain = pd.read_csv("%s/dftrain.csv"%data_root)
dfTest = pd.read_csv("%s/dftest.csv"%data_root)
dfSubm = pd.read_csv("%s/subm.csv"%data_root)
# process data
y_train = dfTrain["label"].values
y_test = dfTest["label"].values
# feature engineering/encoding
enc = OneHotEncoder()
feats = [ "creativeID","adID", "camgaignID", "advertiserID", 
         "appID", "appPlatform","connectionType", "positionID",
         "telecomsOperator","age","gender","education","marriageStatus",
         "haveBaby","hometown","residence","sitesetID","positionType",
         "appCategory","home_or_not","weekday","hours"]
for i,feat in enumerate(feats):
    x_train = enc.fit_transform(dfTrain[feat].values.reshape(-1, 1))
    x_test = enc.transform(dfTest[feat].values.reshape(-1, 1))
    x_Subm = enc.transform(dfSubm[feat].values.reshape(-1, 1))
    if i == 0:
        X_train, X_test, X_Subm = x_train, x_test, x_Subm
    else:
        X_train, X_test, X_Subm = sparse.hstack((X_train, x_train)), sparse.hstack((X_test, x_test)),sparse.hstack((X_Subm, x_Subm))

# model training
lr = LogisticRegression(class_weight='balanced')
lr.fit(X_train, y_train)
proba_test = lr.predict_proba(X_test)[:,1]
lr_ca = LogisticRegression()
lr_ca.fit(proba_test.reshape(749906,1),y_test)
proba_test_ca = lr_ca.predict_proba(proba_test.reshape(749906,1))[:,1]
loss = logloss(y_test,proba_test_ca)
# submission
prob = lr.predict_proba(X_Subm)[:,1]
prob_ca = lr_ca.predict_proba(prob.reshape(338489,1))[:,1]
df = pd.DataFrame({"instanceID": dfSubm["instanceID"].values, "prob": prob_ca})
df.sort_values("instanceID", inplace=True)
df.to_csv("%s/submission.csv"%data_root, index=False)



