from __future__ import division
import torch
import numpy as np
import matplotlib.pyplot as plt
import warnings
warnings.filterwarnings("ignore") #suppress warnings
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
import csv
import pandas as pd
from sklearn.ensemble import RandomForestRegressor
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import RandomizedSearchCV,GridSearchCV
import math
import xgboost as xgb
from scipy.stats.stats import pearsonr
from sklearn.metrics import auc
from sklearn.metrics import roc_auc_score
from scipy.stats import chisquare


#inp = pd.read_csv('Female_aged_15-49_with_NEG_UrTest_8907_with_Av_FPro_plus_11_socioeconom_and_62_SUMS_per_day_foods_vars_ends_in_7864_cases.csv', sep=',') # Dataset produced using Confounding_analysis_Part_3_code.R - see MARK
#inp = pd.read_csv('Female_aged_15-49_with_NEG_UrTest_8907_with_Av_FPro_plus_8_socioeconom_and_62_SUMS_per_day_foods_vars_8907_cases.csv', sep=',') # Dataset produced using - see MARK Confounding_analysis_Part_3_code.R
inp = pd.read_csv('Female_aged_50_anemia_Av_FPro_plus_11_socioeconom_vars_and_62_SUMS_per_day_foods_vars_9928_cases.csv', sep=',') # Dataset produced using Confounding_analysis_Part_3_code.R - see MARK

X = inp.drop(inp.columns[0:2], axis=1)
Y = X.iloc[:,1]
X = X.drop(X.columns[1], axis=1)
y_label=Y.values.reshape(Y.shape[0],1)
YU=X.loc[:,X.apply(pd.Series.nunique) != 1]
Fe44=YU.T.drop_duplicates().T


X=Fe44


print(inp.shape)
print(X.shape)


X4=X
y_label4=y_label

def st_scaler_pd(Xtrain,Xtest): 
  sc = StandardScaler()
  sc.fit(Xtrain)
  Xcal=pd.DataFrame(sc.transform(Xtrain))
  Xval=pd.DataFrame(sc.transform(Xtest))
  return Xcal,Xval


X4r,X3r= st_scaler_pd(X4,X)

def mean_pd(X4r,X3r): 
  X4p=X4r - np.mean(X4r, axis=0)
  X3p=X3r - np.mean(X4r, axis=0)
  return X4p,X3p




X4r=pd.DataFrame(X4r)

Xcal=X4r
Xval=X3r


from sklearn.cross_decomposition import PLSRegression
from sklearn.linear_model import LogisticRegression
import math

def logit_Param_St_error(x,y):
  log1=LogisticRegression()
  log1.fit(x,y)
  Y_pred_proba=log1.predict_proba(x)
  X_design = np.hstack([np.ones((x.shape[0], 1)), x])
  V = np.diagflat(np.product(Y_pred_proba, axis=1))
  covLogit = np.linalg.inv(np.dot(np.dot(X_design.T, V), X_design))
  logitParams = np.insert(log1.coef_, 0, log1.intercept_) # The first element is intercept and isn't relevant for feature imp.
  Stand_error_of_logitParams=np.sqrt(np.diag(covLogit))
  return logitParams,Stand_error_of_logitParams



kfold=10
ncomp=7
y_label4=np.array(y_label4)
Y_pred_uk1=np.empty((len(y_label4),ncomp))
for i in range(kfold):
   X4r_train = X4r[X4r.index % kfold != i] 
   X4r_test = X4r.iloc[i::kfold, :] 
   y_label4_train=np.delete(y_label4, np.arange(i, y_label4.size, kfold))
   y_label4_test=y_label4[i::kfold]
   X4r_train,X4r_test=mean_pd(X4r_train,X4r_test)
   for x in range(1, (ncomp+1)):
      pls1 = PLSRegression(n_components=x)
      pls1.fit(X4r_train, y_label4_train)
      fac=np.linalg.inv(np.dot(pls1.x_weights_.transpose(),pls1.x_loadings_)).dot(pls1.x_weights_.transpose())
      T_train_scores=X4r_train.dot(fac.transpose())
      T_test_scores=X4r_test.dot(fac.transpose())
      log1=LogisticRegression()
      log1.fit(T_train_scores,y_label4_train)
      Y_pred=log1.predict_proba(T_test_scores)
      Y_pred_uk1[X4r_test.index,(x-1)]=Y_pred[:,1]
      

AUROC_CV1=np.empty(ncomp)
for i in range(ncomp):
  AUROC_CV1[i]= roc_auc_score(y_label4,Y_pred_uk1[:,i])


ncomp=np.argmax(AUROC_CV1)+1
pls1 = PLSRegression(n_components=ncomp)
AUROC_f=np.max(AUROC_CV1)

if ncomp==1:
  Y_pred_uk2=np.empty((len(y_label4)))
else:
  Y_pred_uk2=np.empty((len(y_label4),ncomp))

log1=LogisticRegression()
kfold=30
Pd_sum_feat_imp=pd.DataFrame(np.zeros((kfold,Xcal.shape[1])))
Pd_sum_feat_imp2=pd.DataFrame(np.zeros((kfold,Xcal.shape[1])))
for i in range(kfold):
   Xcal_train = Xcal[Xcal.index % kfold != i] 
   Xcal_test = Xcal.iloc[i::kfold, :] 
   y_label4_train=np.delete(y_label4, np.arange(i, y_label4.size, kfold))
   y_label4_test=y_label4[i::kfold]
   Xcal_train,Xval1=mean_pd(Xcal_train,Xval)
   pls1.fit(Xcal_train, y_label4_train)
   fac=np.linalg.inv(np.dot(pls1.x_weights_.transpose(),pls1.x_loadings_)).dot(pls1.x_weights_.transpose())
   T_train_scores=Xcal_train.dot(fac.transpose())
   log1.fit(T_train_scores,y_label4_train)
   Importo=np.linalg.inv(np.dot(pls1.x_weights_.transpose(),pls1.x_loadings_)).dot(pls1.x_weights_.transpose()).transpose().dot(log1.coef_.transpose()).reshape(-1)
   Pd_sum_feat_imp.iloc[i,:]=np.abs(pls1.coef_)
   Pd_sum_feat_imp2.iloc[i,:]=np.abs(Importo)


   
var_std=[]
var_mean=[]
var_stab=[]
var_std2=[]
var_mean2=[]
var_stab2=[]
for i in range(Xcal.shape[1]):
   var_std=np.append(var_std,np.round(np.std(Pd_sum_feat_imp.iloc[:,i]),decimals=10))
   var_mean=np.append(var_mean,np.round(np.mean(Pd_sum_feat_imp.iloc[:,i]),decimals=10))   
   var_stab=np.append(var_stab,(var_mean[i]/var_std[i]))   
   var_std2=np.append(var_std2,np.round(np.std(Pd_sum_feat_imp2.iloc[:,i]),decimals=10))
   var_mean2=np.append(var_mean2,np.round(np.mean(Pd_sum_feat_imp2.iloc[:,i]),decimals=10))   
   var_stab2=np.append(var_stab2,(var_mean2[i]/var_std2[i])) 


var_names=list(X4.columns)
min_stab_thr=1
max_stab_thr=var_stab2[var_stab2.argsort()][-(ncomp+2)]
maximum_stab_thr=var_stab2[var_stab2.argsort()][-1]
se=[]
if maximum_stab_thr<=min_stab_thr:
  print("Model inapplicable, All variables of stabilities at or below 1!")
else:
  br=0
  for i in range(Xcal.shape[1]):
    if var_stab2[i]>1:
      br=br+1
      se=np.append(se,i)

se_array = se.astype(int)
new_var_names=np.array(var_names)[se_array]
new_var_stab=np.array(var_stab2)[se_array]
Xcal_2=Xcal.iloc[:, se_array]
Xcal_2.columns=range(Xcal_2.columns.size)
threshold_slices=10
step=(max_stab_thr-1)/threshold_slices
thr=min_stab_thr
  
AUROC_UVEp=[]

if br<(ncomp+2):
  Xcal_3=Xcal_2
else:   
  for i in range(threshold_slices):
    thr= thr+step
    sele=[]
    for j in range(Xcal_2.shape[1]):
      if new_var_stab[j]>thr:
        sele=np.append(sele,j)
    Xcal_2a=Xcal_2.iloc[:,sele]
    Xcal_2a.columns=range(Xcal_2a.columns.size)
    if ncomp==1:
      sum=np.zeros(kfold)
    else:
      sum=np.zeros((kfold,ncomp))
    for ii in range(kfold):
      Xcal_2a_train = Xcal_2a[Xcal_2a.index % kfold != ii] #Xtrain
      Xcal_2a_test = Xcal_2a.iloc[ii::kfold, :] #Xtest
      y_label4_train=np.delete(y_label4, np.arange(ii, y_label4.size, kfold))
      y_label4_test=y_label4[ii::kfold]
      if ncomp==1:
        pls1 = PLSRegression(n_components=1)
        pls1.fit(Xcal_2a_train, y_label4_train)
        fac=np.linalg.inv(np.dot(pls1.x_weights_.transpose(),pls1.x_loadings_)).dot(pls1.x_weights_.transpose())
        T_train_scores=Xcal_2a_train.dot(fac.transpose())
        T_test_scores=Xcal_2a_test.dot(fac.transpose())
        log1.fit(T_train_scores,y_label4_train)
        Y_pred=log1.predict_proba(T_test_scores)
        Y_pred_uk2[Xcal_2a_test.index]=Y_pred[:,1]
      else:
        for x in range(1, (ncomp+1)):
          pls1 = PLSRegression(n_components=x)
          pls1.fit(Xcal_2a_train, y_label4_train)
          fac=np.linalg.inv(np.dot(pls1.x_weights_.transpose(),pls1.x_loadings_)).dot(pls1.x_weights_.transpose())
          T_train_scores=Xcal_2a_train.dot(fac.transpose())
          T_test_scores=Xcal_2a_test.dot(fac.transpose())
          log1.fit(T_train_scores,y_label4_train)
          Y_pred=log1.predict_proba(T_test_scores)[:,1]
          Y_pred_uk2[Xcal_2a_test.index,(x-1)]=Y_pred
    if ncomp==1:
      AUROC_CV2=roc_auc_score(y_label4,Y_pred_uk2)
      AUROC_UVEp=np.append(AUROC_UVEp,AUROC_CV2)
    else:
      AUROC_CV2=np.empty(ncomp)
      for i in range(ncomp):
        AUROC_CV2[i]= roc_auc_score(y_label4,Y_pred_uk2[:,i])
      AUROC_UVEp=np.append(AUROC_UVEp,np.max(AUROC_CV2))
  threshold=min_stab_thr+(1+np.argmax(AUROC_UVEp))*step
  sele=[]
  for j in range(Xcal_2.shape[1]):
    if new_var_stab[j]>threshold:
      sele=np.append(sele,j)
  Xcal_3=Xcal_2.iloc[:,sele]
  Xcal_3.columns=range(Xcal_3.columns.size)
  sele_array = sele.astype(int)
  new_var_names=np.array(new_var_names)[sele_array]
  new_var_stab=np.array(new_var_stab)[sele_array]


Y_pred_uk3=np.empty((len(y_label4),ncomp))
for i in range(kfold):
   X4r_train = Xcal_3[Xcal_3.index % kfold != i] 
   X4r_test = Xcal_3.iloc[i::kfold, :] 
   y_label4_train=np.delete(y_label4, np.arange(i, y_label4.size, kfold))
   y_label4_test=y_label4[i::kfold]
   for x in range(1, (ncomp+1)):
      pls1 = PLSRegression(n_components=x)
      pls1.fit(X4r_train, y_label4_train)
      fac=np.linalg.inv(np.dot(pls1.x_weights_.transpose(),pls1.x_loadings_)).dot(pls1.x_weights_.transpose())
      T_train_scores=X4r_train.dot(fac.transpose())
      T_test_scores=X4r_test.dot(fac.transpose())
      log1.fit(T_train_scores,y_label4_train)
      Y_pred=log1.predict_proba(T_test_scores)[:,1]
      Y_pred_uk3[X4r_test.index,(x-1)]=Y_pred
      

AUROC_CV1=np.empty(ncomp)
for i in range(ncomp):
  AUROC_CV1[i]= roc_auc_score(y_label4,Y_pred_uk3[:,i])

ncomp=np.argmax(AUROC_CV1)+1
pls1 = PLSRegression(n_components=ncomp)
AUROC_CV=np.max(AUROC_CV1)
Y_pred_uk4=Y_pred_uk3[:,ncomp-1]

Y_pred_cal=np.empty(len(y_label4))
pls1.fit(Xcal_3, y_label4)
fac=np.linalg.inv(np.dot(pls1.x_weights_.transpose(),pls1.x_loadings_)).dot(pls1.x_weights_.transpose())
T_train_scores=Xcal_3.dot(fac.transpose())
log1=LogisticRegression()
log1.fit(T_train_scores,y_label4)
Y_pred_cal=log1.predict(T_train_scores)

Y_pred_cal_proba=log1.predict_proba(T_train_scores)


Xtrain=Xcal_3
varX=np.zeros(ncomp)
for i in range(ncomp):
  Xtrain=Xtrain-np.matmul(pls1.x_scores_[:,i].reshape(pls1.x_scores_[:,i].shape[0],1),pls1.x_loadings_[:,i].reshape(1,pls1.x_loadings_[:,i].shape[0]))
  varX[i]=1-np.sum(np.diagonal(np.matmul(Xtrain.transpose(),Xtrain)))/np.sum(np.diagonal(np.matmul(Xcal_3.transpose(),Xcal_3)))

Importan=np.linalg.inv(np.dot(pls1.x_weights_.transpose(),pls1.x_loadings_)).dot(pls1.x_weights_.transpose()).transpose().dot(log1.coef_.transpose()).reshape(-1)

Abs_Importan=abs(Importan)
Logistic_PLS_comps_coefs=log1.coef_
PLS_regression_coefficients=pls1.coef_

logitParams2,Stand_error_of_logitParams2=logit_Param_St_error(T_train_scores,y_label4)


ja=np.repeat(divmod(Xcal_3.shape[0],kfold)[0],kfold,axis=0)
for j in range(divmod(Xcal_3.shape[0],kfold)[1]):
  ja[j]=ja[j]+1

ja=ja[::-1]
jab=np.cumsum(ja)
Y_pred_uk_log_Bl=np.empty(len(y_label4))
Y_pred_proba_uk_log_Bl=np.empty([len(y_label4),2])
for i in range(kfold):
   if i==0:
      Xcal_test = Xcal_3[0:ja[i]]
      Xcal_train = Xcal_3.drop(Xcal_3.index[np.arange(0, ja[i], 1)])
      y_label4_train=np.delete(y_label4,np.arange(0, ja[i], 1))
      y_label4_test=y_label4[0:ja[i]]
      Xcal_train,Xcal_test=mean_pd(Xcal_train,Xcal_test)
      pls1.fit(Xcal_train, y_label4_train)
      fac=np.linalg.inv(np.dot(pls1.x_weights_.transpose(),pls1.x_loadings_)).dot(pls1.x_weights_.transpose())
      T_train_scores=Xcal_train.dot(fac.transpose())
      T_test_scores=Xcal_test.dot(fac.transpose())
      log1=LogisticRegression()
      log1.fit(T_train_scores,y_label4_train)
      Y_pred = log1.predict(T_test_scores)
      Y_pred_prob_Bl=log1.predict_proba(T_test_scores)
      Y_pred_uk_log_Bl[Xcal_test.index]=Y_pred
      Y_pred_proba_uk_log_Bl[Xcal_test.index]=Y_pred_prob_Bl
   else:
      Xcal_test = Xcal_3[jab[i-1]:jab[i]]
      Xcal_train = Xcal_3.drop(Xcal_3.index[np.arange(jab[i-1], jab[i], 1)])
      y_label4_train=np.delete(y_label4,np.arange(jab[i-1], jab[i], 1))
      y_label4_test=y_label4[jab[i-1]:jab[i]]
      Xcal_train,Xcal_test=mean_pd(Xcal_train,Xcal_test)
      pls1.fit(Xcal_train, y_label4_train)
      fac=np.linalg.inv(np.dot(pls1.x_weights_.transpose(),pls1.x_loadings_)).dot(pls1.x_weights_.transpose())
      T_train_scores=Xcal_train.dot(fac.transpose())
      T_test_scores=Xcal_test.dot(fac.transpose())
      log1=LogisticRegression()
      log1.fit(T_train_scores,y_label4_train)
      Y_pred = log1.predict(T_test_scores)
      Y_pred_prob_Bl=log1.predict_proba(T_test_scores)
      Y_pred_uk_log_Bl[Xcal_test.index]=Y_pred
      Y_pred_proba_uk_log_Bl[Xcal_test.index]=Y_pred_prob_Bl



BalAcc_CV_ar=np.zeros(400)
precision_CV_ar=np.zeros(400)
recall_CV_ar=np.zeros(400)
specificity_CV_ar=np.zeros(400)
accuracy_CV_ar=np.zeros(400)
F1_CV_ar=np.zeros(400)
TP_CV_ar=np.zeros(400)
TN_CV_ar=np.zeros(400)
FP_CV_ar=np.zeros(400)
FN_CV_ar=np.zeros(400)
for i in range(len(BalAcc_CV_ar)):
  threshold=0.001+(i/len(BalAcc_CV_ar))
  Y_pred_uk_log_acc_thres=np.zeros(len(y_label4))
  for j in range(len(y_label4)):
    if Y_pred_uk4[j]>threshold:
      Y_pred_uk_log_acc_thres[j]=1
  for j in range(len(y_label4)):
    if (y_label4[j]==Y_pred_uk_log_acc_thres[j]):
      if y_label4[j]==1:
        TP_CV_ar[i]=TP_CV_ar[i]+1
      else:
        TN_CV_ar[i]=TN_CV_ar[i]+1
    else:
      if y_label4[j]==1:
        FN_CV_ar[i]=FN_CV_ar[i]+1
      else:
        FP_CV_ar[i]=FP_CV_ar[i]+1
  BalAcc_CV_ar[i]=0.5*((TP_CV_ar[i]/(TP_CV_ar[i]+FN_CV_ar[i]))+(TN_CV_ar[i]/(TN_CV_ar[i]+ FP_CV_ar[i])))
  precision_CV_ar[i]=TP_CV_ar[i]/(TP_CV_ar[i]+FP_CV_ar[i])
  recall_CV_ar[i]=TP_CV_ar[i]/(TP_CV_ar[i]+FN_CV_ar[i])
  specificity_CV_ar[i]=TN_CV_ar[i]/(TN_CV_ar[i]+FP_CV_ar[i])
  accuracy_CV_ar[i]=(TP_CV_ar[i]+TN_CV_ar[i])/len(y_label4)
  F1_CV_ar[i]=2*TP_CV_ar[i]/(2*TP_CV_ar[i]+(FP_CV_ar[i]+FN_CV_ar[i]))


threshold=0.001+(F1_CV_ar.argmax()/len(BalAcc_CV_ar))

F1_CV=F1_CV_ar.max()
BalAcc_CV=BalAcc_CV_ar[F1_CV_ar.argmax()]
precision_CV=precision_CV_ar[F1_CV_ar.argmax()]
recall_CV=recall_CV_ar[F1_CV_ar.argmax()]
specificity_CV=specificity_CV_ar[F1_CV_ar.argmax()]
accuracy_CV=accuracy_CV_ar[F1_CV_ar.argmax()]
TP_CV=TP_CV_ar[F1_CV_ar.argmax()]
TN_CV=TN_CV_ar[F1_CV_ar.argmax()]
FP_CV=FP_CV_ar[F1_CV_ar.argmax()]
FN_CV=FN_CV_ar[F1_CV_ar.argmax()]


TP_TR=0; TN_TR=0; FP_TR=0; FN_TR=0;
prediction_train_log_thres=np.zeros(len(y_label4))
for j in range(len(y_label4)):
  if Y_pred_cal_proba[j,1]>threshold:
    prediction_train_log_thres[j]=1

for j in range(len(y_label4)):
  if (y_label4[j]==prediction_train_log_thres[j]):
    if y_label4[j]==1:
      TP_TR=TP_TR+1
    else:
      TN_TR=TN_TR+1
  else:
    if y_label4[j]==1:
      FN_TR=FN_TR+1
    else:
      FP_TR=FP_TR+1


BalAcc_TR=0.5*((TP_TR/(TP_TR+FN_TR))+(TN_TR/(TN_TR+FP_TR)))
precision_TR=TP_TR/(TP_TR+FP_TR)
recall_TR=TP_TR/(TP_TR+FN_TR)
specificity_TR=TN_TR/(TN_TR+FP_TR)
accuracy_TR=(TP_TR+TN_TR)/len(y_label4)
F1_TR=2*TP_TR/(2*TP_TR+(FP_TR+FN_TR))

TP_CV_Bl=0; TN_CV_Bl=0; FP_CV_Bl=0; FN_CV_Bl=0;
Y_pred_uk_log_acc_thres_Bl=np.zeros(len(y_label4))
for j in range(len(y_label4)):
  if Y_pred_proba_uk_log_Bl[j,1]>threshold:
    Y_pred_uk_log_acc_thres_Bl[j]=1

for j in range(len(y_label4)):
  if (y_label4[j]==Y_pred_uk_log_acc_thres_Bl[j]):
    if y_label4[j]==1:
      TP_CV_Bl=TP_CV_Bl+1
    else:
      TN_CV_Bl=TN_CV_Bl+1
  else:
    if y_label4[j]==1:
      FN_CV_Bl=FN_CV_Bl+1
    else:
      FP_CV_Bl=FP_CV_Bl+1


BalAcc_CV_Bl=0.5*((TP_CV_Bl/(TP_CV_Bl+FN_CV_Bl))+(TN_CV_Bl/(TN_CV_Bl+FP_CV_Bl)))
precision_CV_Bl=TP_CV_Bl/(TP_CV_Bl+FP_CV_Bl)
recall_CV_Bl=TP_CV_Bl/(TP_CV_Bl+FN_CV_Bl)
specificity_CV_Bl=TN_CV_Bl/(TN_CV_Bl+FP_CV_Bl)
accuracy_CV_Bl=(TP_CV_Bl+TN_CV_Bl)/len(y_label4)
F1_CV_Bl=2*TP_CV_Bl/(2*TP_CV_Bl+(FP_CV_Bl+FN_CV_Bl))

AUROC_CV=roc_auc_score(y_label4,Y_pred_uk4)
AUROC_CV_Bl=roc_auc_score(y_label4,Y_pred_proba_uk_log_Bl[:,1])
AUROC_TR=roc_auc_score(y_label4,Y_pred_cal_proba[:,1])


from scipy.stats import chisquare

if round(BalAcc_CV*len(y_label4))+round((1-BalAcc_CV)*len(y_label4))==len(y_label4):
  CHI_CV=chisquare(f_obs=[round(BalAcc_CV*len(y_label4)),round((1-BalAcc_CV)*len(y_label4))],f_exp=[divmod(len(y_label4),2)[0],(divmod(len(y_label4),2)[0]+divmod(len(y_label4),2)[1])])
else:
  CHI_CV=chisquare(f_obs=[round(BalAcc_CV*len(y_label4))+1,round((1-BalAcc_CV)*len(y_label4))],f_exp=[divmod(len(y_label4),2)[0],(divmod(len(y_label4),2)[0]+divmod(len(y_label4),2)[1])])


if round(BalAcc_CV_Bl*len(y_label4))+round((1-BalAcc_CV_Bl)*len(y_label4))==len(y_label4):
  CHI_CV_Bl=chisquare(f_obs=[round(BalAcc_CV_Bl*len(y_label4)),round((1-BalAcc_CV_Bl)*len(y_label4))],f_exp=[divmod(len(y_label4),2)[0],(divmod(len(y_label4),2)[0]+divmod(len(y_label4),2)[1])])
else:
  CHI_CV_Bl=chisquare(f_obs=[round(BalAcc_CV_Bl*len(y_label4))+1,round((1-BalAcc_CV_Bl)*len(y_label4))],f_exp=[divmod(len(y_label4),2)[0],(divmod(len(y_label4),2)[0]+divmod(len(y_label4),2)[1])])



if round(BalAcc_TR*len(y_label4))+round((1-BalAcc_TR)*len(y_label4))==len(y_label4):
  CHI_TR=chisquare(f_obs=[round(BalAcc_TR*len(y_label4)),round((1-BalAcc_TR)*len(y_label4))],f_exp=[divmod(len(y_label4),2)[0],(divmod(len(y_label4),2)[0]+divmod(len(y_label4),2)[1])])
else:
  CHI_TR=chisquare(f_obs=[round(BalAcc_TR*len(y_label4))+1,round((1-BalAcc_TR)*len(y_label4))],f_exp=[divmod(len(y_label4),2)[0],(divmod(len(y_label4),2)[0]+divmod(len(y_label4),2)[1])])


absrez=np.abs(Y_pred_uk4-y_label4)


kfold=30
Pd_sum_feat_imp=pd.DataFrame(np.zeros((kfold,Xcal_3.shape[1])))
Pd_sum_feat_imp2=pd.DataFrame(np.zeros((kfold,Xcal_3.shape[1])))
for i in range(kfold):
   Xcal_train = Xcal_3[Xcal_3.index % kfold != i] 
   Xcal_test = Xcal_3.iloc[i::kfold, :] 
   y_label4_train=np.delete(y_label4, np.arange(i, y_label4.size, kfold))
   y_label4_test=y_label4[i::kfold]
   Xcal_train,Xval1=mean_pd(Xcal_train,Xval)
   pls1.fit(Xcal_train, y_label4_train)
   fac=np.linalg.inv(np.dot(pls1.x_weights_.transpose(),pls1.x_loadings_)).dot(pls1.x_weights_.transpose())
   T_train_scores=Xcal_train.dot(fac.transpose())
   log1=LogisticRegression()
   log1.fit(T_train_scores,y_label4_train)
   Importo=np.linalg.inv(np.dot(pls1.x_weights_.transpose(),pls1.x_loadings_)).dot(pls1.x_weights_.transpose()).transpose().dot(log1.coef_.transpose()).reshape(-1)
   Pd_sum_feat_imp.iloc[i,:]=np.abs(pls1.coef_)
   Pd_sum_feat_imp2.iloc[i,:]=np.abs(Importo)


   
var_std=[]
var_mean=[]
var_stab=[]
var_std2=[]
var_mean2=[]
var_stab2=[]
for i in range(Xcal_3.shape[1]):
   var_std=np.append(var_std,np.round(np.std(Pd_sum_feat_imp.iloc[:,i]),decimals=10))
   var_mean=np.append(var_mean,np.round(np.mean(Pd_sum_feat_imp.iloc[:,i]),decimals=10))   
   var_stab=np.append(var_stab,(var_mean[i]/var_std[i]))   
   var_std2=np.append(var_std2,np.round(np.std(Pd_sum_feat_imp2.iloc[:,i]),decimals=10))
   var_mean2=np.append(var_mean2,np.round(np.mean(Pd_sum_feat_imp2.iloc[:,i]),decimals=10))   
   var_stab2=np.append(var_stab2,(var_mean2[i]/var_std2[i])) 



CI_95_halfmargin=(1.96*var_std/np.sqrt(kfold))


print("Accuracy(train) = ",accuracy_TR)
print("Accuracy(CV) = ",accuracy_CV)

print("Balanced accuracy(train) = ",BalAcc_TR)
print("Balanced accuracy(CV) = ",BalAcc_CV)

print("Precision(train) = ",precision_TR)
print("Precision(CV) = ",precision_CV)

print("Recall(train) = ",recall_TR)
print("Recall(CV) = ",recall_CV)

print("Specificty(train) = ",specificity_TR)
print("Specificity(CV) = ",specificity_CV)

print("Dice (i.e. F1 score) (train) = ",F1_TR)
print("Dice (i.e. F1 score) (CV) = ",F1_CV)

print("AUROC(train) = ",AUROC_TR)
print("AUROC(CV) = ",AUROC_CV)
print("Determined binary (0/1) classification threshold via argmax(F1_score(CV)) = ",threshold)

print("                                                       ")
print("            Predicted CV")
print("               P      N ")
print("Actual    P  ",   TP_CV.astype('int'),"   ", FN_CV.astype('int') ," as TP    FN")
print("                                                       ")
print("Class     N  ",  FP_CV.astype('int'),"   ", TN_CV.astype('int') ," as FP    TN") 
print("                                                       ")
print("                                                       ")
print("            Predicted TRAIN")
print("               P      N ")
print("Actual    P  ",   TP_TR,"   ", FN_TR ," as TP    FN")
print("                                                       ")
print("Class     N  ",  FP_TR,"   ", TN_TR ," as FP    TN") 
print("                                                       ")
print("                                                       ")

print("Selected variable names are: ",new_var_names)   ### MARK for FINAL_results_Genereal_Confounding_variable_analysis_Part_4_code_and_Final_results
print("Selected variables positions are: ",list(range(len(new_var_names))))
print("Number of PLS components:",ncomp)
print("Logistic regression coefficients of PLS components [Logistic_PLS_comps_coefs]:",Logistic_PLS_comps_coefs)
print("Intercept (1st elem.) of PLS comp coef: ", logitParams2[0])
print("Standard errors of Intercept and Logistic PLS comp coef: ", Stand_error_of_logitParams2)
print("PLS regression coefficient vector:",PLS_regression_coefficients)
print("Explained variance in X:",varX)
print("Variable importances calculated as PLS-Log regression coefficients [Logistic_PLS_comps_coefs x INV(W(t)xL)xW(t), W,L are PLS x-Weights and x-Loadings matrices (t-transpose)]: ",Importan)
print("Correlation between PLS regression vector and Logistic regression vector:",np.corrcoef(PLS_regression_coefficients,Importan)[0,1])
print("Variable stabilities as mean/stdev of PLS-Log regression coefficients: ",var_stab2)
print("Training set dimensions: ", Xcal_3.shape)
print("Training number of positives: ", np.sum(y_label4))
print("Absolute difference 95% prediction half-margin in binary probability scale: ",np.percentile(absrez,95))
print("Absolute difference 80% prediction half-margin in binary probability scale: ",np.percentile(absrez,80))
print("Chisquare test significance for the CV of the training set based on balanced accuracy (50% margin)): ",CHI_CV)
print("In addition to Sequential kfold-CV given above, Block-wise kfold-CV result in terms of Balanced accuracy = ", BalAcc_CV_Bl)
print("In addition to Sequential kfold-CV given above, Block-wise kfold-CV result in terms of Accuracy = ", accuracy_CV_Bl)
print("In addition to Sequential kfold-CV given above, Block-wise kfold-CV result in terms of F1 score = ", F1_CV_Bl)
print("In addition to Sequential kfold-CV given above, Block-wise kfold-CV result in terms of TP,TN,FP,FN = ",TP_CV_Bl,TN_CV_Bl,FP_CV_Bl,FN_CV_Bl)


