import matplotlib
import operator
%matplotlib inline
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from tqdm import tqdm
import joblib
import scipy
from scipy.optimize import linesearch
from scipy.optimize.linesearch import line_search_wolfe2

# Set 
matplotlib.rcParams['font.serif'] = 'Times New Roman'
matplotlib.rcParams['font.family'] = "serif"
matplotlib.rcParams['font.size'] = 10

#import custom_functions
from functions_statistical_performance import *

reproduceflag=True



def ClassifyDB(db, model_per_fold,nut_sel):
    dbsel=db.loc[:,nut_sel]
    Xnut=dbsel.values 
    indfold=0
    for model in model_per_fold:
        indfold+=1
        y_pred = model.predict(Xnut)
        y_probs= model.predict_proba(Xnut)
        db['classf'+str(indfold)]=y_pred
        db['p1f'+str(indfold)]=y_probs[:,0]
        db['p2f'+str(indfold)]=y_probs[:,1]
        db['p3f'+str(indfold)]=y_probs[:,2]
        db['p4f'+str(indfold)]=y_probs[:,3]
        db['FProf'+str(indfold)]=(1-db['p1f'+str(indfold)]+db['p4f'+str(indfold)])/2  
    for p in range(1,5):
        db['p'+str(p)]=db.loc[:, ['p'+str(p)+'f1', 'p'+str(p)+'f2', 'p'+str(p)+'f3','p'+str(p)+'f4', 'p'+str(p)+'f5' ]].mean(axis=1)
        db['std_p'+str(p)]=db.loc[:, ['p'+str(p)+'f1', 'p'+str(p)+'f2', 'p'+str(p)+'f3','p'+str(p)+'f4', 'p'+str(p)+'f5' ]].std(axis=1)
    db['FPro']=db.loc[:, ['FProf1', 'FProf2', 'FProf3','FProf4', 'FProf5' ]].mean(axis=1)
    db['std_FPro']=db.loc[:, ['FProf1', 'FProf2', 'FProf3','FProf4', 'FProf5']].std(axis=1)
    db['min_FPro']=db.loc[:, ['FProf1', 'FProf2', 'FProf3','FProf4', 'FProf5']].min(axis=1)
    db['max_p']=db.loc[:, ['p1', 'p2', 'p3','p4' ]].idxmax(axis=1)
    db['class']=[int(s[1])-1 for s in db.loc[:, ['p1', 'p2', 'p3','p4' ]].idxmax(axis=1)]
    db['min_in_which_fold']=db.loc[:, ['FProf1', 'FProf2', 'FProf3','FProf4', 'FProf5']].idxmin(axis=1)
    db['min_fold_id']=[int(s[-1]) for s in db.loc[:, ['FProf1', 'FProf2', 'FProf3','FProf4', 'FProf5']].idxmin(axis=1)]
    db['min_class']=[db['classf'+str(db['min_fold_id'].iloc[n])].iloc[n] for n in range(db.shape[0])]    
    for ind in range(1,5):
        db['p'+str(ind)+'_minFPro']=[db['p'+str(ind)+'f'+str(db['min_fold_id'].iloc[n])].iloc[n] for n in range(db.shape[0])]
    return db



RFFNDDS=pd.read_csv('./input_data/FNDDS_2009-10_Training_Data.csv') # from git clone of https://github.com/menicgiulia/MLFoodProcessing and then downloaded
RFFNDDS
nl=list(RFFNDDS.columns)
nl=nl[0:-7]
len(nl)


RFFNDDSSR=RFFNDDS.loc[:, 'Protein': 'Fatty acids, total polyunsaturated']
StandardNutPanel=list(RFFNDDSSR.columns)
RFFNDDSSR['pythonlabel']=RFFNDDS['pythonlabel'].copy()

if reproduceflag:
    model_per_fold62nut=joblib.load("./paper_classifiers_outcomes/FNDDS0910_62Nutrients_5folds_SMOTE.pkl") # This file has to be also git downloaded from https://github.com/menicgiulia/MLFoodProcessing/tree/main/paper_classifiers_outcomes
else:
    data2train=RFFNDDSSR[RFFNDDSSR.pythonlabel!=-1]
    X=data2train.loc[:, 'Protein': 'Fatty acids, total polyunsaturated']
    y=data2train['pythonlabel']
    smoteflag=True
    (performancesAUC62nut, performancesAUP62nut, splits62nut, model_per_fold62nut)=AUCAUPkfold(X,y,smoteflag)

RFFNDDS20172018=pd.read_csv('./input_data/X62_set_2007-2008.csv') #Produced from "calculation_of_Fpro.R" file
RFFNDDS20172018 = RFFNDDS20172018.drop(RFFNDDS20172018.columns[0], axis=1)
df=ClassifyDB(RFFNDDS20172018,model_per_fold62nut,StandardNutPanel)
df.to_csv("./output_data/A_FNDDS_2007-08_62Nutrients_ensemble_5folds.csv")

# The similar was done for the rest eight dataset with 2015-16 already produced by the reference.

