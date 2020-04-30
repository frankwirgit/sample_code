# An example for depression analysis based on model of Randomforest Classification


import pandas as pd
import numpy as np
from pandas import *
from numpy import *
from pandas import ExcelWriter

#define un-related variables
#they are pre-selected during the kneepain study
unrelated_var = ['alastvrb','wtia_sa','jntchr','sprflu2', 'arx12_5', 'livyr', 
'ephev', 'ahcafyr2','kidwkyr', 'angev', 'ulcev', 'canev',
'hybpcktp','sinyr','strat_p','hrtest','psu_p','che','proxysa','rectype']


#read in sample adult file
dfd = pandas.read_stata('Data\samadult2014.dta', encoding= 'utf-8')
dfd=dfd.drop(unrelated_var,1)

#read in the functioning disability file
dff = pandas.read_stata('Data\sfuncdisb2014.dta', encoding= 'utf-8')
#drop co-existing variables from fundis file
dff=dff.drop(['strat_p', 'psu_p', 'rectype','fdrn_flg','intv_qrt','intv_mon'],1)

#merge funcdis file and samadult file
#by keys of survey year, hhx, fmx, fpx
db = pd.merge(dfd, dff, how='inner', on=["hhx","fmx","fpx","srvy_yr"])
#drop merging keys after merge
db = db.drop(['hhx','fmx','fpx','srvy_yr'],1)


#prepare descriptive stats analysis
da = db.copy()
#drop variables with NA cases above threshold
da=da.dropna(thresh=len(da) - 1000, axis=1) #808 -> 213
#group target variable dep_1
da['dep']=0
da.loc[(da.dep_1<4), 'dep'] = 1
da.loc[(da['dep_1']==4) | (da['dep_1']==5), 'dep'] = 2
da.loc[(da['dep_1']>=7), 'dep'] = 3
da=da.drop(['dep_1','dep_2','anx_1','fla1ar','rcs_afd','lateinta'],1)

da["age_grp"]=0
da.loc[(da.age_p<20), 'age_grp'] = 1
da.loc[(da['age_p']>=20) & (da['age_p']<30), 'age_grp'] = 2
da.loc[(da['age_p']>=30) & (da['age_p']<40), 'age_grp'] = 3
da.loc[(da.age_p>=40) & (da.age_p<50), 'age_grp'] = 4
da.loc[(da['age_p']>=50) & (da['age_p']<60), 'age_grp'] = 5
da.loc[(da['age_p']>=60) & (da['age_p']<70), 'age_grp'] = 6
da.loc[(da.age_p>=70) & (da.age_p<80), 'age_grp'] = 7
da.loc[(da.age_p>=80) & (da.age_p<85), 'age_grp'] = 8
da.loc[da['age_p']>=85, 'age_grp'] = 9
#da['age_grp'].value_counts()
da=da.drop(['age_p'],1)

da["bmi_grp"]=0
da.loc[(da.bmi<25), 'bmi_grp'] = 1
da.loc[(da['bmi']>=25) & (da['bmi_grp']<30), 'bmi_grp'] = 2
da.loc[(da['bmi']>=30) & (da['bmi_grp']<35), 'bmi_grp'] = 3
da.loc[(da['bmi']>=35) & (da['bmi_grp']<40), 'bmi_grp'] = 4
da.loc[da['bmi']>=40, 'bmi_grp'] = 5
da['bmi_grp'].value_counts()
da=da.drop(['bmi'],1)

da['hispan']=0
da.loc[(da.hispan_i<12),'hispan']=1
da.hispan.unique()
da=da.drop(['hispan_i'],1)

#lateinta
#regroup var related to the communication with doctor
#ahcsyr4, ahcsyr3, hit4a, hit5a, aprvtryr, 
#ahcafyr5, ahcafyr6, adrnai, adrnanp
#print da['fla1ar'].value_counts()

#default is set to be not see doctor
da['see_doc']=2
#set those ones who have seen doctors
da.loc[(da.ahcsyr4==1)|(da.ahcsyr3==1)|(da.hit5a==1)|(da.hit4a==1)|(da.aprvtryr==2),'see_doc']=1
#set those ones whose answer are unknown
da.loc[(da.ahcsyr4>=7)|(da.ahcsyr3>=7)|(da.hit5a>=7)|(da.hit4a>=7)|(da.aprvtryr>=7),'see_doc']=3
da= da.drop(['ahcsyr4','ahcsyr3','hit4a', 'hit5a', 'aprvtryr'],1)
#print da['see_doc'].value_counts()

#default health insurance or care to be available
da['health_plan']=1
da.loc[(da.ahcafyr6==1)|(da.adrnai==1)|(da.adrnanp==1)|(da.ahcafyr5==1),'health_plan']=2
da.loc[(da.ahcafyr6>=7)|(da.adrnai>=7)|(da.adrnanp>=7)|(da.ahcafyr5>=7),'health_plan']=3
da= da.drop(['ahcafyr6','adrnai','adrnanp','ahcafyr5'],1)
#print da['health_plan'].value_counts()

#combine vision issue default as 2, no vision issue
da['vision_issue']=2
da.loc[((da.vis_ss2>=2)&(da.vis_ss2<=4))|(da.avision==1),'vision_issue']=1
da.loc[(da.vis_ss2>=7)|(da.avision>=7),'vision_issue']=3
da= da.drop(['vis_ss2','avision'],1)

da=da.dropna(axis=1,how='all')


#run feature selection
#import moduels

from sklearn.linear_model import LogisticRegression
from sklearn.feature_selection import RFE
from sklearn.ensemble import RandomForestClassifier
from sklearn import cross_validation, metrics
import matplotlib.pylab as plt


def gettargetlist(df):
    depress=df['dep'].values
    targetlist=[]
    for i in xrange(len(depress)):
        if (depress[i]==1):
            targetlist.append(1)
        else:
            targetlist.append(0) 
    return targetlist


def recursivefeatureselect(df,X,targetlist, feature_number):
    model = LogisticRegression()
    rfe = RFE(model, feature_number)
    rfe = rfe.fit(X, targetlist)
    #print rfe.support_ rfe.n_features_ =31
    #print rfe.ranking_
    featureslist = []
    rankinglist=[]
    for i in xrange(len(df.columns)):
        if rfe.support_[i] == True:
            #print str(df.columns[i])
            featureslist.append(str(df.columns[i]))
            rankinglist.append(rfe.ranking_[i])
    return [x for (y,x) in sorted(zip(rankinglist,featureslist))]
    

def classifywithRandomforest(test,train,predictors,cv_folds=5):
    #cv_folds=5
    clf = RandomForestClassifier(n_estimators=1000)
    clf.fit(train[predictors], train['target'])  
    predictions = clf.predict(test[predictors])
    predprob = clf.predict_proba(test[predictors])[:, 1]
    """
RandomForestClassifier(bootstrap=True, class_weight=None, criterion='gini',
            max_depth=None, max_features='auto', max_leaf_nodes=None,
            min_samples_leaf=1, min_samples_split=2,
            min_weight_fraction_leaf=0.0, n_estimators=1000, n_jobs=1,
            oob_score=False, random_state=None, verbose=0,
            warm_start=False)
""" 

    #Perform cross-validation:
    cv_score = cross_validation.cross_val_score(clf, train[predictors], train['target'], cv=cv_folds, scoring='roc_auc')
    print "Accuracy : %.4g" % metrics.accuracy_score(test['target'].values, predictions)
    print "\nAUC Score (Train): %f" % metrics.roc_auc_score(test['target'].astype(int), predprob)
    print "CV Score : Mean - %.7g | Std - %.7g | Min - %.7g | Max - %.7g" % (np.mean(cv_score), np.std(cv_score), np.min(cv_score), np.max(cv_score))
    #diffstats(test,predictions)
    # Print Feature Importance:

    feat_imp = pandas.Series(clf.feature_importances_, predictors).sort_values(ascending=False)
    """    
    feat_df = pd.DataFrame({'features':feat_imp.index, 'rank_sores':feat_imp.values})
    writer = ExcelWriter('Data\desc_var2.xlsx')
    feat_df.to_excel(writer,'feature')
    writer.save()
    """

    feat_imp.plot(kind='bar', title='Feature Importances')
    plt.ylabel('Feature Importance Score')
    plt.show()
    


def buildmodel(train,test):
    targetlisttrain = gettargetlist(train)
    targetlisttest = gettargetlist(test)
    train['target']=targetlisttrain
    test['target']=targetlisttest
    predictors = [x for x in train.columns if x not in ['target','dep']]
    train = train.drop(['dep'], 1)
    classifywithRandomforest(test, train,predictors)
    
        
#step 1: drop survey weight
df = da
df=df.drop(['wtfa_sa'],1)
df=df.drop(['asisad','asiwthls','anx_2'],1)


#step 2: drop depression dep=3 (unknown)
df = df[df.dep!=3]

#step 3: get the target list
df = df.fillna(0)
X = df.values
depress=df['dep'].values
targetlist= gettargetlist(df)

#step 4: elect features using recursive feature selection
featureslist1 = recursivefeatureselect(df, X, targetlist, 31)

#feature_df = pd.DataFrame(featureslist1, columns=["feature"])
#writer = ExcelWriter('Data\desc_var2.xlsx')
#feature_df.to_excel(writer,'ft')
#writer.save()

#drop non-selected features         
excludecolumns = [x for x in df.columns if x not in featureslist1]
excludecolumns = [x.encode('UTF8') for x in excludecolumns]
df = df.drop(excludecolumns, 1)
    
msk = np.random.rand(len(df)) < 0.8
train = df[msk]
test= df[~msk]
train.to_csv('train_dep.csv')
test.to_csv('test_dep.csv')


#run model using Randomforest
buildmodel_RF(train,test)









