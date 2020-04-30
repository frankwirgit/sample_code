# An example for knee pain analysis based on multiple predictive models - SVC, Gradient boosting, RandomForrest classifiers.

import numpy as np
import scipy as sp
import scipy.stats
import pylab as pl
import matplotlib.pyplot as plt
from sklearn import datasets,svm,cross_validation, metrics
from sklearn.feature_selection import SelectKBest,chi2,RFE
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import GradientBoostingClassifier,RandomForestClassifier
from sklearn.svm import LinearSVC
from sklearn.feature_selection import SelectFromModel
from sklearn.ensemble import ExtraTreesClassifier
from sklearn.cluster import KMeans
from sklearn.cross_validation import train_test_split
from sklearn.decomposition import PCA
from pandas import *
import math
import sys
import matplotlib.pylab as plt
from matplotlib.pylab import rcParams
rcParams['figure.figsize'] = 20, 9
reload(sys)
sys.setdefaultencoding('utf8')
unicode('\xd0', errors='ignore')

def percentage(x,y):

    return ((int(x)*int(100))/float(y))

def diffstats(test,predictions):
    agelist = test['age_p'].values

    list1844,list4564,list65,listmale,listfemale,targetlist1844,targetlist4564,targetlist65,targetmale,targetF=[],[],[],[],[],[],[],[],[],[]


    for i in xrange(len(test)):
        if ((agelist[i] >= 18) and (agelist[i] <= 44)):
            list1844.append(test['target'].values[i])
            targetlist1844.append(predictions[i])



        elif ((agelist[i] >= 45) and (agelist[i] <= 64)):
            list4564.append(test['target'].values[i])
            targetlist4564.append(predictions[i])

        elif (agelist[i] >= 65):
            list65.append(test['target'].values[i])
            targetlist65.append(predictions[i])


        if (test['sex'].values[i]==1):
            listmale.append(test['target'].values[i])
            targetmale.append(predictions[i])
        elif (test['sex'].values[i]==2):
            listfemale.append(test['target'].values[i])
            targetF.append(predictions[i])

    print "Accuracy 18-44 years: %.4g" % metrics.accuracy_score(list1844, targetlist1844)
    print "Accuracy 45-64 years: %.4g" % metrics.accuracy_score(list4564, targetlist4564)
    print "Accuracy 65+ years: %.4g" % metrics.accuracy_score(list65, targetlist65)


    print "Accuracy male: %.4g" % metrics.accuracy_score(listmale, targetmale)
    print "Accuracy female: %.4g" % metrics.accuracy_score(listfemale, targetF)


def gettargetlist(df,targetname):
    keyname=df[targetname].values
    #rightknee=df['jmthp10'].values
    targetlist=[]
    for i in xrange(len(keyname)):
        if (keyname[i]==1):
            targetlist.append(1)
        else:
            targetlist.append(0)
    return targetlist


def preprocess(df,targetcolumname):
    df=df.dropna(axis=1,how='all')

    #remove the rows which have target attribute as null
    df = df[np.isfinite(df[targetcolumname])]
    df = df[df[targetcolumname]<3]
    #df = df[df.jmthp10 < 3]
    df = df.dropna(thresh=len(df) - 1000, axis=1)
    df = df.fillna(0)
    df=df.drop('alastvrb',1)#'jntchr','hrfam','hrtev','ahado','ahearst2','jnthp','jntsymp','intv_mon','jawp','arthwt', 'arthph', 'arthlmt', 'arthwrk','occupn1','jntpn','awaitrmn','flstoop','avisapn2','alastyp2','fdrn_flg','hrfire','hhx','wtia_sa','wtfa_sa','hybpcktp','sinyr','flstoop','strat_p','hrtest','fpx','psu_p','che','proxysa','fmx','srvy_yr','rectype'],1)
    X = df.values
    targetlist= gettargetlist(df,targetcolumname)

    return df,X,targetlist


def preprocessless3months(df):
    df=df.dropna(axis=1,how='all')

    #remove the rows which have target attribute as null
    df = df[np.isfinite(df['jmthp9'])]
    df = df[df.jmthp9<3]
    df = df[df.jmthp10 < 3]
    df = df[df.jntchr == 2]
    df = df.dropna(thresh=len(df) - 1000, axis=1)

    df = df.fillna(366)
    df=df.drop(['alastvrb','hhx','wtia_sa','wtfa_sa','jntchr','sprflu2', 'arx12_5', 'livyr', 'ephev', 'ahcafyr2',
'kidwkyr', 'angev', 'ulcev', 'canev','hybpcktp','sinyr','strat_p','hrtest','fpx','psu_p','che','proxysa','fmx','srvy_yr','rectype'],1)
    X = df.values
    targetlist= gettargetlist(df)

    return df,X,targetlist

def preprocessgreater3months(df):
    df=df.dropna(axis=1,how='all')

    #remove the rows which have target attribute as null
    df = df[np.isfinite(df['jmthp9'])]
    df = df[df.jmthp9<3]
    df = df[df.jmthp10 < 3]
    df = df[df.jntchr == 1]
    df = df.dropna(thresh=len(df) - 1000, axis=1)
    df = df.fillna(0)
    df=df.drop(['alastvrb','jntchr','jnthp','jntsymp','jawp','arthwt', 'arthph', 'arthlmt', 'arthwrk','occupn1','jntpn','awaitrmn','flstoop','avisapn2','alastyp2','fdrn_flg','hrfire','hhx','wtia_sa','wtfa_sa','hybpcktp','sinyr','flstoop','strat_p','hrtest','fpx','psu_p','che','proxysa','fmx','srvy_yr','rectype'],1)
    X = df.values
    targetlist= gettargetlist(df)

    return df,X,targetlist


def univariatefeatureselect(df, X,targetlist):
    rfe = SelectKBest(chi2, k=100)

    rfe.fit_transform(X, targetlist)
    #print rfe.get_support()
    #print rfe.scores_
    featureslist = []
    rankinglist = []
    for i in xrange(len(df.columns)):
        if rfe.get_support()[i] == True:
            #print str(df.columns[i])
            featureslist.append(str(df.columns[i]))
            rankinglist.append(rfe.scores_[i])
    return [x for (y,x) in sorted(zip(rankinglist,featureslist))]

def recursivefeatureselect(df,X,targetlist):
    model = LogisticRegression()
    rfe = RFE(model, 30)
    rfe = rfe.fit(X, targetlist)
    #print rfe.support_
    featureslist = []
    rankinglist=[]
    for i in xrange(len(df.columns)):
        if rfe.support_[i] == True:
            #print str(df.columns[i])
            featureslist.append(str(df.columns[i]))
            rankinglist.append(rfe.ranking_[i])
    return [x for (y,x) in sorted(zip(rankinglist,featureslist))]

def L1basedfeatureselect(df, X,targetlist):
    lsvc = LinearSVC(C=0.01, penalty="l1", dual=False).fit(X, targetlist)
    model = SelectFromModel(lsvc, prefit=True)
    X_new = model.transform(X)
    featureslist=[]
    for i in xrange(len(df.columns)):
        if model.get_support()[i] == True:
            #print str(df.columns[i])
            featureslist.append(str(df.columns[i]))
    return featureslist

def treebasedfeatureselect(df, X,targetlist):
    clf = ExtraTreesClassifier()
    clf = clf.fit(X, targetlist)
    model = SelectFromModel(clf, prefit=True)
    X_new = model.transform(X)

    featureslist=[]
    for i in xrange(len(df.columns)):
        if model.get_support()[i] == True:
            featureslist.append(str(df.columns[i]))
    return featureslist

def classifywithGBM(test,train,predictors,cv_folds=5):
    clf = GradientBoostingClassifier(learning_rate=0.1, min_samples_split=100,min_samples_leaf=60,max_depth=9,max_features='sqrt',subsample=0.8,random_state=10)

    clf.fit(train[predictors], train['target'])
    predictions = clf.predict(test[predictors])
    predprob = clf.predict_proba(test[predictors])[:, 1]

    #Perform cross-validation:

    #cv_score = cross_validation.cross_val_score(clf, train[predictors], train['target'], cv=cv_folds, scoring='roc_auc')
    print "Accuracy : %.4g" % metrics.accuracy_score(test['target'].values, predictions)
    print "\nAUC Score (Test): %f" % metrics.roc_auc_score(test['target'].astype(int), predprob)
    print "\nCross Validation Score : Mean - %.7g | Std - %.7g | Min - %.7g | Max - %.7g" % (np.mean(cv_score), np.std(cv_score), np.min(cv_score), np.max(cv_score))

    # Print Feature Importance:

    feat_imp = pandas.Series(clf.feature_importances_, predictors).sort_values(ascending=False)
    feat_imp.plot(kind='bar', title='Feature Importances')
    plt.ylabel('Feature Importance Score')
    plt.show()

def classifywithRandomforest(test,train,predictors,cv_folds=5):
    clf = RandomForestClassifier(n_estimators=1000)
    clf.fit(train[predictors], train['target'])
    predictions = clf.predict(test[predictors])
    predprob = clf.predict_proba(test[predictors])[:, 1]

    #Perform cross-validation:

    #cv_score = cross_validation.cross_val_score(clf, train[predictors], train['target'], cv=cv_folds, scoring='roc_auc')
    print "Accuracy : %.4g" % metrics.accuracy_score(test['target'].values, predictions)
    print "\nAUC Score (test): %f" % metrics.roc_auc_score(test['target'].astype(int), predprob)
    #print "\nCross Validation Score : Mean - %.7g | Std - %.7g | Min - %.7g | Max - %.7g" % (np.mean(cv_score), np.std(cv_score), np.min(cv_score), np.max(cv_score))
    #diffstats(test,predictions)
    # Print Feature Importance:

    feat_imp = pandas.Series(clf.feature_importances_, predictors).sort_values(ascending=False)

    feat_imp.plot(kind='bar', title='Feature Importances')
    plt.ylabel('Feature Importance Score')
    plt.show()

def classifywithsvm(test, train,predictors):
    clf = svm.SVC(kernel='rbf',C=1, gamma=1)
    clf.fit(train[predictors], train['target'])
    predictions = clf.predict(test[predictors])
    #cv_score = cross_validation.cross_val_score(clf, train[predictors], train['target'], cv=cv_folds, scoring='roc_auc')
    print "Accuracy : %.4g" % metrics.accuracy_score(test['target'].values, predictions)
    #print "CV Score : Mean - %.7g | Std - %.7g | Min - %.7g | Max - %.7g" % (np.mean(cv_score), np.std(cv_score), np.min(cv_score), np.max(cv_score))


def buildmodel(train,test,targetcolumname):
    targetlisttrain = gettargetlist(train,targetcolumname)
    targetlisttest = gettargetlist(test,targetcolumname)
    train['target']=targetlisttrain
    test['target']=targetlisttest
    predictors = [x for x in train.columns if x not in ['target',targetcolumnname]]
    train = train.drop([targetcolumnname], 1)
    #classifywithGBM(test, train,predictors)
    classifywithRandomforest(test, train,predictors)
    #classifywithsvm(test, train,predictors)


if __name__ == "__main__":
    df = pandas.read_stata('samadult.dta', encoding= 'utf-8')
    targetcolumnname = str(raw_input("\nEnter target column attribute name:"))
    df,X,targetlist = preprocess(df,targetcolumnname)
    featureslistmandatory=['bmi','age_p','sex','smkstat2','aweightp','doinglwa','racerpi2','r_maritl','arth1' 'hispan_i']

    #Analyse the data for people who have knee pain < 3 months ago
    #df, X, targetlist = preprocessless3months(df)
    # Analyse the data for people who have knee pain > 3 months ago
    #df, X, targetlist = preprocessgreater3months(df)

    featureslist = univariatefeatureselect(df,X,targetlist)

    #featureslist = recursivefeatureselect(df, X, targetlist)

    #featureslist = L1basedfeatureselect(df, X, targetlist)

    #featureslist1 = treebasedfeatureselect(df, X, targetlist)

    featureslistfinal = list(set(featureslist) | set(featureslistmandatory))
    print featureslistfinal
    print featureslistfinal.__len__()
    #u = set.intersection(set(featureslist1),set(featureslist2),set(featureslist3),set(featureslist4))
    excludecolumns = [x for x in df.columns if x not in featureslistfinal]
    df = df.drop(excludecolumns, 1)
    df.columns = [x.encode('UTF8') for x in df.columns]
    train, test = train_test_split(df, test_size=0.2)
    #k_means = KMeans(n_clusters=3, random_state=0)
    #k_means.fit(train)
    #predicted = k_means.predict(train)



    #train.to_csv('train.csv')
    #test.to_csv('test.csv')
    buildmodel(train,test,targetcolumnname)












