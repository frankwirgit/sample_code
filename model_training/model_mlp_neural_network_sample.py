# An example to train the model with Multi-layer Perceptron Classifier in neural network sklearn package

import os
import shutil
import numpy as np 
import pandas as pd 
#from sklearn.ensemble import RandomForestClassifier
from sklearn.neural_network import MLPClassifier
from sklearn.externals import joblib
from sklearn.metrics import roc_auc_score
from sklearn.preprocessing import StandardScaler
from os.path import join, expanduser
#import sys
import glob
import random

from parameter_settings_mlp import used_features, get_global_argparser

def create_train_test_df(feature_path, data_path,data_name,hor,random_seed,train_ratio,test_ratio,bin_size):

    print("Creating data......")
    feature_path = join(feature_path,"Label_%s" % hor)
    #print("feature_path:%s" %feature_path)
    
    train_path = join(feature_path, "TrainPlusPlus")
    train_pattern = join(train_path,"*_train%s.csv" %train_ratio)
    #print("train_pattern:%s" % train_pattern)
    alltrainfiles = glob.glob(train_pattern)
    print("len(alltrainfiles):", len(alltrainfiles))
    #random.seed(random_seed)
    #random.shuffle(alltestfiles)

    #train_pat = "_train%s" %train_ratio
    #test_pat = "_test%s" %test_ratio
    #testfiles = [f.replace("hr/Train/","hr/Test/").replace(train_pat,test_pat) for f in trainfiles]
    
    test_path = join(feature_path, "TestPlusPlus")
    test_pattern = join(test_path,"*_test%s.csv" %test_ratio)
    #print("test_pattern:%s" % test_pattern)
    alltestfiles = glob.glob(test_pattern)
    random.seed(random_seed)
    
    testfile_index = random.sample(xrange(0, 450), bin_size)
    print("max index", max(testfile_index), "min index", min(testfile_index))
    print("size of test file list", len(alltestfiles))
    
    #test_df = pd.concat(pd.read_csv(alltestfiles[i]) for i in testfile_index)
    test_df = pd.concat(pd.read_csv(f) for f in alltestfiles)
    train_df = pd.concat(pd.read_csv(f) for f in alltrainfiles)
    print("Done reading, train_df.shape:",train_df.shape, ",test_df.shape:",test_df.shape)
    #train_df = train_df.fillna(-999)
    #test_df = test_df.fillna(-999)
    #print("NA is replaced by -999, train_df.shape:",train_df.shape, ",test_df.shape:",test_df.shape)
    train_df = train_df.dropna()
    test_df = test_df.dropna()
    print("After dropna, train_df.shape:",train_df.shape, ",test_df.shape:",test_df.shape)

    #label = "label%s" %hor
    label = "fliplabel%s" %hor
	 
    print("Train:",train_df.groupby([label]).size())
    print("Test:",test_df.groupby([label]).size())

    #print(list(train_df))
    #print(list(test_df))

    output_path = join(data_path, data_name)
    #key to identify experiments
    key ="train%s_test%s_label%s_bin%s_r%s"%(train_ratio,test_ratio,hor,bin_size,random_seed)
    train_filename = 'trainset_%s.csv' % key
    test_filename = 'testset_%s.csv' % key
    
    print("Output path and file:", output_path, train_filename, test_filename)
    train_df.to_csv(join(output_path, train_filename) , index = False)
    test_df.to_csv(join(output_path, test_filename) , index = False)
    print("DONE Creating data......")
    
    return (train_df, test_df)

#def rf_training(exp_path, exp_name, data_path, data_name, hor,  n_trees, num_feat, rf_jobs, random_seed,train_ratio,test_ratio, bin_size, pred_model = True):
def mlp_training(exp_path, exp_name, data_path, data_name, hor, n_iter, random_seed,train_ratio,test_ratio, bin_size, pred_model = True):

    print("Training model......")
    data_path_ = join(data_path, data_name)
    pred_path = join(join(exp_path, exp_name), "predictions")
    model_path = join(join(exp_path, exp_name), "models")
    perf_metrics_path = join(join(exp_path, exp_name), "performance_metrics")

    print(data_path_, pred_path, model_path, perf_metrics_path)
    #key to identify experiments
    key ="train%s_test%s_label%s_bin%s_r%s"%(train_ratio,test_ratio,hor,bin_size,random_seed)
    train_filename = 'trainset_%s.csv' % key
    test_filename = 'testset_%s.csv' % key

    key ="train%s_test%s_label%s_bin%s_r%s"%(train_ratio,test_ratio,hor,bin_size,"400")
    pred_file = '%s_predictions.csv' %key
    perf_file = '%s_perf_metrics.csv' %key
    model_file = '%s_model.pkl' %key
    #feat_imp_file = "%s_feature_importance.csv" %key
    print(train_filename, test_filename, pred_file, perf_file, model_file)
    #sys.exit()

    train_df = pd.read_csv(join(data_path_, train_filename))
    test_df = pd.read_csv(join(data_path_, test_filename))
    print("train_df.shape:", train_df.shape, ", test_df.shape:", test_df.shape)
    #print(len(used_features), used_features)
    train_df_features = train_df[used_features]
    test_df_features = test_df[used_features]
    print("After selection, train_df_features.shape:", train_df_features.shape, ", test_df_features.shape:",test_df_features.shape)

    #label= 'label%s' %hor
    label= 'fliplabel%s' %hor
    #train_features_names = list(train_df_features)
    X_train = train_df_features.values 
    y_train = train_df[label].values
    X_test  = test_df_features.values
    y_test  = test_df[label].values
    
    #Scale the train/test set 
    scalerizer = StandardScaler()
    X_train_scaled = scalerizer.fit_transform(X_train)
    X_test_scaled = scalerizer.transform(X_test)


    #apply the nerual network mlp classifier
    total_feat = len(used_features)
    #apply 3 layers of perceptron
    #500 - mlp = MLPClassifier(hidden_layer_sizes=(total_feat,total_feat,total_feat))
    #700 - mlp = MLPClassifier(hidden_layer_sizes=(total_feat,total_feat,total_feat,total_feat,total_feat,total_feat))
    #900 - mlp = MLPClassifier(hidden_layer_sizes=(total_feat,total_feat,total_feat), activation='logistic')
    #200 - mlp = MLPClassifier(hidden_layer_sizes=(total_feat,24,12), activation='logistic')
    #300 - mlp = MLPClassifier(hidden_layer_sizes=(total_feat,total_feat,total_feat), activation='logistic', solver='sgd')
    #400 - mlp = MLPClassifier(hidden_layer_sizes=(total_feat,24,16,10,6,4), activation='logistic')
    #100 - mlp = MLPClassifier(hidden_layer_sizes=(total_feat,total_feat,total_feat,total_feat,total_feat,total_feat), activation='logistic')
    #700 - mlp = MLPClassifier(hidden_layer_sizes=(total_feat,total_feat,total_feat,total_feat,total_feat,total_feat))
    #mlp = MLPClassifier(hidden_layer_sizes=(num_feat,num_feat,num_feat),max_iter=n_iter)
    mlp.fit(X_train_scaled,y_train)
    #sys.exit()
    #rf_model = RandomForestClassifier(n_estimators = n_trees, min_samples_leaf = 1, 
    #                                 max_leaf_nodes = None, max_features = num_feat, n_jobs = rf_jobs,
    #                                random_state = random_seed)
    #rf_model.fit(X_train_scaled, y_train)

    #feature_imp_df = pd.DataFrame({'feature': train_features_names, 'importance': rf_model.feature_importances_})
    #feature_imp_df = feature_imp_df.sort_values(by=['importance'], ascending=False)
    #feature_imp_df.to_csv(join(perf_metrics_path, feat_imp_file))
    
    if pred_model:
        y_pred = mlp.predict_proba(X_test_scaled)
        hypo_col_idx = np.where(mlp.classes_ == 1)[0][0]
        #y_pred = rf_model.predict_proba(X_test_scaled)
        #hypo_col_idx = np.where(rf_model.classes_ == 1)[0][0]
        y_pred = y_pred[:, hypo_col_idx]
        
        pred_df = test_df[['userid', 'epoch', label]]
        pred_df = pd.concat([pred_df, pd.DataFrame({'ypred': y_pred}, index=pred_df.index.values)], axis = 1)
        pred_df = pred_df.rename(columns={'ypred': 'y_pred', label:'y_test'})
        pred_df.to_csv(join(pred_path, pred_file), index = False)
        fhop = open(expanduser(join(perf_metrics_path, perf_file)), 'w')
        fhop.write("-------------------------------------------------------------------\n")
        fhop.write("Time Horizon: %s \n" %hor)
        fhop.write("ROC AUC: %f\n" % roc_auc_score(y_test, y_pred))
        print("Time Horizon: %s \n" %hor)
        print("ROC AUC: %f\n" % roc_auc_score(y_test, y_pred))
        fhop.write("-------------------------------------------------------------------\n")
        fhop.close()
    #joblib.dump(rf_model, join(model_path, model_file))
    joblib.dump(mlp, join(model_path, model_file))
    print("DONE Training model......")
    print("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")

	
def create_dirs(opt):
    datadir = join(opt.data_path, opt.data_name)
    expdir = join(opt.exp_path, opt.exp_name)
    print(datadir, expdir)
    if os.path.exists(datadir):
        print("Removing %s" %datadir)
        shutil.rmtree(datadir, ignore_errors=True) 
    os.makedirs(datadir)

    if os.path.exists(expdir):
        print("Removing %s" %expdir)
        shutil.rmtree(expdir, ignore_errors=True) 
    os.makedirs(expdir)
    os.makedirs(join(expdir,"models"))
    os.makedirs(join(expdir,"predictions"))
    os.makedirs(join(expdir,"performance_metrics"))


if __name__ == "__main__":
    parser = get_global_argparser()

    parser.add_argument('-feature-path', help='Directory of GC data', default="/storage2/medtronics/Guardian_Connect_JSON_Jan_26/PerUserSplits/TemporalHypoTwentyPercentTest", type=str)
    parser.add_argument('-hor', help="A time horizon used for time window: 2hr or 4hr", default="2hr", type=str)

    parser.add_argument('-train-ratio', help='Train ratio in the range [10,20,...,80]',default=20, type=int)
    parser.add_argument('-test-ratio', help='Test ratio in the range [10,20,...,80]',default=20, type=int)
    parser.add_argument('-bin_size', help='Bin size',default=450, type=int)
    #parser.add_argument('-bin-num', help='Bin num',default=2, type=int)

    parser.add_argument('-niter', help="Max number of iterations for convergence", default=200, type=int)
    
    #parser.add_argument('-ntrees', help="Number of RF trees to use", default=100, type=int)
    #parser.add_argument('-nfeat', help="Number of features considered at each split", default=10, type=int)
    #parser.add_argument('-rf-jobs', help="Number of RF trees to run in parallel", default=10, type=int)
    #parser.add_argument('-rseed', help="Random seed to use", type=int, default=99)

    opt = parser.parse_args()
    
    #create_dirs(opt)
    #for i in range(10):
    for i in range(1):
        #opt.rseed = random.randrange(1, 1000)
        opt.rseed = 500
        for h in ["2hr", "4hr"]:
            opt.hor = h
            opt.train_ratio = 80
            #for k in [30,40,50,60,70,80,90]:
                #opt.bin_size = k
            print("================================")
            #continue
            #create_train_test_df(opt.feature_path, opt.data_path, opt.data_name, opt.hor, opt.rseed, opt.train_ratio, opt.test_ratio, opt.bin_size)
            #rf_training(opt.exp_path, opt.exp_name, opt.data_path, opt.data_name, opt.hor, opt.ntrees, opt.nfeat, opt.rf_jobs, opt.rseed, opt.train_ratio, opt.test_ratio, opt.bin_size)
            mlp_training(opt.exp_path, opt.exp_name, opt.data_path, opt.data_name, opt.hor, opt.niter, opt.rseed, opt.train_ratio, opt.test_ratio, opt.bin_size)
            #end for
        #end for
    #end for
    