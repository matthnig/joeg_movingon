#########################################################################
# Description:    Script to determine optimal prediction thresholds	    #
#                 in order to use samples for learning.		            #
# Authors:        Matthias Niggli/CIEB UniBasel                         #
# Last Revised:   11.03.2021                                            #
#########################################################################

##################################################
###### Load packages and set directories #########
##################################################

#### Import packages ---------------------------------------------------------
import numpy as np
import pandas as pd

from sklearn.model_selection import train_test_split
from sklearn import preprocessing
from sklearn import metrics

from sklearn.model_selection import RandomizedSearchCV
import xgboost as xgb
print("All packages loaded.")

#### Set directory ---------------------------------------------------------
path = "..." # your path to the repository directory
print("Directories specified")

#############################
###### Load data ############
#############################

# data
df = pd.read_csv(path+"/00_data_and_model/data/03_athletes_nameprism.csv")

le = preprocessing.LabelEncoder()
le.fit(df["origin"])
df["origin_encoded"] = le.fit_transform(df["origin"])
response = np.array(df["origin_encoded"])
features = np.array(df.drop(["Name", "Year", "full_name_encoded", "origin", "origin_encoded"], axis = 1))
indices = np.arange(len(df))

# use only those samples the XGB did not use for training:
x_train, x_test, y_train, y_test, idx_train, idx_test = train_test_split(
    features, response, indices,
    test_size = 0.2, random_state = 25022021)

print('Training Features Shape:', x_train.shape)
print('Training Labels Shape:', y_train.shape)
print('Testing Features Shape:', x_test.shape)
print('Testing Labels Shape:', y_test.shape)

############################################
###### Get the XGB model ###################
############################################

# best parameters from hyperparameter tuning the XGB:
# {'n_estimators': 140, 'min_child_weight': 3, 'max_depth': 3, 'learning_rate': 0.1}
xgb_model = xgb.XGBClassifier(random_state = 8032021, n_estimators = 140,
                            learning_rate = 0.1, min_child_weight = 3, max_depth = 3)
xgb_model.fit(X = x_train, y = y_train)

y_pred = xgb_model.predict(x_test)
acc = metrics.accuracy_score(y_test, y_pred)
f1 = metrics.f1_score(y_true = y_test, y_pred = y_pred, average = "weighted")
print("Overall accuracy of the tuned XGBoost is ", 
      round(acc * 100, 1), "%") # 85.2%
print("Weighted F1 score of the tuned XGBoost is: ", 
      round(f1 * 100, 1), "%") # 85.1%

#################################################################
###### Subsampling based on 3 metrics to increase performance ###
#################################################################

#### group shares ----------------------
group_shares = pd.Series(y_test).value_counts()/len(y_test)
group_shares = group_shares.sort_index()

#### metrics -----------------------------
class_probas = xgb_model.predict_proba(x_test)

# 1) highest origin probability
max_proba = [max(x) for x in class_probas]

# 2) difference to second highest origin probability
pred_second = [sorted(x)[-2] for x in class_probas]
dist_second = pd.Series(max_proba)- pd.Series(pred_second)

# 3) entropy among all origin probabilities
entro = [x * np.log(x) for x in class_probas]
entro = [-1 * np.nansum(x) for x in entro]

dat_eval = pd.DataFrame({"max_proba": max_proba, "dist_second": dist_second, "entro": entro})

# group shares
groups = pd.DataFrame(columns = ["ethnic origin", "baseline_share", "min_proba", 
                                "min_distance", "max_entropy", "N_samples", "sample_share"])
def group_size_eval(MAX_PROBA, DIST_SECOND, ENTRO):
    tmp = dat_eval[(dat_eval["max_proba"] >= MAX_PROBA) &
                   (dat_eval["entro"] <= ENTRO) &
                   (dat_eval["dist_second"] >= DIST_SECOND)]
    
    N_samples = pd.Series(y_pred[tmp.index])
    N_samples = N_samples.value_counts().sort_index()
    sample_share = [x / sum(N_samples) for x in N_samples]
    
    tmp = pd.DataFrame({"ethnic origin": N_samples.index, "baseline_share": list(group_shares),
                    "min_proba": MAX_PROBA, "min_distance": DIST_SECOND, "max_entropy": ENTRO,
                    "N_samples": list(N_samples), "sample_share": sample_share})
    return(tmp)

def acc_evaluate(MAX_PROBA, DIST_SECOND, ENTRO):
    tmp = dat_eval[(dat_eval["max_proba"] >= MAX_PROBA) &
                   (dat_eval["entro"] <= ENTRO) &
                   (dat_eval["dist_second"] >= DIST_SECOND)]
    
    y_pred_thres = y_pred[tmp.index]
    y_test_thres = y_test[tmp.index]
    
    thres_acc = metrics.accuracy_score(y_test_thres, y_pred_thres)
    thres_f1 = metrics.f1_score(y_test_thres, y_pred_thres, average = "weighted")
    sample_fraction = len(y_test_thres) / len(y_test)
    
    res_out = [MAX_PROBA, DIST_SECOND, ENTRO, 
               round(sample_fraction, 3), round(thres_acc, 3), round(thres_f1, 3)]
    return(res_out)

# define threshold values to evaluate
PROBAS = [0, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7]
DISTANCES = [0, 0.2, 0.3, 0.4, 0.5] 
ENTROPIES = [10, 2, 1.75]

res = pd.DataFrame(None, columns = ["min_proba", "min_distance",
                                    "max_entropy", "sample_fraction",
                                    "accuracy", "f1"])

# check accuracies for threshold combinations
for PROBA in PROBAS:
    for DIST in DISTANCES:
        for ENTROP in ENTROPIES:
            res_out = acc_evaluate(MAX_PROBA=PROBA, DIST_SECOND=DIST, ENTRO = ENTROP)
            res.loc[len(res)] = res_out
            groups_out = group_size_eval(MAX_PROBA=PROBA, DIST_SECOND=DIST, ENTRO = ENTROP)
            groups = groups.append(groups_out)
            
res = res.sort_values("f1", ascending = False)

# save the results
res.to_csv(path + "/01_create_training_dataset/df0_evaluation_indicator_thresholds.csv")
print("Evaluation results for indicator thresholds saved.")
groups.to_csv(path + "/01_create_training_dataset/df1_evaluation_indicator_groupsize.csv")
print("Group size evaluation for indicator thresholds saved.")