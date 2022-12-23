#########################################################################
# Description:    Script to construct the inventor sample for training  #
#                 unsing prediction chosen probability thresholds	    #
# Authors:        Matthias Niggli/CIEB UniBasel                         #
# Last Revised:   15.03.2021                                            #
#########################################################################

##################################################
###### Load packages and set directories #########
##################################################

#### Import packages ---------------------------------------------------------
import numpy as np
import pandas as pd
import pyreadr

from sklearn.model_selection import train_test_split
from sklearn import preprocessing
from sklearn import metrics

import xgboost as xgb

#### Set directory ---------------------------------------------------------
path = "..." # your path to the repository directory
print("Directories specified")

########################
###### XGBoost #########
########################

#### load training data -------------------------------------------------------
df = pd.read_csv(path+"/00_data_and_model/data/03_athletes_nameprism.csv")
df = df.drop(labels = ["Name", "Year", "full_name_encoded"], axis = 1)

le = preprocessing.LabelEncoder()
le.fit(df["origin"])
df["origin_encoded"] = le.fit_transform(df["origin"])

response = np.array(df["origin_encoded"])
features = np.array(df.drop(["origin", "origin_encoded"], axis = 1))

#### train the final model using the best parameter values and all samples ---------------------------
xgb_model = xgb.XGBClassifier(random_state = 8032021, n_estimators = 140,
                            learning_rate = 0.1, min_child_weight = 3, max_depth = 3)
xgb_model.fit(X = features, y = response)

###################################################
###### Assign inventors to ethnic origins #########
###################################################

inv_dat = pyreadr.read_r(path+"/00_data_and_model/data/02_inventors_nameprism.rds")
inv_dat = inv_dat[None]

new_dat = inv_dat.drop(["full_name", "full_name_encoded", "scrap"], axis = 1)

# test wether columns match:
test_cols = list(df.columns)[1:]
test_cols = [x.replace(".", ",") for x in test_cols]
for i in range(len(new_dat.columns)):
    if new_dat.columns[i] == test_cols[i]:
        count =+ 1
    else: 
        print("Error found in column number", i)
        print("inventor column:", new_dat.columns[i])
        print("athletes column: ", test_cols[i])

# predict inventors ethnic origins using the trained xgb classifier
new_dat = np.array(new_dat)
pred_origin = xgb_model.predict(new_dat)

# get class probabilities from xgb classifier
class_probas = xgb_model.predict_proba(new_dat)

# calculate subsetting indicators
max_proba = [max(x) for x in class_probas]
pred_second = [sorted(x)[-2] for x in class_probas]
dist_second = pd.Series(max_proba)- pd.Series(pred_second)
entro = [x * np.log(x) for x in class_probas]
entro = [-1 * np.nansum(x) for x in entro]
dat_eval = pd.DataFrame({"max_proba": max_proba, "dist_second": dist_second, "entro": entro})

# subset samples according to the threshold conditions:
MIN_PROBA = 0.65
MIN_DISTANCE = 0.2
MAX_ENTROPY = 1.75

dat_eval = dat_eval.loc[(dat_eval.max_proba >= MIN_PROBA) & 
                        (dat_eval.dist_second >= MIN_DISTANCE) &
                        (dat_eval.entro <= MAX_ENTROPY)]
print(len(dat_eval), "samples can be used for training") # 53'189
print(100 * round(len(dat_eval)/len(new_dat), 3), "% of samples remain for training") # 83.7%

###################################################
###### Create inventor sample for training ########
###################################################

# construct the sample:
keep_idx = dat_eval.index
inventor_sample = inv_dat.loc[keep_idx, ["full_name"]]
inventor_sample["origin_encoded"] = pred_origin[keep_idx]

# assign the ethnic origins to the codes:
origin = df[["origin", "origin_encoded"]].drop_duplicates().reset_index().drop(["index"], axis = 1)
inventor_sample = pd.merge(inventor_sample, origin, on = "origin_encoded")

###################################################
###### Create inventor sample for training ########
###################################################

inventor_sample.to_csv(path+"/00_data_and_model/data/04_inventor_sample.csv", index = False)
print("Ethnic origins of", len(inventor_sample), "encoded and saved.")
