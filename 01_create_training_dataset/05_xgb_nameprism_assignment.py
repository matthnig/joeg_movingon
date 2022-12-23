#########################################################################
# Description:    Script to assign NamePrism Leaf Nationalities to      #
#                 ethnic origins based on XGBoost.                      #
# Authors:        Matthias Niggli/CIEB UniBasel                         #
# Last Revised:   10.03.2021                                            #
#########################################################################

#######################################
###### Load packages and data #########
#######################################

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

#### Load & process the data -------------------------------------------------
df = pd.read_csv(path+"/00_data_and_model/data/03_athletes_nameprism.csv")
df = df.drop(labels = ["Name", "Year", "full_name_encoded"], axis = 1)

le = preprocessing.LabelEncoder()
le.fit(df["origin"])
df["origin_encoded"] = le.fit_transform(df["origin"])

response = np.array(df["origin_encoded"])
features = np.array(df.drop(["origin", "origin_encoded"], axis = 1))

x_train, x_test, y_train, y_test = train_test_split(features, response, 
                                                    test_size = 0.2, 
                                                    random_state = 25022021)
print('Training Features Shape:', x_train.shape)
print('Training Labels Shape:', y_train.shape)
print('Testing Features Shape:', x_test.shape)
print('Testing Labels Shape:', y_test.shape)

############################
###### Train XGBoots #######
############################

# (1) Baseline --------------------------------------------------------
xgb_model = xgb.XGBClassifier(random_state = 8032021, n_estimators = 100, learning_rate = 0.1)
xgb_model.fit(X = x_train, y = y_train)
y_pred = xgb_model.predict(x_test)
xgb_base_acc = metrics.accuracy_score(y_test, y_pred)
print("Overall accuracy of the XGBoost is ", 
      round(xgb_base_acc * 100, 1), "%") # 85.2%
f1 = metrics.f1_score(y_true = y_test, y_pred = y_pred, average = "weighted")
print("Weighted F1 score of XGBoost is: ", 
      round(f1 * 100, 1), "%") # 85.1%

# (2) Hyperparameter Tuning ------------------------------------------
N_ESTIMATORS = [x for x in range(60, 160, 20)]
MAX_DEPTH = [x for x in range(1, 11)]
LEARN_RATE = [0.1, 0.05, 0.01]
CHILD_WEIGHTS = [1, 3, 5]
XG_GRID = {"n_estimators": N_ESTIMATORS, "max_depth": MAX_DEPTH,
           "learning_rate": LEARN_RATE, "min_child_weight": CHILD_WEIGHTS}
print("Searching through", 
      np.prod([len(x) for x in XG_GRID.values()]),
      "parameter combinations.")

""" # use subsample for testing the code
x_train, x_test, y_train, y_test = train_test_split(features, response, 
                                                    test_size = 250, train_size = 750,
                                                    random_state = 8032021)
 """
xgb_model = xgb.XGBClassifier()
xgb_random = RandomizedSearchCV(estimator = xgb_model, 
                               param_distributions = XG_GRID, 
                               n_iter = 50, cv = 3, verbose = 0, 
                               random_state = 8032021)
xgb_random = xgb_random.fit(X = x_train, y = y_train)
print("Parameters of the tuned XGBoost: ")
print(xgb_random.best_params_) # ''n_estimators': 140, 'min_child_weight': 3, 'max_depth': 3, 'learning_rate': 0.1: 


xgb_best_random = xgb_random.best_estimator_
y_pred = xgb_best_random.predict(x_test)
xgb_random_acc = metrics.accuracy_score(y_test, y_pred)
print("Overall accuracy of the tuned XGBoost is ", 
      round(xgb_random_acc * 100, 1), "%") # 85.2%
f1 = metrics.f1_score(y_true = y_test, y_pred = y_pred, average = "weighted")
print("Weighted F1 score of XGBoost is: ", 
      round(f1 * 100, 1), "%") # 85.1%
print("Best parameters of the tuned XGBoost are:")
print(xgb_random.best_params_)