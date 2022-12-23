#########################################################################
# Description:    Script to assign NamePrism Leaf Nationalities to      #
#                 ethnic origins based on random forests.               #
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

from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import RandomizedSearchCV

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

#################################
###### Train RandomForest #######
#################################

# (1) specify the model parameters and fit a baseline random forest ------------------
N_TREE = 200
rf = RandomForestClassifier(n_estimators = N_TREE, max_features = "sqrt",
                            min_samples_split = 5, min_samples_leaf = 3,
                            random_state= 28022021)
rf = rf.fit(X = x_train, y = y_train)

y_pred = rf.predict(x_test)
rf_base_acc = metrics.accuracy_score(y_test, y_pred)
f1 = metrics.f1_score(y_true = y_test, y_pred = y_pred, average = "weighted")
print("Overall accuracy of the Random Forest classifier is ", 
      round(rf_base_acc * 100, 1), "%") # 84.6%
print("Weighted F1 score of Random Forest Classifier is: ", 
      round(f1 * 100, 1), "%") # 84.5%

# (2) Hyperparamater Tuning Random Forest --------------------------------------
N_TREE = [x for x in range(200, 2200, 200)]
MAX_FEATURES = ['auto', 'sqrt', 8, 12]
MIN_SPLIT = [2, 5, 8]
MIN_LEAF = [1, 2, 5]
RF_GRID = {"n_estimators": N_TREE, "max_features": MAX_FEATURES,
           "min_samples_split": MIN_SPLIT, "min_samples_leaf": MIN_LEAF}
print("Searching through", 
      np.prod([len(x) for x in RF_GRID.values()]),
      "parameter combinations.")

# find best parameters based on random grid search
rf = RandomForestClassifier() 
rf_random = RandomizedSearchCV(estimator = rf, 
                               param_distributions = RF_GRID, 
                               n_iter = 50, cv = 3, verbose = 0, 
                               random_state = 28022021)

rf_random = rf_random.fit(X = x_train, y = y_train)
print("Parameters of the tuned random forest: ")
print(rf_random.best_params_) # 'max_features': 'auto', 'min_samples_leaf': 1, min_samples_split': 2, 'n_estimators': 1400
rf_best_random = rf_random.best_estimator_

# compare accuracy to base random forest model
y_pred = rf_best_random.predict(x_test)
rf_random_acc = metrics.accuracy_score(y_test, y_pred)
f1 = metrics.f1_score(y_true = y_test, y_pred = y_pred, average = "weighted")
print("Overall accuracy of the tuned Random Forest classifier is ", 
      round(rf_random_acc * 100, 1), "%") #85.0%
print("Improvment against baseline random forest is ", 
      round((rf_random_acc - rf_base_acc) * 100, 1), "percentage points") # 0.4pp
print("Weighted F1 score of tuned Random Forest Classifier is: ", 
      round(f1 * 100, 1), "%") # xx.x%

# (3) Sample weights to imporve learning of Random Forest -------------------------------------
# use weights on samples that have been wrongly classified in baseline
y_pred = rf_best_random.predict(x_train) # predict on training sample
train_acc = metrics.accuracy_score(y_train, y_pred)
print("Accuracy on the training sample is ",
      round(train_acc * 100, 1), "%") # 99.1%

WEIGHTS = np.where(y_pred == y_train, 1, 1-train_acc)
rf_best_random_weights = rf_best_random.fit(X = x_train, y = y_train, sample_weight = WEIGHTS)

y_pred = rf_best_random_weights.predict(x_test) # predict on test sample
rf_random_weight_acc = metrics.accuracy_score(y_test, y_pred)
f1 = metrics.f1_score(y_true = y_test, y_pred = y_pred, average = "weighted")
print("Overall accuracy of the Random Forest classifier with weights is ", 
      round(rf_random_weight_acc * 100, 1), "%") # 85.0%
print("Improvment against not using weights is ", 
      round((rf_random_weight_acc - rf_random_acc) * 100, 1), "percentage points") # 0.0 pp
print("Weighted F1 score of Random Forest Classifier with weights is: ", 
      round(f1 * 100, 1), "%") # xx.x%