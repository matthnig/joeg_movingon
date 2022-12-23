#########################################################################
# Description:    Script to assign NamePrism Leaf Nationalities to      #
#                 ethnic origins based on FF Neural Net.                #
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

import tensorflow as tf
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

######################################
###### Train FF Neural Network #######
######################################

tf.random.set_seed(8032021)

# define the network
def FF_network(optimizer = "adam", units = 128, activation = "relu"):
      ff_network = tf.keras.models.Sequential([
            tf.keras.layers.Input(shape = x_train.shape[1]),
            tf.keras.layers.Dense(units = units, activation = activation),
            tf.keras.layers.Dense(len(np.unique(y_test)), activation = "softmax")
            ])
            
      ff_network.compile(optimizer =  optimizer, loss = "sparse_categorical_crossentropy", metrics = ["accuracy"])

      return ff_network

ff_network = tf.keras.wrappers.scikit_learn.KerasClassifier(build_fn=FF_network, verbose = 0)

# set the parameter values
EPOCHS = [20, 30]
BATCHES = [64, 128, 256]
NODES = [512, 256, 128, 64]
ACTIVS = ["relu", "softmax"]
OPTIMS = ["rmsprop", "adam"]

FFNN_GRID = dict(epochs = EPOCHS, batch_size = BATCHES, units = NODES, 
                 activation = ACTIVS, optimizer = OPTIMS)
print("Searching through", 
       np.prod([len(x) for x in FFNN_GRID.values()]),
      "parameter combinations.")


FFNN_random = RandomizedSearchCV(estimator = ff_network, param_distributions = FFNN_GRID, 
                        n_iter = 50, cv = 3, verbose = 0, random_state = 10032021)

FFNN_random = FFNN_random.fit(x_train, y_train)

print("Parameters of the tuned network: ")
print(FFNN_random.best_params_) # units: , optimizer: , epochs:, batch_size:, activation:

y_pred = FFNN_random.best_estimator_.predict(x_test)
FFNN_acc = metrics.accuracy_score(y_test, y_pred)
f1 = metrics.f1_score(y_true = y_test, y_pred = y_pred, average = "weighted")
print("Overall accuracy of the best Feed-Forward Neural Network is ", 
      round(FFNN_acc * 100, 1), "%") # 82.8%
print("Weighted F1 score of Feed-Forward Neural Network is: ", 
      round(f1 * 100, 1), "%") # 82.7%