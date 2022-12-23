#################################################################
# Description:    Script to train the LSTM model to classify    #
#                 inventor origins based on names.              #
# Authors:        Matthias Niggli/CIEB UniBasel                 #
# Last Revised:   25.03.2021                                    #
#################################################################

#### Import packages ---------------------------------------------------------
import tensorflow as tf
import numpy as np
import pandas as pd
from matplotlib import pyplot as plt

from sklearn.model_selection import train_test_split
from sklearn import metrics

import random
print("All packages loaded.")

#### set seed for reproducibility --------------------------------------------
tf.random.set_seed(10082020)
random.seed(13032021)

#### Set directory-------------------------------------------------------------
path = "..." # your path to the repository directory
print("Directories specified")

#### Load the data ------------------------------------------------------------
athletes = pd.read_csv(path+"/00_data_and_model/data/01_athlete_sample.csv").drop(["Year"], axis = 1)
inventors = pd.read_csv(path+"/00_data_and_model/data/04_inventor_sample.csv").rename(columns = {"full_name": "Name"})
inventors = inventors[["Name", "origin", "origin_encoded"]]

origin_encoded = inventors[["origin", "origin_encoded"]].drop_duplicates().reset_index().drop(["index"], axis = 1)
origin_encoded.loc[len(origin_encoded)] = ["Dutch-Flemish", max(origin_encoded["origin_encoded"]) + 1]
athletes = pd.merge(athletes, origin_encoded, on = "origin")
df_train = pd.concat([inventors, athletes])

#### shuffle the data and reset index ------------------------------------------------------------
df_train = df_train.sample(frac = 1, random_state = 13032021)
df_train = df_train.reset_index().drop(["index"], axis = 1)
print(len(df_train), "samples can be used for training")

############################################
#### Encode data for training the model ####
############################################

#### Specify parameters for encoding -----------------------------------------------------------
CHAR_DICT = list([chr(i) for i in range(97,123)])+[" ", "END"]  # all letters of the latin alphabeth
SEQ_MAX = 30                                                    # leaves 95% of names unaffected
N_CHARS = 28                                                    # latin alphabeth plus whitespace and padding
NAMES = df_train["Name"]

#### Function for encoding names as one-hot-encoded sequence of letters ---------------
def encode_chars(names, seq_max, char_dict, n_chars):

    N = len(names)
    END_idx = np.where(pd.Series(char_dict) == "END")[0][0]
    
    # Create 3D-Tensor with shape: 
    # (No. of samples, maximum name length, number of characters):
    tmp = np.zeros(shape = (N, seq_max, n_chars)) 

    # iterate over all names
    for i in range(N):
        name = names[i]
        
        # truncate at seq_max
        if(len(name) > seq_max):
            name = name[:seq_max]
        
        # encode characters
        for char in range(len(name)):
            idx_pos = np.where(pd.Series(char_dict) == name[char])[0][0]
            tmp[i, char, idx_pos] = 1
            
        # padding
        if len(name) < seq_max:
            tmp[i, len(name):seq_max, END_idx] = 1
    
    return tmp

#### Encode names and ethnic origins --------------------------
x_dat = encode_chars(names = NAMES, char_dict = CHAR_DICT,
             seq_max = SEQ_MAX, n_chars = N_CHARS)
y_dat = np.array(df_train["origin_encoded"])

print("All names classified and encoded for training the model.")

#########################################
#### Define training and testing set ####
#########################################

indices = np.arange(len(df_train))
x_train, x_test, y_train, y_test, idx_train, idx_test = train_test_split(x_dat, y_dat, indices, test_size = 0.1, random_state = 30032021, stratify = y_dat)

print('Training Features Shape:', x_train.shape)
print('Training Labels Shape:', y_train.shape)
print('Testing Features Shape:', x_test.shape)
print('Testing Labels Shape:', y_test.shape)

#########################################
#### Set up and train the final model ###
#########################################

model = tf.keras.models.Sequential([
    tf.keras.layers.LSTM(units = 512, return_sequences = True,
                         dropout = 0.2,
                         input_shape = (x_train.shape[1], x_train.shape[2])),
    tf.keras.layers.LSTM(units = 256, dropout=0.2,
                         return_sequences = True),
    tf.keras.layers.LSTM(units = 64, dropout = 0.2,
                         return_sequences = False),
    tf.keras.layers.Dense(len(np.unique(y_train)), activation = "softmax")
    ])

OPTIMIZER = tf.keras.optimizers.Adam(learning_rate = 0.0025)

model.compile(optimizer = OPTIMIZER,
             loss = "sparse_categorical_crossentropy",
             metrics = ["accuracy"])

# set training parameters
EPOCHS = 50
BATCH_SIZE = 256
CALLBACK = tf.keras.callbacks.EarlyStopping(monitor = 'val_loss', 
                                            patience = 7, 
                                            restore_best_weights = True)
print("Model parameters specified")

# train the network
hist = model.fit(x = x_train, y = y_train,
                epochs = EPOCHS, batch_size = BATCH_SIZE,
                callbacks = [CALLBACK], verbose = 2,
                validation_data = (x_test, y_test))
print("Final Model trained")

# Show the training history:
fig, axs = plt.subplots(1, 2, figsize=(10,5))
axs[0].plot(hist.epoch, hist.history['loss'])
axs[0].plot(hist.epoch, hist.history['val_loss'])
axs[0].legend(('training loss', 'validation loss'), loc='upper right')
axs[1].plot(hist.epoch, hist.history['accuracy'])
axs[1].plot(hist.epoch, hist.history['val_accuracy'])
axs[1].legend(('training accuracy', 'validation accuracy'), loc='lower right')
plt.show()
plt.savefig(path + "/training_hist.png")
print("Training history saved")

##################################################
#### Evaluate the final model on the test set ####
##################################################

y_pred = np.argmax(model.predict(x_test), axis=-1)
f1 = metrics.f1_score(y_true = y_test, y_pred = y_pred, average = "weighted")
print("F1 score on the test set is:", round(f1, 3))

LABELS = origin_encoded.sort_values(["origin_encoded"])["origin"].tolist()
model_report = metrics.classification_report(y_true = y_test, y_pred = y_pred, 
                                            digits = 3, target_names = LABELS)
print(model_report)

###############################################
#### Train with all data an save the model ####
###############################################

print("Train final model with all data")
print("Features:", x_dat.shape)
print("Response:", y_dat.shape)

# final model using all available data
final_model = tf.keras.models.Sequential([
    tf.keras.layers.LSTM(units = 512, return_sequences = True,
                         dropout = 0.2,
                         input_shape = (x_train.shape[1], x_train.shape[2])),
    tf.keras.layers.LSTM(units = 256, dropout=0.2,
                         return_sequences = True),
    tf.keras.layers.LSTM(units = 64, dropout = 0.2,
                         return_sequences = False),
    tf.keras.layers.Dense(len(np.unique(y_train)), activation = "softmax")
    ])

OPTIMIZER = tf.keras.optimizers.Adam(learning_rate = 0.0025)

final_model.compile(optimizer = OPTIMIZER,
             loss = "sparse_categorical_crossentropy",
             metrics = ["accuracy"])


# train the final network
EPOCHS = 30
BATCH_SIZE = 256
hist = final_model.fit(x = x_dat, y = y_dat,
                epochs = EPOCHS, batch_size = BATCH_SIZE,
                verbose = 0)
print("Final Model trained")

# # sanity check: evaluate on test data (should be higher f1)
y_pred = np.argmax(final_model.predict(x_test), axis=-1)
f1_final = metrics.f1_score(y_true = y_test, y_pred = y_pred, average = "weighted")
print("F1 score of the model trained on all data on the test set is:", round(f1_final, 3))
if f1_final > f1:
    print("Sanity check fullfilled. Model trained on all data performs better on test dataset.")

# save the model ----------
tf.keras.models.save_model(model = final_model, filepath = path + "/00_data_and_model/model/name_origin_lstm.h5")
print("Final model saved")
