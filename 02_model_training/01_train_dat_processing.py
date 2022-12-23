#######################################################################
# Description:    Script to determine parameters for encoding         #
#                 names.                                              #
# Authors:        Matthias Niggli/CIEB UniBasel                       #
# Date:           12.03.2021                                          #
#######################################################################

#######################################
## Load packages and set directories ##
#######################################

#### Import packages ---------------------------------------------------------
import numpy as np
import pandas as pd

#### Set directory ---------------------------------------------------------
path = "..." # your path to the repository directory
print("Directories specified")

####################################################################
############ Construct and investigate the full dataset ############
####################################################################

athletes = pd.read_csv(path+"/00_data_and_model/data/01_athlete_sample.csv").drop(["Year"], axis = 1)
inventors = pd.read_csv(path+"/00_data_and_model/data/04_inventor_sample.csv").rename(columns = {"full_name": "Name"})
inventors = inventors[["Name", "origin", "origin_encoded"]]
print(len(inventors), "inventor names used for training.") # 53'189 (55.9%)

origin_encoded = inventors[["origin", "origin_encoded"]].drop_duplicates().reset_index().drop(["index"], axis = 1)
origin_encoded.loc[len(origin_encoded)] = ["Dutch-Flemish", max(origin_encoded["origin_encoded"]) + 1]
athletes = pd.merge(athletes, origin_encoded, on = "origin")
print(len(athletes), "athletes names used for training.") # 43'588 (44.1%)

#### Evaluate Distribution: --------------------------------------------
df_train = pd.concat([inventors, athletes])
print(len(df_train), "samples can be used for training") # 96'777

origin_samples = df_train.groupby(["origin"]).count()["origin_encoded"]
print(origin_samples)
print("Mean number of samples per ethnic origin is", round(origin_samples.mean()))
print("Median number of samples per ethnic origin is", round(origin_samples.median()))
print("Largest ethnic origin is", origin_samples.idxmax(), "with", max(origin_samples), "samples")
print("Smallest ethnic origin is", origin_samples.idxmin(), "with", min(origin_samples), "samples")
print("Ratio between the largest and smalles ethnic origin is", round(max(origin_samples) / min(origin_samples), 1))

origin_shares = df_train.groupby(["origin"]).count()["origin_encoded"] / len(df_train)
print(round(origin_shares, 3))
print("Mean share of ethnic origin is", round(100 * origin_shares.mean(), 1), "%")
print("Median share of ethnic origin is", round(100 * origin_shares.median(), 1), "%")
print("Largest ethnic origin is", origin_shares.idxmax(), "with", round(100 * max(origin_shares), 1), "%")
print("Smallest ethnic origin is", origin_shares.idxmin(), "with", round(100 * min(origin_shares), 1), "%")

#### check for non-Latin characters in names ----------------------------------
import re

def check_names(names):

    tmp = [bool(re.match('[^a-zA-Z]', x.strip())) for x in names]
    
    special_char_name_idx = np.where(np.asarray(tmp) == True)

    if special_char_name_idx[0].size == 0:
        print("No non-Latin characters or punctuation in names")
        special_char_name_idx = 0
        return(special_char_name_idx)
    else:
        print("Non-Latin characters present in the returned array")
        return(special_char_name_idx[0])

NAMES = list(df_train["Name"])

special_chars_idx = check_names(NAMES)

if special_chars_idx != 0:
    special_chars_idx = special_chars_idx.tolist()
    print(len(special_chars_idx), "names with non-Latin characters")
    special_names = [NAMES[i] for i in special_chars_idx]
    print("Clean special names")

#### choose sequence length ---------------------------------------------------

N_CHARS_NAMES = np.array([len(x) for x in NAMES])
PERCT = 95

MAX_CHARS = np.percentile(N_CHARS_NAMES, PERCT) # 30
print("Truncate maximum number of letters per name at", round(MAX_CHARS), 
     "characters. This leaves", PERCT, "% of names unaffected ")
