#################################################################
# Description:    Script to classify inventors' origins based   #
#                 on the trained model.                         #
# Authors:        Matthias Niggli/CIEB UniBasel                 #
# Last revised:   06.07.2022                                    #
#################################################################

#######################################
## Load packages and set directories ##
#######################################

#### packages for data processing: ---------------------------------------------
library("tidyverse")

#### packages for making predictions using the trained network: -------------------
library("reticulate")
library("tensorflow")
library("keras")

#### use the same python virtual environment as for training the network
use_virtualenv("...", required = TRUE) # your path to the virtualenv

#### directories  -----------------------------------------------------------------
DatDir <- "..." # path to CIEB University of Basel patent data
path = "..." # your path to the repository directory
setwd(path)

#### load the trained LSTM model  -------------------------------------------------
origin_model <- load_model_hdf5("00_data_and_model/model/name_origin_lstm.h5")
if(is.null(origin_model) == FALSE){print("Classification model loaded")}else{"Model could not be loaded"}

#######################################
###### Load inventor information ######
#######################################

# Load the CIEB inventor dataset to be complemented with ethnic origin information
pred_dat <- readRDS(paste0(DatDir, "/created data/inv_origins_temp/inventor_raw.rds"))
print(paste("Information on", nrow(pred_dat), "inventors loaded."))

# Load 18 Ethnic Origin Classes that were used to train the model
y_classes <- read.csv("00_data_and_model/data/04_inventor_sample.csv")
y_classes <- y_classes %>% distinct(origin_encoded, origin) %>% arrange(origin_encoded)
y_classes[nrow(y_classes) + 1, ] <- c(max(y_classes$origin_encoded) + 1, "Dutch") # add Dutch
print(paste(nrow(y_classes), "Ethnic origin classes loaded."))

# Load batch size for the encoded names from the cluster saved in 'inventor_data_encoding.R'
BATCH_PARAMS <- readRDS(paste0(DatDir, "/created data/inv_origins_temp/batch_params.rds"))
print("All data loaded. Ready for classification.")

################################################
########## Classify inventors' origins #########
################################################

res <- lapply(BATCH_PARAMS[[2]], function(x){
        
        # load batch of encoded names
        names_encoded <- readRDS(paste0(DatDir,
                                "/created data/inv_origins_temp/BATCH",
                                x + 1, "_encoded.rds"))

        # subset the inventor dataset to the samples in the encoded batch
        start_idx <- x + 1
        end_idx <- ifelse(nrow(pred_dat) > (x + BATCH_PARAMS[[1]]),
                          (x + BATCH_PARAMS[[1]]), nrow(pred_dat))
        res <- pred_dat[start_idx : end_idx, ]
        
        # classify origin classes of the batch
        res$origin <- as.numeric(origin_model %>% predict_classes(names_encoded[ , , ]))
        res$origin <- y_classes[res$origin + 1, ]$origin
        
        # predict class probabilities of the batch
        tmp <- origin_model %>% predict_proba(names_encoded[ , , ])
        tmp <- as.data.frame(tmp)
        names(tmp) <- paste0("prob_", y_classes$origin)
        res <- cbind(res, tmp)
        
        # clean and return the predicted batch as a data.frame
        names_encoded <- NULL
        print(paste0("Predicted batch ", x + 1, "to", 
                     x + BATCH_PARAMS[[1]]))
        return(res)}
       )

# combine all batches to a single data.frame and delete all batches
pred_dat <- bind_rows(res)
res <- NULL

#### save the full sample on the cluster:
pred_dat %>% saveRDS(paste0(DatDir, "/created data/inventor_origin_joeg_revision.rds"))
print("Dataset saved as 'inventor_origin_joeg_revision.rds'.")


