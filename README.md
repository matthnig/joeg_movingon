# Replication Code for 'Moving On' - Investigating Inventors' Ethnic Origins Using Supervised Learning
This repository contains the replication code for the article that is forthcoming in the *Journal of Economic Geography*.

## Repository Structure
Raw data and ceated **datasets** for this article can be found in the folder `00_data_and_model`. Replication code is placed in the other three folders `01_create_training_dataset`, `02_model_training` and `03_inventor_composition_analysis`, whereas the numbering `0x_` indicates the running order of the code that roughly corresponds to the article structure.   

The folder `01_create_training_dataset` contains all the replication code to generate the **training dataset** for the LSTM classifier. `02_model_training` features scripts to train the **LSTM classifier** using `tensorflow`. The `03_inventor_composition_analysis` directory contains scripts for predicting the ethnic origins of a large sample of patent inventors and the subsequent **descriptive analyses**. These computations were performed at sciCORE (http://scicore.unibas.ch/) scientific computing core facility at University of Basel. Since the corresponding patent data from the Center for International Business and Economics has been cleaned and enriched with additional data, it is partly proprietary and cannot be made publicly accessible as such. However patent inventor information can be gathered upon registration from either the [USPTO](https://patentsview.org/) or the [OECD](https://www.oecd.org/sti/intellectual-property-statistics-and-analysis.htm), and I further provide a random sub-sample of the dataset (`00_data_and_model/data/05_example_inventors_dat.csv`) used in this article to demonstrate the replication of all descriptive results. The correpsonding code can be found in the folder `03_inventor_composition_analysis/`

## Classifier Usage
Note that the trained LSTM classifier, which is at the core of the analysis in this paper, can be accessed or downloaded at `00_data_and_model/model/name_origin_lstm.h5`. **A package that allows for easy use of the classifier is in the making.**. For now, you can use the classifier by downloading the LSTM model and following the encoding and predicting procedure as stated in the code scripts in the folders `02_model_training/` and `03_inventor_composition_analysis/`. 

## Dependencies
All code is either written in `Python` (Version 3.7.) or `R` (Version 4.0.1). Corresponding Python libraries can be found in the `environment.yml` or `requirements.txt` files (installing a virtual environment is recommended). `R` packages needed for replication are: `tidyverse`, `data.table`, `reticulate`, `tensorflow`, `keras`, `stringi`, `jsonlite`, `countrycode`, and `viridis` and can be installed from CRAN.

## References & Contact
Niggli, M. (*forthcoming*), Moving On-Investigating Inventors' Ethnic Origins Using Supervised Learning, Journal of Economic Geography.

If you have questions, please contact me at matthias.niggli@unibas.ch.





