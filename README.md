# Replication Code for 'Moving On' - Investigating Inventors' Ethnic Origins Using Supervised Learning
This repository contains the replication code for the article that is forthcoming in the *Journal of Economic Geography*.

## Repository Structure
Raw data and ceated **datasets** for this article can be found in the folder `00_data_and_model`. Replication code is placed in the other three folders `01_create_training_dataset`, `02_model_training` and `03_inventor_composition_analysis`, whereas the numbering `0x_` indicates the running order of the code that roughly corresponds to the article structure.   

The folder `01_create_training_dataset` contains all the replication code to generate the **training dataset** for the LSTM classifier. `02_model_training` features scripts to train the **LSTM classifier** using `tensorflow`. The `03_inventor_composition_analysis` directory contains scripts for predicting the ethnic origins of a large sample of patent inventors and the subsequent **descriptive analyses**. These computations were performed at sciCORE (http://scicore.unibas.ch/) scientific computing core facility at University of Basel. Since the corresponding patent data from the Center for International Business and Economics has been cleaned and enriched with additional data, it is partly proprietary and cannot be made publicly accessible as such. However patent inventor information can be gathered upon registration from either the [USPTO](https://patentsview.org/) or the [OECD](https://www.oecd.org/sti/intellectual-property-statistics-and-analysis.htm), and I further provide a random sub-sample of the dataset (`00_data_and_model/data/05_example_inventors_dat.csv`) used in this article to demonstrate the replication of all descriptive results. The correpsonding code can be found in the folder `03_inventor_composition_analysis/`

## Classifier Usage
The trained LSTM classifier, which is at the core of the analysis in this paper, can be accessed via a preliminary python package that is available on [pipy](https://pypi.org/project/ntec/). The respective source code can be accessed at [this GitHub repository](https://github.com/matthnig/ntec). Details on the classifier's training are available in the paper and the respective code is in the folder `02_model_training/`.

## Dependencies
All code is either written in `Python` (Version 3.7.) or `R` (Version 4.0.1). Corresponding Python libraries can be found in the `environment.yml` or `requirements.txt` files (installing a virtual environment is recommended). `R` packages needed for replication are: `tidyverse`, `data.table`, `reticulate`, `tensorflow`, `keras`, `stringi`, `jsonlite`, `countrycode`, and `viridis` and can be installed from CRAN.

## References & Contact
Matthias Niggli (2023), ‘Moving On’—investigating inventors’ ethnic origins using supervised learning, *Journal of Economic Geography*, lbad001, [https://doi.org/10.1093/jeg/lbad001](https://doi.org/10.1093/jeg/lbad001).

BibTex:
```
@article{niggli2023,
    author = {Niggli, Matthias},
    title = "{‘Moving On’—investigating inventors’ ethnic origins using supervised learning}",
    journal = {Journal of Economic Geography},
    year = {2023},
    month = {01},
    abstract = "{Patent data provides rich information about technical inventions, but does not disclose the ethnic origin of inventors. In this article, I use supervised learning techniques to infer this information. To do so, I construct a dataset of 96′777 labeled names and train an artificial recurrent neural network with long short-term memory (LSTM) to predict ethnic origins based on names. The trained network achieves an overall performance of 91.4\\% across 18 ethnic origins. I use this model to predict and investigate the ethnic origins of 2.68 million inventors and provide novel descriptive evidence regarding their ethnic origin composition over time and across countries and technological fields. The global ethnic origin composition has become more diverse over the last decades, which was mostly due to a relative increase of Asian origin inventors. Furthermore, the prevalence of foreign-origin inventors is especially high in the USA, but has also increased in other high-income economies. This increase was mainly driven by an inflow of non-Western inventors into emerging high-technology fields for the USA, but not for other high-income countries.}",
    issn = {1468-2702},
    doi = {10.1093/jeg/lbad001},
    url = {https://doi.org/10.1093/jeg/lbad001},
    note = {lbad001},
    eprint = {https://academic.oup.com/joeg/advance-article-pdf/doi/10.1093/jeg/lbad001/48958974/lbad001.pdf},
}
```

If you have questions, please contact me at matthiasniggli@gmx.ch.