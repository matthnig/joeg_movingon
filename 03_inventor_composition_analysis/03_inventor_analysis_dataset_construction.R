#################################################################
# Description:    Script to combine predicted inventor origins  #
#                 with location and techfield information       #
# Authors:        Matthias Niggli/CIEB UniBasel                 #
# Last revised:   12.09.2022                                    #
#################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")
library("data.table")
library("stringi")

# directories  -----------------------------------------------------------------
DatDir <- "..." # CIEB University of Basel patent data
path = "..." # your path to the repository directory
setwd(path)

#####################################################
## Load inventor origin predictions from the model ##
#####################################################

# Load 3.4 Mio. registered patent inventor names and their predicted origins created in '02_LSTM_inventor_classification.R'
inv_dat <- readRDS(paste0(DatDir, "/created data/inventor_origin_joeg_revision.rds")) # N = 3'402'686

# select origin predictions
INV_VARS <- c("name", "origin", "prob_AngloSaxon", "prob_Arabic",
              "prob_Balkans", "prob_China", "prob_EastEurope", "prob_French",
              "prob_German", "prob_Hispanic-Iberian", "prob_India", "prob_Italian",
              "prob_Japan", "prob_Korea", "prob_Persian", "prob_Scandinavian",
              "prob_Slavic-Russian", "prob_SouthEastAsia", "prob_Turkey", 
              "prob_Dutch")
inv_dat <- inv_dat[, INV_VARS]
PAT_INVS <- unique(inv_dat$name) # N = 3'342'528
inv_dat <- setDT(inv_dat, key = "name")

############################################################
## Load inventor information (year, residence, techfield) ##
############################################################

# load CIEB data on location priority year and only consider unique name-year-techfield combinations
inv_reg <- readRDS(paste0(DatDir, "/created data/inv_reg.rds")) # N = 20'056'152 entries
inv_reg <- inv_reg[, c("name", "p_year", "Ctry_code", "tech_field", "Up_reg_code", "Up_reg_label")] 

# only consider distinct name-year-techfield combinations:
inv_reg <- inv_reg %>% 
  distinct(name, p_year, Ctry_code, tech_field, .keep_all = TRUE) # N = 13'877'328 entries

##### clean names --------------------------------

# transform everything to lowercase letters and remove punctuation
inv_reg$name <- tolower(inv_reg$name)
inv_reg$name <- gsub("[[:punct:]]", "", inv_reg$name)
inv_reg$name <- gsub("[0-9]", "", inv_reg$name)

# drop NA names from sample
NA_obs <- which(is.na(inv_reg$name) == TRUE)
if(length(NA_obs) > 0){inv_reg <- inv_reg[-NA_obs, ]}
if(length(inv_reg$name[is.na(inv_reg$name) == TRUE]) != 0){
  warning("Some first names in the sample are NA")}else{
    print("Ready to proceed. No names are NA")}

# identify all letters in the sample and replace special characters
special_chars <- gsub(" ", replacement = "", inv_reg$name)
special_chars <- stri_extract_all(str = special_chars, regex = "[^a-z]")
special_chars <- unlist(special_chars)
special_chars <- unique(special_chars)
special_chars <- special_chars[is.na(special_chars) == FALSE]
special_chars # encoded in UTF-8
repl_vec <- iconv(special_chars, from = "UTF-8", to = "ASCII//TRANSLIT")
repl_vec <- data.frame(spec = special_chars, rep = repl_vec)
repl_vec[which(repl_vec$rep == "?"), ]
repl_vec[which(repl_vec$rep == "?"), ]$rep <- c("o", "a", "b", "th", "t", "d",
                                                "l", "e", "th")
repl_vec

# test:
if(special_chars[25] != "ì" | special_chars[49] != "ą"){warnings(
  "Replacemnet of special characters with an error")}else{
    repl_vec <- repl_vec$rep
    print("Ready to clean special characters")}

clear_chars <- function(names, special_chars, repl_vec){
  
  # encode all special chars to a regex pattern
  sc <- paste0(special_chars, collapse = "|")
  
  # find those names in the name list with one of these special chars
  repl_names <- grepl(sc, x = names)
  repl_names <- which(repl_names == TRUE)
  
  # replace special characters in these names
  tmp <- unlist(lapply(names[repl_names],
                       function(x) stri_replace_all_fixed(str = x,
                                                          pattern = special_chars,
                                                          replacement = repl_vec,
                                                          vectorize_all = FALSE)))
  
  # replace the uncleaned names with the cleaned ones
  names[repl_names] <- tmp
  return(names)
}

inv_reg$name <- clear_chars(names = inv_reg$name,
                            special_chars = special_chars,
                            repl_vec = repl_vec)
print("Cleaning of names successfull")

#################################################
## Combine datasets with inventor informations ##
#################################################

# merge both datasets on inventor name
inv_reg <- setDT(inv_reg, key = "name")
inv_reg <- inv_reg[name %in% PAT_INVS, ] # N = 10'138'108
PAT_INVS <- NULL
inv_dat <- merge.data.table(inv_reg, inv_dat, by = "name", all = TRUE) # 10'912'225
inv_reg <- NULL

# only keep observations with complete information expect for tech_field and region:
complete_cols <- names(inv_dat)[!names(inv_dat) %in% c("tech_field", "Up_reg_code", "Up_reg_label")]
inv_dat <- inv_dat %>% drop_na(all_of(complete_cols))

# summarise
N_inv_unique <- length(unique(inv_dat$name)) # N = 2'680'775 
N_entries <- nrow(inv_dat) # N = 8'543'459
paste("Final dataset ready for analysis. Contains", N_inv_unique, 
      "inventors with", N_entries, "of entries.")

###############################
## Save the combined dataset ##
###############################

#### full sample:
# saveRDS(inv_dat, "/scicore/home/weder/nigmat01/Data/inv_origin_dat_joeg_revision.rds")

#### random sample for replication:
# set.seed(27122021)
# ex_dat <- inv_dat[sample(seq(nrow(inv_dat)), 20000), ]
# write.csv(ex_dat, paste0(path, "00_data_and_model/data/05_example_inventors_dat.csv" ))



