#######################################################################
# Description:    Script to retrieve name-prism predictions for a     #
#                 sample consisting of olympic athletes name's which  #
#                 are labeled to ethnic origins.                      #
# Authors:        Matthias Niggli/CIEB UniBasel                       #
# Date:           25.02.2021                                          #
#######################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")

# packages for prism-name API: -------------------------------------------------
library("jsonlite")

# directories--------------
path <- "..." # your path to the repository directory
setwd(path)

# reproducibility ----------------
set.seed(24012021)

#########################
## Load & process data ##
#########################

## olympic athletes:
df_olympic <- read.csv("/00_data_and_model/data/01_athlete_sample.csv")

## Specify desired number of samples per ethnic origin:
min_N <- 750

## sampled names with name-prism information from previous project -------
pre_accessed <- read.csv("/scicore/home/weder/nigmat01/Data/nameprism_inventor_random_sample.csv")
pre_accessed <- pre_accessed %>% mutate(origin = case_when(origin == "Slawic" ~ "Slavic-Russian",
                                                           origin == "Hispanic" ~ "Hispanic-Iberian",
                                                           origin == "Portugese" ~ "Hispanic-Iberian",
                                                           !origin %in% c("Slawic", "Hispanic", "Portugese") ~ origin))
pre_dist <- pre_accessed %>% group_by(origin) %>% 
  summarise(count = n(), share = count / nrow(pre_accessed), add_N = min_N - count)

# subset to names that have not yet been predicted by NamePrism
test_sample <- df_olympic %>% filter(!Name %in% pre_accessed$Name)
test_sample <- test_sample[sample(nrow(test_sample), nrow(test_sample)),]
tmp <- test_sample %>% filter(origin == "Korea") %>% sample_n(min_N)

# sample names to reach min_N inventors for all ethnic origins
test_sample <- lapply(unique(pre_dist$origin), function(x){
  N_obs <- pre_dist[pre_dist$origin == x, ]$add_N
  tmp <- test_sample %>% filter(origin == x)
  if(N_obs > nrow(tmp)){N_obs <- nrow(tmp)}
  tmp <- tmp %>% sample_n(N_obs)
  return(tmp)
})
test_sample <- bind_rows(test_sample)
test_sample <- rbind(test_sample, tmp)
tmp <- NULL

# check projected ethnic origin distribution for training data:
tmp <- test_sample %>% group_by(origin) %>%
  summarise(count = n(), share = count / nrow(test_sample))
left_join(tmp, pre_dist, by = "origin") %>% mutate(count = count.x + count.y) %>% select(origin, count)
tmp <- NULL

###########################
## Access name-prism API ##
###########################

# encode names to access NamePrism
test_sample$full_name_encoded <- gsub(" ", "%20", x = test_sample$Name)

## specify the API-Token and URL -----------------------------------------------
API_nameprism <- "XXXXXXXXXXXXXXXXXXXXXXX" # your token from nameprism. Get it at: https://name-prism.com/api
pred_type = "nat"
response_format = "json"

# get the 39 different origins from the API
for(i in 5:43){test_sample[, i] <- NA}
api_url <- paste("http://www.name-prism.com/api_token/",
                 pred_type, "/", response_format, "/",
                 API_nameprism, "/",
                 "test%20name", sep = "")
origins <- names(fromJSON(txt = api_url))
names(test_sample)[5:43] <- origins
print("Prepared dataframe to store origin predictions from name-prism.")

## !!! NOT RUN: !!!!----------------------------

# Get name-prism predictions for all names
# for(i in 1:nrow(test_sample)){
#   api_url <- paste0("http://www.name-prism.com/api_token/",
#                     pred_type, "/", response_format, "/",
#                     API_nameprism, "/", test_sample$full_name_encoded[i])
#   tmp <- fromJSON(txt = api_url)
#   tmp <- unlist(tmp)
#   test_sample[i, 5:43] <- tmp
#   if(i %in% c(10, 100, 500, seq(1000, 9000, by = 1000))){print(paste("Retrieved predictions for", i, "names"))}
#   Sys.sleep(0.2) # without Sys.sleep: ~0.5s per loop
# }

print(paste("Origin predictions for", nrow(test_sample), "names retrieved"))

###################################
## Add previously accessed names ##
###################################

tmp <- test_sample
names(tmp) <- gsub(",", ".", names(tmp))
names(tmp)[40] <- "EastAsian.South.Korea"
tmp <- rbind(tmp, pre_accessed)
tmp %>% group_by(origin) %>%
  summarise(count = n(), share = count / nrow(tmp))

###############################
## Save the verification set ##
###############################

#write.csv(x = tmp, file = "/00_data_and_model/data/03_athletes_nameprism.csv", row.names = FALSE)



