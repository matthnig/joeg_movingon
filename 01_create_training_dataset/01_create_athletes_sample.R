#######################################################################
# Description:    Script to construct a dataset consisting of olympic #
#                 athletes name's which are labeled to ethnic origins #
# Authors:        Matthias Niggli/CIEB UniBasel                       #
# Date:           05.07.2021                                          #
#######################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("dplyr")
library("countrycode")
library("stringi")

# directories ----------------------
datDir <- "..." # your directory where the raw athletes data from Kaggle is stored. Download at: https://www.kaggle.com/datasets/heesoo37/120-years-of-olympic-history-athletes-and-results
path <- "..." # your path to the repository directory
setwd(path)

print("Directories specified")

# reproducibility
set.seed(24012021)

#########################
## Load & process data ##
#########################

#### Olympic Athletes data
df_olympic <- read.csv(paste0(datDir, "/athlete_events.csv")) # raw athletes data from Kaggle
df_olympic <- df_olympic %>% distinct(ID, .keep_all = TRUE) %>% 
  select(Name, Team, Year)
df_olympic <- filter(df_olympic, 
                     Team %in% c("Great Britain", 
                                 unique(countrycode::codelist$country.name.en))
                     )

#### create dictionary to assign nationalities to ethnic origins: -----------------

origins <- c(rep("Slavic-Russian", 3), 
             rep("EastEurope", 3), 
             rep("Balkans", 3), 
             rep("Arabic", 8), 
             "Italian", "French", 
             rep("SouthEastAsia", 6), 
             rep("Scandinavian", 5), 
             "Persian",
             "Dutch-Flemish",
             rep("Hispanic-Iberian", 3), 
             "Japan", 
             "German", 
             "China", 
             "India", 
             "Turkey", 
             rep("AngloSaxon", 2), 
             rep("Korea", 2))

countries <- c("Russia", "Ukraine", "Belarus",
               "Poland", "Hungary", "Czechoslovakia", 
               "Yugoslavia", "Serbia", "Croatia",
               countrycode(c("SY", "EG", "SA", "JO", "AE", "MA", "DZ", "TN"), origin = "iso2c",
                           destination = "country.name.en"),
               "Italy", 
               "France",
               c(countrycode(c("ID", "VN", "TH", "MY"), origin = "iso2c",
                           destination = "country.name.en"), "Laos", "Cambodia"),
               countrycode(c("FI", "SE", "DK", "NO", "IS"),origin = "iso2c",
                           destination = "country.name.en"),
               "Iran",
               "Netherlands",
               "Spain", "Mexico", "Portugal",
               "Japan", 
               "Germany", 
               "China", 
               "India", 
               "Turkey", 
               "Ireland", "Great Britain", 
               "South Korea", "North Korea")
  
ref_list <- data.frame(origin = origins, country = countries)

# subset to athletes from these countries and assign ethnic origin:
df_olympic <- df_olympic %>% filter(Team %in% ref_list$country) %>% rename(country = Team)
df_olympic <- left_join(df_olympic, ref_list, by = "country")            

#### subset athletes from immigrant countries to pre-immigration periods: 
IMMIGRANT_COUNTRIES <- c("Great Britain", "Germany", "France", 
                         "Italy", "Norway", "Denmark", "Sweden", "Finland",
                         "Spain", "Portugal", "Netherlands")
MAX_YEARS <- c(1970, 1980, 1970, 1980, 1980, 1980, 1980, 1980, 1980, 1980, 1980)

res <- df_olympic %>% filter(!country %in% IMMIGRANT_COUNTRIES)
for (i in 1:length(IMMIGRANT_COUNTRIES)){
  tmp <- df_olympic %>% filter(country == IMMIGRANT_COUNTRIES[i] & 
                                 Year <= MAX_YEARS[i])
  res <- rbind(res, tmp)
}
df_olympic <- res
res <- NULL
df_olympic %>% group_by(origin) %>% summarize(count = n(),
                                              share = count / nrow(df_olympic))

#### Balance the ethnic origin distribution
# Scandinavia & East-European athletes are over-representeted 
# Downsample to ~ the mean number of western counties' shares
df_olympic %>% filter(origin %in% c("AngloSaxon", "German", "French", 
                                    "Hispanic-Iberian", "Italian",
                                    "Scandinavian")) %>%
  group_by(origin) %>% summarize(count = n())
downsample_fun <- function(df, area, N){
  tmp <- df %>% filter(origin == area)
  tmp <- tmp %>% sample_n(size = N)
  df <- filter(df, origin != area)
  df <- rbind(df, tmp)
  return(df)
}
df_olympic <- downsample_fun(df = df_olympic,
                             area = "EastEurope",
                             N = 3500)
df_olympic <- downsample_fun(df = df_olympic,
                             area = "Scandinavian",
                             N = 3500)
df_olympic %>% group_by(origin) %>% 
  summarize(count = n(),
            share = count/nrow(df_olympic)) %>%
  arrange(-count)

#### process athletes' names
df_olympic$Name <- trimws(df_olympic$Name)
df_olympic$Name <- tolower(df_olympic$Name)
df_olympic$Name <- gsub("[[:punct:]]", "", df_olympic$Name)
df_olympic$Name <- gsub("[0-9]", "", df_olympic$Name)
df_olympic$Name <- trimws(df_olympic$Name)

# check if there are non-Latin characters in names
special_chars <- gsub(" ", replacement = "", df_olympic$Name)
special_chars <- stri_extract_all(str = special_chars, regex = "[^a-z]")
special_chars <- unlist(special_chars)
special_chars <- unique(special_chars)
special_chars <- special_chars[is.na(special_chars) == FALSE]
if(length(special_chars) == 0){print("No non-Latin characters in names")}

#### save the data for training the models: -------------------------------------
write.csv(df_olympic %>% select(-country), 
         file = "00_data_and_model/data/01_athlete_sample.csv", row.names = FALSE)



