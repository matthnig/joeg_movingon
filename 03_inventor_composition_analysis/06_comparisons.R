#################################################################
# Description:    Script to compare descriptive statistics of   #
#                 the inventor distribution to other studies    #
# Authors:        Matthias Niggli/CIEB UniBasel                 #
# Last revised:   01.11.2022                                    #
#################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")
library("countrycode")
library("pals")
library("ipumsr")

# directories  -----------------------------------------------------------------
path = "..." # your path to the repository directory

###################
## Load the data ##
###################

#### CIEB patent inventor information
inv_dat <- read.csv(paste0(path, "/00_data_and_model/data/05_example_inventors_dat.csv"))[-1] # example dataset
source("03_inventor_composition_analysis/comparison_functions.R")

###################################################################
################# FIGURE F.1: COMPARE TO KERR (2008) ###############
###################################################################

#### Kerr (2008) distribution of US ethnic patenting
kerr_dat <- read.csv(paste0(path, "/00_data_and_model/data/kerr_data.csv"), sep = ";")
ORIGINS <- c("AngloSaxon", "European",
             "China", "Hispanic-Iberian", "India", 
             "Japan", "Korea", "Slavic-Russian")
MIN_PERIOD <- 1980
names(kerr_dat) <- c("country","industry","period",
                     "AngloSaxon","China", "European",
                     "Hispanic-Iberian","India", "Japan",
                     "Korea","Slavic-Russian", "Vietnamese", 
                     "source")

kerr_dat <- kerr_dat %>% 
  gather("origin", "origin_share", -country, -industry, -period, -source) %>%
  filter(origin %in% ORIGINS & industry == "overall") %>%
  mutate(source = "kerr_2008")
kerr_dat$period <- as.numeric(unlist(lapply(strsplit(kerr_dat$period, split = "_"), function(x)x[[1]])))

# prepare inventor data: ----------------------------
dot_cols <- which(grepl(pattern = ".", x = names(inv_dat), fixed = TRUE))
names(inv_dat)[dot_cols] <- gsub("\\.", "-", names(inv_dat)[dot_cols])
PERIODS <- c(unique(kerr_dat$period), 2005, 2010)
inv_dat <- assgin_periods(periods = PERIODS, df = inv_dat, year_var = "p_year")

# filter to unique inventors per period
plot_df <- inv_dat %>% 
  filter(Ctry_code == "US" & period %in% unique(kerr_dat$period)) %>%
  distinct(name, period, .keep_all = TRUE)

# calculate prevalence for origins of interest
KERR_EUROPEAN_ORIGINS <- c("French","Italian","German", "Dutch")
tmp <- inv_preval_kerr(df = plot_df, countries = "US",
                           origins = c(ORIGINS, KERR_EUROPEAN_ORIGINS),
                           min_period = MIN_PERIOD) # prevalence
tmp <- tmp %>%
  mutate(origin = case_when(origin %in% KERR_EUROPEAN_ORIGINS ~ "European",
                            TRUE ~ origin)) %>%
  group_by(period, country, origin) %>%
  summarise(origin_share = sum(origin_share)) %>%
  mutate(period = as.numeric(period))
tmp$source <- "this_paper"
plot_df <- rbind(tmp, kerr_dat[names(tmp)]) %>% filter(period >= 1980)

# combine and evaluate
corr <- plot_df %>%
  as.data.frame() %>%
  filter(source == "kerr_2008") %>%
  select(period, origin, origin_share) %>%
  rename(kerr_share = origin_share) %>%
  merge(plot_df %>%
          as.data.frame() %>%
          filter(source == "this_paper") %>%
          select(period, origin, origin_share) %>%
          rename(prevalence = origin_share),
        by = c("period", "origin")) %>%
  summarise(corr = cor(prevalence, kerr_share)) %>%
  pull()
paste("Correlation of period origin shares and period origin prevalence:", round(corr, 3)) # 0.947

# rename  ethnic classes and plot
plot_df <- mutate(plot_df, 
                  origin = case_when(
                    origin == "AngloSaxon" ~ "English",
                    origin == "China" ~ "Chinese",
                    origin == "Hispanic-Iberian" ~ "Hispanic",
                    origin == "India" ~ "Indian",
                    origin == "Korea" ~ "Korean",
                    origin == "Japan" ~ "Japanese",
                    origin == "Slavic-Russian" ~ "Russian",
                    TRUE ~ origin)) 

ggplot(plot_df, aes(x = as.character(period), y = origin_share, fill = source))+
  facet_wrap(.~ origin, scales = "free", ) +
  geom_col(position = "dodge")+
  labs(x = "Period", y = "Ethnic Origin Prevalence", fill = "Source")+
  scale_y_continuous(n.breaks = 3, labels = scales::percent) +
  scale_x_discrete(labels = c("1980-1984", "1985-1989", "1990-1994", "1995-1999", "2000-2004")) +
  scale_fill_manual(values = as.vector(glasbey(2)), labels = c("Kerr (2008)", "This paper"))+
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 22.5, hjust = 1),
        axis.line = element_line(),
        axis.title = element_text(face="bold",size=10))
# ggsave("/scicore/home/weder/nigmat01/paper_plots/joeg_revision_plots/kerr_comparison.png")

##########################################################
################# Figure F.2.: USA 2015 ACS ##############
##########################################################

# load the data extracted from IPUMS:
ipums_ddi <- read_ipums_ddi("/scicore/home/weder/nigmat01/paper_plots/joeg_revision_plots/usa_00003.xml")
ipums_dat <- read_ipums_micro(ipums_ddi)

# assign ancestry codes to ethnic origins:
sort(unique(inv_dat$origin))
ancestry_to_origin <- list()
ancestry_to_origin[["AngloSaxon"]] <- c(11, 12, 50, seq(900, 994))
ancestry_to_origin[["Arabic"]] <- c(seq(400, 415), seq(421,429), seq(435,496))
ancestry_to_origin[["Balkans"]] <- c(109, 152, 176)
ancestry_to_origin[["China"]] <- seq(706, 718)
ancestry_to_origin[["Dutch"]] <- c(21)
ancestry_to_origin[["EastEurope"]] <- c(142, 111, 125, 126, 153)
ancestry_to_origin[["French"]] <- c(26, 27, 28)
ancestry_to_origin[["German"]] <- seq(32, 43)
ancestry_to_origin[["Hispanic-Iberian"]] <- c(seq(200, 296), 360, 84, 85, 86)
ancestry_to_origin[["India"]] <- seq(615, 675)
ancestry_to_origin[["Italian"]] <- seq(51, 73)
ancestry_to_origin[["Japan"]] <- seq(740, 748)
ancestry_to_origin[["Korea"]] <- c(750)
ancestry_to_origin[["Persian"]] <- c(416)
ancestry_to_origin[["Scandinavian"]] <- c(20,23,24,49,82,89,98)
ancestry_to_origin[["Slavic-Russian"]] <- c(102, 148, 150, 164, 171, 178, 179)
ancestry_to_origin[["SouthEastAsia"]] <- c(703, 770, seq(767,790), 730, seq(765, 768))
ancestry_to_origin[["Turkey"]] <- c(434)

# sample share of 'Not reported'
ipums_dat %>% 
  group_by(ANCESTR1) %>% 
  summarise(count = n()) %>% 
  mutate(share = scales::percent(count / sum(count))) %>%
  filter(ANCESTR1 >= 995) %>%
  arrange(-count) %>%
  head(20)

res = data.frame()
for (e in names(ancestry_to_origin)) {
  N <- length(ancestry_to_origin[[e]])
  tmp <- data.frame(origin = rep(e, N), ipums_codes = ancestry_to_origin[[e]])
  res <- rbind(res, tmp)
}

# calculate share of all origins based on ipums:
plot_df = data.frame()
for (l in c("ANCESTR1", "ANCESTR2")) {
  for (ethn in unique(res$origin)) {
    origin_codes <- filter(res, origin == ethn) %>% pull(ipums_codes)
    N_people = ipums_dat %>% 
      filter_at(.vars = l, ~. %in% origin_codes) %>% 
      summarise(n_persons_count = n(),
                n_persons_weight = sum(PERWT))
    tmp = data.frame(origin = ethn, 
                     N_people_count = N_people$n_persons_count, 
                     N_people_weight = N_people$n_persons_weight,
                     src_level = l)
    plot_df <- rbind(plot_df, tmp)
  }
}
plot_df <- plot_df %>% mutate(ancestry_weight = ifelse(src_level == "ANCESTR1", 0.9, 0.1))
plot_df <- plot_df %>% group_by(origin) %>% 
  summarise(N_people_count = sum(ancestry_weight * N_people_count),
            N_people_weight = sum(ancestry_weight * N_people_weight)) %>%
  mutate(ethnic_share_count = N_people_count / sum(N_people_count),
         ethnic_share_weight = N_people_weight / sum(N_people_weight),
         source = "IPUMS")

# calculate the same for this paper:
tmp <- inv_dat %>% 
  filter(Ctry_code == "US" & p_year == 2015) %>%
  distinct(name, p_year, .keep_all = TRUE) %>%
  select(-period) %>% rename(period = p_year)
tmp <- inv_preval_kerr(df = tmp, countries = "US",
                       origins = unique(inv_dat$origin),
                       min_period = 2015) %>%
  select(-period, -country) %>%
  mutate(source = "This paper") %>% 
  rename(ethnic_share_count = origin_share)

# combine:
plot_df <- rbind(plot_df[, names(tmp)], tmp)

# correlation estimate across derived ethnic shares
corr <- plot_df %>%
  as.data.frame() %>%
  filter(source == "IPUMS") %>%
  select(origin, ethnic_share_count) %>%
  rename(ipums_share = ethnic_share_count) %>%
  merge(plot_df %>%
          as.data.frame() %>%
          filter(source == "This paper") %>%
          select(origin, ethnic_share_count) %>%
          rename(prevalence = ethnic_share_count),
        by = c("origin")) %>%
  summarise(corr = cor(prevalence, ipums_share)) %>%
  pull()
paste("Correlation of period origin shares and period origin prevalence:", round(corr, 3)) 
# overall: 0.867

# plot
YLABEL <- "Ethnic Origin Prevalence"
ggplot(plot_df, aes(x = origin, y = ethnic_share_count, fill = source))+
  geom_col(position = "dodge")+
  labs(x = "Ethnic Origin", y = YLABEL, fill = "Source")+
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.6))+
  scale_fill_manual(values = as.vector(glasbey(2)), labels = c("IPUMS ACS 2015", "This paper"))+
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
        legend.position = "bottom",
        axis.line = element_line(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(face="bold",size=10))
# ggsave("/scicore/home/weder/nigmat01/paper_plots/joeg_revision_plots/acs_comparison.png")

##########################################################
################# Figure F.3.: EU 2011 Census ##############
##########################################################

# inventor data: ---------------------------
unique(inv_dat$origin)
plot_df <- inv_dat %>%
  filter(Ctry_code %in% c("DE", "FR", "GB", "IT") & p_year == 2011) %>%
  distinct(name, Ctry_code, .keep_all = TRUE) 
plot_df <- plot_df%>%
  group_by(Ctry_code) %>% 
  summarise(annual_total = n()) %>%
  merge(plot_df, by = "Ctry_code")
plot_df <- plot_df %>% 
  group_by(Ctry_code) %>% 
  select(contains("prob")) %>%
  summarise_all(.funs = sum) %>%
  merge(plot_df %>% select(Ctry_code, annual_total) %>% distinct(Ctry_code, .keep_all = TRUE), 
        by = "Ctry_code")
plot_df <- gather(plot_df, key = "origin", value = "weighted_N", -annual_total, -Ctry_code)
plot_df$origin <- gsub("prob_", "", plot_df$origin)
plot_df <- plot_df %>% mutate(
  broad_origin = case_when(
    origin %in% c("AngloSaxon", "German", "Italian", "Dutch", "French", "AngloSaxon", 
                  "EastEurope", "Scandinavian", "Hispanic-Iberian") ~ "EU, North America & Oceania",
    origin %in% c("Korea", "Japan", "China", "Persian", "India", "SouthEastAsia", "Arabic") ~ "Asia & Africa",
    origin %in% c("Turkey", "Slavic-Russian", "Balkans") ~ "Other Europe")) %>%
  group_by(broad_origin, Ctry_code) %>%
  summarise(weighted_N = sum(weighted_N), total = mean(annual_total), origin_share = weighted_N / total) %>%
  mutate(source = "this_paper")
plot_df <- plot_df[, c("Ctry_code", "broad_origin", "origin_share", "source")]

# eu census data: ---------------------------
tmp <- read.csv("00_data_and_model/data/eu_census_HC34_2022_08_11_11_44.csv")
tmp <- tmp %>% filter(POB == "TOTAL") %>% 
  group_by(GEO) %>% 
  summarise(total = sum(WERT)) %>%
  merge(tmp) %>%
  filter(POB != "TOTAL") %>%
  group_by(GEO, POB) %>%
  summarise(WERT = sum(WERT), total = mean(total)) %>%
  select(GEO, POB, WERT, total) %>%
  mutate(broad_origin = case_when(
    POB %in% c("AFR", "ASI") ~ "Asia & Africa",
    POB == "EUR_NEU" ~ "Other Europe",
    POB %in% c("NAT", "EU_OTH", "AME_N", "OCE") ~ "EU, North America & Oceania")
  ) %>%
  group_by(GEO, broad_origin) %>%
  summarise(N = sum(WERT), total = mean(total)) %>%
  mutate(origin_share = N / total, source = "eu_census_2011") %>%
  rename(Ctry_code = GEO)
tmp <- tmp[, c("Ctry_code", "broad_origin", "origin_share", "source")]
plot_df <- rbind(plot_df, tmp)
plot_df$Ctry_code <- gsub("UK", "GB", plot_df$Ctry_code) 

# plot: ---------------------------
YLABEL <- "Origin Shares for High-Skilled Pepole \n and Inventor Origin Prevalence"
ggplot(plot_df, aes(x = broad_origin, y = origin_share, fill = source, 
                    # label = scales::percent(origin_share, accuracy = 1)
))+
  facet_wrap(.~ Ctry_code) +
  geom_col(position = "dodge")+
  labs(x = "Ethnic Origin Region / Country of Birth", y = YLABEL, fill = "Source")+
  scale_y_continuous(labels = scales::percent) +#, limits = c(0, 0.1))+
  scale_fill_manual(values = as.vector(glasbey(2)), labels = c("EU Census 2011", "This paper"))+
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
        legend.position = "bottom",
        axis.line = element_line(),
        axis.title = element_text(face="bold",size=10))
# ggsave("/scicore/home/weder/nigmat01/paper_plots/joeg_revision_plots/eu_census_comparison.png")

####################################################################
################# Define stock of patent inventors   ###############
################# in selected destination countries  ###############
################# based on bilateral immigrant flows ###############
####################################################################

# create a reference dictionary for iso2-codes, countries and ethnic origins
origins <- c(rep("Slavic-Russian", 3), 
             rep("EastEurope", 3), 
             rep("Balkans", 3), 
             rep("Arabic", 8), 
             "Italian", "French", 
             rep("SouthEastAsia", 6), 
             rep("Scandinavian", 5), 
             "Persian",
             "Dutch",
             rep("Hispanic-Iberian", 3), 
             "Japan", 
             "German", 
             "China", 
             "India", 
             "Turkey", 
             rep("AngloSaxon", 3), 
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
               "Ireland", "Great Britain", "United States", 
               "South Korea", "North Korea")
iso2 <- c(countrycode(countries, "country.name.en", "iso2c"))
iso2[is.na(iso2)] <- c("CS", "YU") # manually refine errors according to WIPO documentation
COUNTRY_DICT <- data.frame(origin = origins, country = countries, iso2 = iso2)

##############################################################################
################# FIGURE F.4. (Appendix): COMPARE TO WIPO (2013) #############
##############################################################################

#### WIPO inventor information used in Fink & Miguelez (2013)
# The database can be downloaded from WIPO: https://www.wipo.int/publications/en/details.jsp?id=3952
# The data is described in Miguelez, E and C. Fink (2013), Measuring the international mobility of inventors: A new database, WIPO Economics & Statistics Series 8
WIPO_PATH <- "/scicore/home/weder/GROUP/Innovation/01_patent_data/raw data/wipo_inventor_nationality_database/"
wipo_annual_stocks <- read.csv(paste0(WIPO_PATH, 
                                      "raw_files/13. Stocks of nationals, immigrants, residents, emigrants.csv"))
wipo_bilalteral_flows <- read.csv(paste0(WIPO_PATH, 
                                         "raw_files/1. Bilateral flows.csv"))

# assign periods to WIPO annual data:
wipo_annual_stocks <- assgin_periods(periods = PERIODS, df = wipo_annual_stocks, year_var = "prio_year")
wipo_bilalteral_flows <- assgin_periods(periods = PERIODS, df = wipo_bilalteral_flows, year_var = "prio_year")

# calculate number of immigrants per period and get cumulative number:
DESTINATION_COUNTRIES <- c("GB", "DE", "FR", "IT")
ORIGINS <- unique(inv_dat$origin)
MIN_YEAR <- 1990 # no earlier data on resident stocks
MAX_YEAR <- 2009 # later years in ambiguous quality...

plot_df <- lapply(DESTINATION_COUNTRIES, function(destination_country){
  
  # get annual immigrant flows for selected countries and time window:
  tmp <- annual_immigrants(df = wipo_bilalteral_flows, country_dict = COUNTRY_DICT,
                           destination_country = destination_country,
                           min_year = MIN_YEAR, max_year = MAX_YEAR)
  
  # add ethnic origins and calculate stock of immigrants (assuming they didnt move out again):
  tmp <- immigrant_origin_stocks(df = tmp, periods = TRUE, 
                                 destination_country = destination_country,
                                 country_dict = COUNTRY_DICT)
  
  # add resident stocks and calculate ethnic origin shares
  # (these numbers are only available from 1990 onwards)
  tmp <- immigrant_origin_shares(df = tmp, periods = TRUE,
                                 destination_country = destination_country,
                                 min_year = MIN_YEAR, max_year = MAX_YEAR)
  
  tmp <- as.data.frame(tmp)
  
  return(tmp)}
)
plot_df <- bind_rows(plot_df) %>%
  filter(is.na(origin_share) == FALSE 
         & origin %in% ORIGINS 
         & period == 2005
         ) %>%
  rename(country = destination_country) %>%
  select(period, country, origin, 
         # immigrants, immigrant_stock, residents, # for checks
         origin_share) %>%
  mutate(source = "wipo")

# get LSTM pervalence estimates for the same time period and combine
tmp <- inv_dat %>% 
  filter(Ctry_code %in% DESTINATION_COUNTRIES & period %in% unique(plot_df$period)) %>%
  distinct(name, Ctry_code, period, .keep_all = TRUE)
tmp <- inv_preval_kerr(df = tmp, countries = DESTINATION_COUNTRIES, 
                       origins = ORIGINS, 
                       min_period = 2005) # prevalence
tmp$source <- "this_paper"

# combine
plot_df <- rbind(plot_df, tmp)
plot_df$source <- factor(plot_df$source, levels = c("wipo", "this_paper"))

# select origins of interest and discard dominant domestic due to graphical reasons
plot_df <- plot_df %>% 
  mutate(dominant_origin = case_when(
    country == "DE" ~ "German",
    country == "IT" ~ "Italian",
    country == "FR" ~ "French",
    country == "GB" ~ "AngloSaxon")
    ) %>%
  filter(origin != dominant_origin) # %>%

# plot
YLABEL <- "Cumulative Immigrant Inventor Share \nand Ethnic Origin Prevalence"
ggplot(plot_df, aes(x = origin, y = origin_share, fill = source))+
  facet_wrap(.~ country) +
  geom_col(position = "dodge")+
  labs(x = "Ethnic Origin", y = YLABEL, fill = "Source")+
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.15))+
  scale_fill_manual(values = as.vector(glasbey(2)), labels = c("WIPO", "This paper"))+
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(),
        axis.title = element_text(face="bold",size=10))
# ggsave(paste0("/scicore/home/weder/nigmat01/paper_plots/joeg_revision_plots/wipo_comparison_eu.png"))

# correlation
corr <- plot_df %>%
  as.data.frame() %>%
  filter(source == "wipo") %>%
  select(country, origin, origin_share) %>%
  rename(wipo_share = origin_share) %>%
  merge(plot_df %>%
          as.data.frame() %>%
          filter(source == "this_paper") %>%
          select(country, origin, origin_share) %>%
          rename(prevalence = origin_share),
        by = c("country", "origin")) %>%
  summarise(corr = cor(prevalence, wipo_share)) %>%
  pull()
paste("Correlation of period origin shares and period origin prevalence:", round(corr, 3)) 
# 0.79

