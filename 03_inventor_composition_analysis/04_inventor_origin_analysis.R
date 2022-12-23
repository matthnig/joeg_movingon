#################################################################
# Description:    Script to evaluate the composition of patent  #
#                 inventors in different countries              #
# Authors:        Matthias Niggli/CIEB UniBasel                 #
# Last revised:   26.10.2022                                    #
#################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")
library("viridis")
library("pals")

# directories  -----------------------------------------------------------------
path = "..." # your path to the repository directory
setwd(path)

# Load helper functions for analysis -------------------------------------------
source("03_inventor_composition_analysis/inventor_analysis_functions.R")

###################
## Load the data ##
###################

# techfield names
techfield_grouping <- read.table(paste0(getwd(), "/00_data_and_model/data/techfield_names.txt"),
                                 header = TRUE, sep = ";")

# inventor information
inv_dat <- read.csv(paste0(path, "/00_data_and_model/data/05_example_inventors_dat.csv")) # example dataset

# summarise:
paste("Number of patent inventor entries: ", 
      nrow(inv_dat)) # Full sample: N = 8'543'459
paste("Number of unique patent inventor names: ", 
      length(unique(inv_dat$name))) # Full sample: N =  2'680'775

##############################################################
##### Figure 2: Global Distribution of Ethnic Origins ########
##############################################################

# specify dataset: only consider a name once per year:
plot_df <- inv_dat %>% distinct(name, p_year, .keep_all = TRUE)
plot_df <- origin_dist(df = plot_df)

ggplot(plot_df, aes(x = p_year, y = share, fill = origin))+
        geom_bar(stat = "identity", width = 1) +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_manual(values = as.vector(glasbey(18))) +
        guides(fill = guide_legend(ncol = 1))+
        labs(x = "Year", y = "Share Among Patent Inventors",
             fill = "Ethnic Origin")+
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              axis.title = element_text(face="bold",size=10))
# ggsave("/scicore/home/weder/nigmat01/paper_plots/joeg_revision_plots/origin_distribution.png",
#        width = 3231.6, height = 1521.6, units = "px")

################################################################################
######## Figure 3: Aggregate Prevalence of Non-Western Ethnic Origins ##########
################################################################################

# specify dataset: only consider a name once per year and country:
plot_df <- inv_dat %>% distinct(name, p_year, Ctry_code, .keep_all = TRUE)
COUNTRIES <- c("US", "GB", "FR", "DE", "JP", "IT")
NON_WESTERN_ORIGIN <- c("China", "India", "Slavic-Russian",
                        "Arabic", "Persian", "Turkey", "SouthEastAsia")
plot_df <- non_western_comparison(df = plot_df,
                                  countries = COUNTRIES, 
                                  origins = NON_WESTERN_ORIGIN)

ggplot(plot_df, aes(x = p_year, y = share, color = country, shape = country))+
        geom_line()+ geom_point() + scale_color_hue("Country")+
        scale_y_continuous(labels = scales::percent, limits = c(0, 0.32))+
        labs(y = "Share of Non-Western Origins", x = "Year",
             shape = "", color = "")+
        scale_color_manual(values = as.vector(glasbey(6))) +
        guides(color = guide_legend(nrow = 1))+
        theme(panel.background = element_blank(),
              panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
              legend.position = "bottom", legend.direction = "horizontal",
              axis.line = element_line(),
              axis.title = element_text(face="bold",size=10))
#ggsave("/scicore/home/weder/nigmat01/paper_plots/joeg_revision_plots/nonwestern_origin_selected_countries.png")

# Changes from 1980 to 2015
plot_df %>% filter(p_year %in% c(1980, 2015)) %>% 
        group_by(country) %>% 
        arrange(p_year) %>%
        summarize(change = diff(share)) %>%
        arrange(-change)

################################################################################
## Figure 4: Prevalence of Non-Western Ethnic Origin Across Technologies #######
################################################################################

# specify dataset: only consider a name once per year, country and technology:
plot_df <- inv_dat %>% distinct(name, p_year, Ctry_code, tech_field, .keep_all = TRUE)
COUNTRIES <- c("GB", "FR", "DE", "IT", "JP", "US")
NON_WESTERN_ORIGIN <- c("China", "India", "Slavic-Russian",
                        "Arabic", "Persian", "Turkey", "SouthEastAsia")#, "Balkans", "EastEurope")
TECHFIELDS <- c(4, 6, 8) # hot emerging fields
TECHFIELDS <- c(TECHFIELDS, c(27, 26, 32)) # low traditional fields
TECHFIELDS <- as.character(TECHFIELDS)

plot_df <- non_western_techfield(df = plot_df,
                                 countries = COUNTRIES, origins = NON_WESTERN_ORIGIN,
                                 techfields = TECHFIELDS, min_inventors = 30)
# add techfield names
plot_df <- merge(plot_df, techfield_grouping, by = c("tech_field"), all.x = TRUE)
ggplot(plot_df, aes(x = p_year, y = share, color = tech_field_name))+
        facet_wrap(.~country)+
        geom_line()+ 
        scale_color_hue("Technology Field")+
        scale_y_continuous(labels = scales::percent, limits = c(0, 0.6))+
        labs(y = "Shares of Non-Western Origins", x = "Year", color = "", shape = "")+
        scale_color_manual(values = as.vector(glasbey(6))) +
        guides(color = guide_legend(nrow = 2))+
        theme(panel.background = element_blank(),
              panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
              legend.position = "bottom", legend.direction = "horizontal",
              axis.line = element_line(),
              axis.title = element_text(face="bold",size=10))
#ggsave("/scicore/home/weder/nigmat01/paper_plots/joeg_revision_plots/non_western_techfields_All.png")

# Changes from 1980 to 2015
plot_df %>% filter(p_year %in% c(1980, 2015)) %>% 
        group_by(country, tech_field_name) %>% 
        arrange(p_year) %>%
        summarize(change = diff(share)) %>%
        arrange(-change) %>%
        View()

##########################################################################
##### Figure E6 & E7 (Appendix): China, Korea, India Distribution #######
##########################################################################

# specify dataset: only consider a name once per year:
plot_df <- inv_dat %>% distinct(name, p_year, .keep_all = TRUE)
ORIGINS <- c("China", "India", "Korea")
COUNTRIES <- c("CN", "IN", "KR")
plot_df <- abroad_domestic_fun(df = plot_df, ORIGINS = ORIGINS, COUNTRIES = COUNTRIES)

# visualize D3 --------------
ggplot(plot_df, aes(x = p_year, y = share, fill = domestic))+
  facet_wrap(.~origin)+
  geom_bar(stat = "identity", width = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = as.vector(glasbey(2))) +
  labs(x = "Year", y = "Share Among Patent Inventors",
       fill = "")+
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "bottom", legend.direction = "horizontal",
        axis.title = element_text(face="bold",size=10))
#ggsave("/scicore/home/weder/nigmat01/paper_plots/joeg_revision_plots/domestic_abroad.png")

# Changes from 1990 to 2015
plot_df %>% filter(p_year %in% c(1990, 2015) & domestic == "domestic") %>% 
  group_by(origin) %>% 
  arrange(p_year) %>%
  mutate(change = diff(share))

# visualize D4 --------------
ggplot(plot_df, aes(x = p_year, y = count, fill = domestic))+
  facet_wrap(.~origin)+
  geom_bar(stat = "identity", width = 1) +
  scale_fill_manual(values = as.vector(glasbey(2))) +
  labs(x = "Year", y = "Share Among Patent Inventors",
       fill = "")+
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "bottom", legend.direction = "horizontal",
        axis.title = element_text(face="bold",size=10))
#ggsave("/scicore/home/weder/nigmat01/paper_plots/joeg_revision_plots/domestic_abroad_absolute.png")

# Changes from 1990 to 2015
plot_df %>% filter(p_year %in% c(1990, 2015)) %>% 
  group_by(origin, domestic) %>% 
  arrange(p_year) %>%
  mutate(change = diff(count))

################################################################################
######## Figure E8 (Appendix): Prevalence of the Dominant Ethnic Origin ########
################################################################################

# specify dataset: only consider a name once per year and country
plot_df <- inv_dat %>% distinct(name, p_year, Ctry_code, .keep_all = TRUE)
COUNTRIES <- c("US", "GB", "FR", "DE", "JP", "IT")
DOMESTIC_ORIGIN <- list("AngloSaxon", "AngloSaxon", "French", 
                     "German", "Japan", "Italian")

plot_df <- dominant_domestic_comparison(df = plot_df,
                                        countries = COUNTRIES,
                                        domestic_origin = DOMESTIC_ORIGIN)

ggplot(plot_df, aes(x = p_year, y = dominant_share, color = country, shape = country))+
        geom_line()+ geom_point() + scale_color_hue("Country")+
        scale_y_continuous(labels = scales::percent, limits = c(0.3, 1))+
        labs(y = "Share of Dominant Domestic Origin", x = "Year",
             shape = "", color = "")+
        scale_color_manual(values = as.vector(glasbey(6))) +
        guides(color = guide_legend(nrow = 1))+
        theme(panel.background = element_blank(),
              panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
              legend.position = "bottom", legend.direction = "horizontal",
              axis.line = element_line(),
              axis.title = element_text(face="bold",size=10))
#ggsave("/scicore/home/weder/nigmat01/paper_plots/joeg_revision_plots/dominant_origin_selected_countries.png")

# Changes from 1980 to 2015
plot_df %>% filter(p_year %in% c(1980, 2015)) %>% 
        group_by(country) %>% summarize(change = diff(dominant_share)) %>%
        arrange(-change)

#############################################################################
#### Figure E9 (Appendix): Prevalence of Non-Western Ethnic Origins #########
#############################################################################

# specify dataset: only consider a name once per year and country
plot_df <- inv_dat %>% distinct(name, p_year, Ctry_code, .keep_all = TRUE)
COUNTRIES <- c("US", "GB", "FR", "DE", "IT", "JP")
NON_WESTERN_ORIGIN <- c("China", "India", "Slavic-Russian",
                        "Arabic", "Persian", "Turkey", "SouthEastAsia")

plot_df <- foreign_shares_fun(df = plot_df, 
                              COUNTRIES = COUNTRIES, 
                              ORIGIN = NON_WESTERN_ORIGIN)

ggplot(plot_df, aes(x = p_year, y = share, color = origin))+
        facet_wrap(.~ country)+ 
        geom_line()+
        labs(x = "Year", y = "Shares of Non-Western Ethnic Origins", color = "", shape = "")+
        scale_y_continuous(labels = scales::percent, limits = c(0, 0.125))+
        scale_color_manual(values = as.vector(glasbey(7))) +
        # scale_color_viridis(option = "viridis", begin = 0, end = 0.8, discrete = TRUE)+
        theme(panel.background = element_blank(),
              panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
              legend.position = "bottom",
              axis.line = element_line(),
              axis.title = element_text(face="bold",size=10))
#ggsave("/scicore/home/weder/nigmat01/paper_plots/joeg_revision_plots/foreign_origins_selected_countries.png")

# Changes from 1980 to 2015
plot_df %>% filter(p_year %in% c(1980, 2015)) %>%
        group_by(country, origin) %>% summarize(change = diff(share)) %>%
        arrange(-change)

##################################################################################################
### Figure E10 (Appendix): Prevalence of the Dominant Ethnic Origin in European Countries #########
##################################################################################################

# specify dataset: only consider a name once per year and country
plot_df <- inv_dat %>% distinct(name, p_year, Ctry_code, .keep_all = TRUE)
COUNTRIES <- c("CH", 
               "NL", 
               "SE", "DK", 
               "AT", 
               "ES", 
               "BE")
DOMESTIC_ORIGIN <- list(c("German", "Italian", "French"), 
                        c("German", "Dutch"),
                        #c("German", "AngloSaxon", "Scandinavian"), # previous proxy for Netherlands
                        "Scandinavian", "Scandinavian",
                        "German", 
                        "Hispanic-Iberian", 
                        c("Dutch", "German","French")
                        # c("German", "AngloSaxon", "French")# previous proxy for Belgium
                        )

plot_df <- dominant_domestic_comparison(df = plot_df, 
                                        countries = COUNTRIES,
                                        domestic_origin = DOMESTIC_ORIGIN)

plot_df <- plot_df %>% group_by(country, p_year) %>% 
        summarize(dominant_share = sum(dominant_share))

ggplot(plot_df, aes(x = p_year, y = dominant_share, color = country))+#, shape = country))+
        geom_line()+ geom_point()+ 
        scale_color_hue("Country")+
        scale_y_continuous(labels = scales::percent, limits = c(0, 1))+
        labs(y = "Share of Dominant Domestic Origins", x = "Year",
             shape = "", color = "")+
        scale_color_manual(values = as.vector(glasbey(7))) +
        guides(color = guide_legend(nrow = 1))+
        theme(panel.background = element_blank(),
              panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
              legend.position = "bottom", legend.direction = "horizontal",
              axis.line = element_line(),
              axis.title = element_text(face="bold",size=10))
# ggsave("/scicore/home/weder/nigmat01/paper_plots/joeg_revision_plots/dominant_origin_european_countries.png")

# Changes from 1980 to 2015
plot_df %>% filter(p_year %in% c(1980, 2015)) %>% 
        group_by(country) %>% summarize(change = diff(dominant_share)) %>%
        arrange(-change)

#########################################################################################################
### Figure E11 (Appendix): Prevalence of Aggregate Non-Western Ethnic Origins in European Countries ######
#########################################################################################################

# specify dataset: only consider a name once per year and country
plot_df <- inv_dat %>% distinct(name, p_year, Ctry_code, .keep_all = TRUE)
COUNTRIES <- c("CH", "NL", "SE", "DK", "AT", "ES", "BE")
NON_WESTERN_ORIGIN <- c("China", "India", "Slavic-Russian",
                        "Arabic", "Persian", "Turkey", "SouthEastAsia")#, "Balkans", "EastEurope")

plot_df <- non_western_comparison(df = plot_df,
                                  countries = COUNTRIES, 
                                  origins = NON_WESTERN_ORIGIN)

ggplot(plot_df, aes(x = p_year, y = share, color = country))+#, shape = country))+
        geom_line()+ geom_point() + scale_color_hue("Country")+
        scale_y_continuous(labels = scales::percent, limits = c(0, 0.3))+
        labs(y = "Share of Non-Western Origins", x = "Year",
             shape = "", color = "")+
        scale_color_manual(values = as.vector(glasbey(7))) +
        guides(color = guide_legend(nrow = 1))+
        theme(panel.background = element_blank(),
              panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
              legend.position = "bottom", legend.direction = "horizontal",
              axis.line = element_line(),
              axis.title = element_text(face="bold",size=10))
#ggsave("/scicore/home/weder/nigmat01/paper_plots/joeg_revision_plots/nonwestern_origin_european_countries.png")

# Changes from 1980 to 2015
plot_df %>% filter(p_year %in% c(1980, 2015)) %>% 
        group_by(country) %>%
        arrange(p_year) %>%
        summarize(change = diff(share)) %>%
        arrange(-change)

############################################################################
#### TABLE F.6. (Appendix): Regional Concentration of Inventor Groups ######
############################################################################

# specify parameters:
COUNTRIES <- list("US" = "AngloSaxon", "GB" = "AngloSaxon",
                  "FR" = "French", "DE" = "German",
                  "IT" = "Italian")
NON_WESTERN_ORIGINS <- c("China", "India", "Slavic-Russian",
                         "Arabic", "Persian", "Turkey", "SouthEastAsia")
WESTERN_ORIGINS <- c("AngloSaxon", "German", "Italian",
                     "French", "Dutch", "EastEurope", 
                     "Scandinavian")
TIME_FRAME <- seq(2010, 2015)
TOP_N_REGIONS <- 3
plot_df <- data.frame()

for(g in c("domestic", "western", "non_western")){
  if(g == "domestic"){
    for(c in names(COUNTRIES)){
      do <- COUNTRIES[[c]]
      tmp <- reg_share_ctry(country = c, domestic_origin = do, 
                            origins = do, years = TIME_FRAME)
      tmp <- mutate(tmp, origin_group = g, hhi = sum(tmp$regio_share_sqrd))
      plot_df <- rbind(plot_df, tmp[1:TOP_N_REGIONS, ])
    }
  }else{
    if(g == "western"){
      ORIGIN_GROUP <- WESTERN_ORIGINS}else{
        ORIGIN_GROUP <- NON_WESTERN_ORIGINS}
    for (c in names(COUNTRIES)) {
      do <- COUNTRIES[[c]]
      tmp <- reg_share_ctry(country = c, domestic_origin = do, 
                            origins = ORIGIN_GROUP, years = TIME_FRAME)
      tmp <- mutate(tmp, origin_group = g, hhi = sum(tmp$regio_share_sqrd))
      plot_df <- rbind(plot_df, tmp[1:TOP_N_REGIONS, ])
    }
  }
}

# HHI
plot_df %>% distinct(Ctry_code, origin_group, hhi)

# number of regions
plot_df %>% group_by(Ctry_code) %>% summarise(n_regions = length(unique(Up_reg_label)))