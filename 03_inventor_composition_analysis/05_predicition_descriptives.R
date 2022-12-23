#################################################################
# Description:    Script to present descriptive statistics of   #
#                 the inventor predictions                      #
# Authors:        Matthias Niggli/CIEB UniBasel                 #
# Last revised:   16.08.2022                                    #
#################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")
library("pals")
library("xtable")

# directories  -----------------------------------------------------------------
path = "..." # your path to the repository directory
setwd(path)

###################
## Load the data ##
###################

# inventor information
inv_dat <- read.csv("00_data_and_model/data/05_example_inventors_dat.csv") # example dataset

# summarise and subset to unique names:
paste("Number of patent inventor entries: ", 
      nrow(inv_dat)) # Full sample: N = 8'543'459
paste("Number of unique patent inventor names: ", 
      length(unique(inv_dat$name))) # Full sample: N =  2'680'775
inv_dat <- inv_dat %>% distinct(name, .keep_all = TRUE)

######################
## Process the data ##
######################

# create a empty data.frame which will contain the ranked predictions of every inventor
plot_df <- inv_dat %>% select(origin)
cols <- paste0("rank_", seq(18))
for(col in cols){
  plot_df[, col] <- NA
}

# rank the origin predictions on the inventor level
plot_df[, -1] <- t(apply(inv_dat[, grepl("prob", names(inv_dat))], 1, 
                   FUN = function(x){sort(x, decreasing = TRUE)}))

################################################################
## Table A.5: Summary statistics for prediction probabilities ##
## within ranked origin classes (full sample)                 ##
################################################################

# get summary statistics for every class:
sum_stat <- data.frame(
  origin_class = colnames(plot_df[-1]),
  avg = unlist(lapply(plot_df[, -1], function(x) mean(x, na.rm = TRUE))),
  std = unlist(lapply(plot_df[, -1], function(x) sd(x, na.rm = TRUE))),
  pct_25 = unlist(lapply(plot_df[, -1], function(x) quantile(x, 0.25))),
  median = unlist(lapply(plot_df[, -1], function(x) median(x, na.rm = TRUE))),
  pct_75 = unlist(lapply(plot_df[, -1], function(x) quantile(x, 0.75))),
  minimum = unlist(lapply(plot_df[, -1], function(x) min(x, na.rm = TRUE))),
  maximum = unlist(lapply(plot_df[, -1], function(x) max(x, na.rm = TRUE)))
)
rownames(sum_stat) <- NULL
sum_stat[,-1] <- sapply(sum_stat[,-1], function(x) round(x, digits = 3))
sum_stat
# origin_class   avg   std pct_25 median pct_75 minimum maximum
# 1        rank_1 0.900 0.167  0.880  0.989  0.999   0.128   1.000
# 2        rank_2 0.063 0.107  0.001  0.006  0.076   0.000   0.499
# 3        rank_3 0.018 0.040  0.000  0.001  0.012   0.000   0.330
# 4        rank_4 0.008 0.019  0.000  0.000  0.004   0.000   0.241
# 5        rank_5 0.004 0.011  0.000  0.000  0.002   0.000   0.174
# 6        rank_6 0.002 0.007  0.000  0.000  0.001   0.000   0.129
# 7        rank_7 0.002 0.005  0.000  0.000  0.001   0.000   0.104
# 8        rank_8 0.001 0.003  0.000  0.000  0.001   0.000   0.086
# 9        rank_9 0.001 0.002  0.000  0.000  0.000   0.000   0.068
# 10      rank_10 0.000 0.002  0.000  0.000  0.000   0.000   0.062
# 11      rank_11 0.000 0.001  0.000  0.000  0.000   0.000   0.052
# 12      rank_12 0.000 0.001  0.000  0.000  0.000   0.000   0.044
# 13      rank_13 0.000 0.001  0.000  0.000  0.000   0.000   0.041
# 14      rank_14 0.000 0.000  0.000  0.000  0.000   0.000   0.030
# 15      rank_15 0.000 0.000  0.000  0.000  0.000   0.000   0.019
# 16      rank_16 0.000 0.000  0.000  0.000  0.000   0.000   0.017
# 17      rank_17 0.000 0.000  0.000  0.000  0.000   0.000   0.012
# 18      rank_18 0.000 0.000  0.000  0.000  0.000   0.000   0.008
# xtable(sum_stat, digits = 3)

################################################################
## Figure 1: Probability distribution for the three highest   ##
## ranked predicted origin classes (full sample)              ##
################################################################

print("Mean of the combined top-3 class probabilities across inventors:")
mean(rowSums(plot_df[, c("rank_1", "rank_2", "rank_3")]), na.rm = TRUE) # 0.980979
print("Median of the combined top-3 class probabilities across inventors:")
median(rowSums(plot_df[, c("rank_1", "rank_2", "rank_3")]), na.rm = TRUE) # 0.998607
print("Standard deviation of the combined top-3 class probabilities across inventors:")
sd(rowSums(plot_df[, c("rank_1", "rank_2", "rank_3")]), na.rm = TRUE) # 04902604
print("Minimum of the combined top-3 class probabilities across inventors:")
min(rowSums(plot_df[, c("rank_1", "rank_2", "rank_3")]), na.rm = TRUE) # 0.3449798
print("Maximum of the combined top-3 class probabilities across inventors:")
max(rowSums(plot_df[, c("rank_1", "rank_2", "rank_3")]), na.rm = TRUE) # 0.9999985
print("5th percentile of the combined top-3 class probabilities across inventors:")
quantile(rowSums(plot_df[, c("rank_1", "rank_2", "rank_3")]), na.rm = TRUE, 0.05) # 0.8889253

# plot the three highest classes:
res <- data.frame()
for(v in c("rank_1", "rank_2", "rank_3")){
  tmp <- plot_df[, c("origin", v)]
  names(tmp)[2] <- "prob"
  tmp$rank <- v
  res <- rbind(res, tmp)
}

ggplot(res, aes(x = rank, y = prob)) +
  geom_boxplot(outlier.shape = NA)+
  stat_summary(fun = mean, geom = "point", size = 2, shape = 4) +
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels = c("Highest", "2nd Highest", "3rd Highest")) +
  labs(x = "Predicted Class", y = "Predicted Probability") +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
        axis.line = element_line(),
        axis.title = element_text(face="bold",size=10))
#ggsave("/scicore/home/weder/nigmat01/paper_plots/joeg_revision_plots/top3_pred.png")
res <- NULL

############################################################
## Figure B.2: Distribution of the Highest Predicted      ## 
## Class Probability Across Ethnic Origins                ##
############################################################

print("Summary statistics for the highest prediction class by origin:")
plot_df %>% group_by(origin) %>% 
  summarise(avg_P_highest = mean(rank_1),
            median_P_highest = median(rank_1),
            sd_P_highest = sd(rank_1),
            pct_25 = quantile(rank_1, 0.25))
# origin           avg_P_highest median_P_highest sd_P_highest
# <chr>                    <dbl>            <dbl>        <dbl>
#   1 AngloSaxon               0.888            0.976       0.166 
# 2 Arabic                   0.815            0.915       0.208 
# 3 Balkans                  0.716            0.736       0.236 
# 4 China                    0.913            0.983       0.138 
# 5 Dutch                    0.867            0.962       0.180 
# 6 EastEurope               0.800            0.903       0.223 
# 7 French                   0.889            0.990       0.178 
# 8 German                   0.877            0.964       0.168 
# 9 Hispanic-Iberian         0.902            0.994       0.176 
# 10 India                    0.923            0.997       0.159 
# 11 Italian                  0.912            0.997       0.166 
# 12 Japan                    0.992            0.999       0.0493
# 13 Korea                    0.917            0.986       0.141 
# 14 Persian                  0.836            0.955       0.210 
# 15 Scandinavian             0.880            0.991       0.186 
# 16 Slavic-Russian           0.896            0.992       0.178 
# 17 SouthEastAsia            0.756            0.788       0.211 
# 18 Turkey                   0.821            0.963       0.228 

# plot the distribution per ethnic origin:
ggplot(plot_df, aes(x = origin, y = rank_1)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", size = 2, shape = 4) +
  scale_y_continuous(breaks = c(0.3, 0.5, 0.7, 0.9), labels = scales::percent)+
  labs(x = "Ethnic Origin", y = "Predicted Origin Probability") +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
        axis.line = element_line(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(face="bold",size=10))
# ggsave("/scicore/home/weder/nigmat01/paper_plots/joeg_revision_plots/pmax_origin.png")

#########################################################################
## Figure B.3: Distribution of the Distance Between Inventors’ Highest ## 
## and 2nd-Highest Predicted Class Probability Across Ethnic Origins   ##
#########################################################################

# second highest origin prediction: ---------------------
plot_df %>% group_by(origin) %>% 
  summarise(avg_P_2ndhighest = mean(rank_2),
            sd_P_2ndhighest = sd(rank_2))
# origin           avg_P_2ndhighest sd_P_2ndhighest
# <chr>                       <dbl>           <dbl>
#   1 AngloSaxon                0.0698           0.107 
# 2 Arabic                    0.106            0.121 
# 3 Balkans                   0.151            0.128 
# 4 China                     0.0680           0.109 
# 5 Dutch                     0.0826           0.112 
# 6 EastEurope                0.112            0.126 
# 7 French                    0.0711           0.114 
# 8 German                    0.0781           0.111 
# 9 Hispanic-Iberian          0.0583           0.104 
# 10 India                     0.0445           0.0911
# 11 Italian                   0.0549           0.104 
# 12 Japan                     0.00434          0.0289
# 13 Korea                     0.0656           0.108 
# 14 Persian                   0.0973           0.123 
# 15 Scandinavian              0.0760           0.119 
# 16 Slavic-Russian            0.0595           0.102 
# 17 SouthEastAsia             0.171            0.148 
# 18 Turkey                    0.0892           0.117 

plot_df <- mutate(plot_df, dist_second = rank_1 - rank_2)
print("Correlation between mean value of highest and distance to 2nd highest predicted class:")
cor(plot_df$rank_1, plot_df$dist_second) # 0.9864385
plot_df %>% 
  group_by(origin) %>% 
  summarise(mean_dist_highest2ndhighest = mean(dist_second),
            mean_Pmax = mean(rank_1)) %>%
  summarise(corr = cor(mean_Pmax, mean_dist_highest2ndhighest, method = "pearson")) # 0.995

print("Overall mean value of distance to 2nd highest predicted class:")
mean(plot_df$dist_second) # 0.8370791
print("Overall median value of distance to 2nd highest predicted class:")
median(plot_df$dist_second) # 0.9832751
print("Overall sd value of distance to 2nd highest predicted class:")
sd(plot_df$dist_second) # 0.8370791
print("Overall minimum value of distance to 2nd highest predicted class:")
min(plot_df$dist_second) # 3.874302e-07
print("Overall maximum value of distance to 2nd highest predicted class:")
max(plot_df$dist_second) # 0.9999933

plot_df %>% group_by(origin) %>% 
  summarise(mean_dist_highest2ndhighest = mean(dist_second),
            sd_dist_highest2dnhighest = sd(dist_second))
# origin           mean_dist_highest2ndhighest sd_dist_highest2dnhighest
# <chr>                                  <dbl>                     <dbl>
#   1 AngloSaxon                             0.818                    0.268 
# 2 Arabic                                 0.709                    0.318 
# 3 Balkans                                0.565                    0.349 
# 4 China                                  0.845                    0.243 
# 5 Dutch                                  0.785                    0.286 
# 6 EastEurope                             0.687                    0.339 
# 7 French                                 0.817                    0.286 
# 8 German                                 0.799                    0.273 
# 9 Hispanic-Iberian                       0.844                    0.273 
# 10 India                                  0.879                    0.245 
# 11 Italian                                0.857                    0.265 
# 12 Japan                                  0.988                    0.0767
# 13 Korea                                  0.851                    0.245 
# 14 Persian                                0.738                    0.323 
# 15 Scandinavian                           0.804                    0.298 
# 16 Slavic-Russian                         0.837                    0.274 
# 17 SouthEastAsia                          0.585                    0.344 
# 18 Turkey                                 0.732                    0.336 

ggplot(plot_df, aes(x = origin, y = dist_second)) +
  geom_boxplot(outlier.shape = NA)+
  stat_summary(fun = mean, geom = "point", size = 2, shape = 4) +
  labs(x = "Ethnic Origin", y = "Distance Between 1st and 2nd\n Ranked Origin Probability") +
  scale_y_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9), labels = scales::percent)+
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
        axis.line = element_line(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(face="bold",size=10))
# ggsave("/scicore/home/weder/nigmat01/paper_plots/joeg_revision_plots/dist2nd_origin.png")

###################################################
## Figure B.4: Sample Composition by Ethnic      ##
## Origin for different Threshold Combinations   ##
###################################################

# preliminary analysis: number and share of
# samples with widely distributed probabilities across
# origin classes:
n_ambiguous <- plot_df %>%
  filter(rank_2 >= 0.25
         & rank_3 >= 0.2
         ) %>% nrow()
paste0(
  "Detected ", 
  n_ambiguous, " (", round(n_ambiguous / nrow(plot_df), 4) * 100, "%) ",
  "inventor names with prediction probabilities spread relatively equally across 3 highest classes")
# 147 (0.01%) for 0.4, 0.15
# 96 (0%) for 0.35, 0.25
# 917 (0.03%) for 0.35, 0.2
# 248 (0.01%) for 0.3, 0.3
# 6734 (0.25%) for 0.3, 0.2
# 4707 (0.18%) for 0.25, 0.25
# 16431 (0.61%) for 0.25, 0.2

n_ambiguous <- plot_df %>%
  filter(rank_1 <= 0.2
         & rank_2 >= 0.1
         & rank_3 >= 0.1
         & rank_4 >= 0.1
         & rank_5 >= 0.1) %>%
  nrow()
paste0(
  "Detected ", 
  n_ambiguous, " (", round(n_ambiguous / nrow(plot_df), 4) * 100, "%) ",
  "inventor names with prediction probabilities spread relatively equally across 5 highest classes")
# 2407 (0.09%) for 0.3, 0.1
# 1298 (0.05%) for 0.25, 0.1
# 290 (0.01%) for 0.2, 0.1
# => threshold values should not apply for many samples.

# get values of lowest ranked group by median of max_g(i,k):
tmp <- plot_df %>% group_by(origin) %>% 
  summarise(P25 = quantile(rank_1, 0.25),
            P50 = quantile(rank_1, 0.5),
            P75 = quantile(rank_1, 0.75),
            D25 = quantile(dist_second, 0.25),
            D50 = quantile(dist_second, 0.5),
            D75 = quantile(dist_second, 0.75)) %>%
  arrange(P50)

# threshold_values for the two metrics:
THRES_PMAX <- sort(as.numeric(tmp[1, grepl("P", names(tmp))]))
THRES_DIST2ND <- sort(as.numeric(tmp[1, grepl("D", names(tmp))]))

# get sample composition for different value combinations:
res <- plot_df %>% group_by(origin) %>% 
  summarise(n_inv = n(), share = n_inv / nrow(plot_df)) %>%
  mutate(threshold = "(A)\nNo thresholds", n_obs = nrow(plot_df))

for(t in 1:length(THRES_PMAX)){
  thresholds = paste0("(", LETTERS[t+1], ")
                      \nMinimum threshold for highest class probability: ", round(THRES_PMAX[t], 3), 
                      "\n Minimum threshold for distance of class probabilities: ", round(THRES_DIST2ND[t], 3))
  tmp <- plot_df %>% 
    filter(rank_1 >= THRES_PMAX[t] & dist_second >= THRES_DIST2ND[t])
  thresholds <- paste0(thresholds, "\n Remaining share of patent inventors: ", 
                       100 * round(nrow(tmp) / nrow(plot_df), 2), "%") 
  tmp <- tmp %>%
    group_by(origin) %>% 
    summarise(n_inv = n(), share = n_inv / nrow(tmp)) %>%
    mutate(threshold = thresholds, n_obs = nrow(tmp))
  res <- rbind(res, tmp)
}

# correlation with original sample shares:
for(t in unique(res$threshold)[-1]){
  x <- cor(
    res[res$threshold == "(A)\nNo thresholds", "share"], 
    res[res$threshold == t, "share"], 
    )
  print(paste("Correlation to original sample shares with threshold", substring(t, 1,3), ":", round(x,3)))
}
# [1] "Correlation to original sample shares with threshold (B) : 0.999"
# [1] "Correlation to original sample shares with threshold (C) : 0.993"
# [1] "Correlation to original sample shares with threshold (D) : 0.942"

# plot the sample compositions:
ggplot(res, aes(x = "", y = share, fill = origin)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  labs(x = "", y = "") +
  facet_wrap(facets = ~threshold) +
  scale_fill_manual(values = as.vector(glasbey(18)), name = "Ethnic Origin")+
  geom_text(aes(x = 1.6, label = paste0(100 * round(share, 2), "%")),
            size = 3, position = position_stack(vjust = 0.5), check_overlap = TRUE) +
  guides(fill=guide_legend(nrow = 2)) +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
        legend.position = "bottom",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank())
# ggsave("/scicore/home/weder/nigmat01/paper_plots/joeg_revision_plots/threshold_sim.png")

# total number of remaining inventors
res %>% distinct(threshold, .keep_all = TRUE) %>% select(n_obs) %>% mutate(remaining_share = n_obs / nrow(plot_df))
# n_obs remaining_share
# <int>           <dbl>
#   1 2680775           1    
# 2 2481482           0.926
# 3 2227939           0.831
# 4 1687226           0.629

# change of sample share by ethnic origin classes
tmp <- res %>% 
  filter(grepl(pattern = "(A)", threshold)) %>% 
  select(origin, share, n_inv) %>% 
  rename(original_share = share, original_n_inv = n_inv) %>%
  merge(res, by = "origin") %>%
  mutate(share_diff = (share/original_share) -1,
         inv_diff = (n_inv/original_n_inv) -1,
         threshold = substring(threshold, 2,2)
         ) %>%
  select(origin, threshold, n_inv, inv_diff, share, share_diff) %>%
  arrange(-share_diff)

# Example: East Europe --------------
tmp %>% filter(origin == "EastEurope") %>% select(origin, threshold, share, share_diff)
# origin threshold      share share_diff
# 1 EastEurope         A 0.02374425  0.0000000
# 2 EastEurope         B 0.02091492 -0.1191586
# 3 EastEurope         C 0.01832635 -0.2281773
# 4 EastEurope         D 0.01548637 -0.3477846

###############################################
## Figure B.5: Prevalence vs. Classification ##
###############################################

origin_dist <- inv_dat %>% 
  group_by(origin) %>% 
  summarise(share = n() / nrow(inv_dat)) %>%
  mutate(type = "Inventor Origin Share Without Threshold Conditions")
tmp <- inv_dat %>% 
  select(origin, contains("prob_")) %>%
  summarise_at(vars(contains("prob_")), funs(prevalence = sum)) %>%
  t() %>%
  data.frame()
origin_dist <- data.frame(origin = gsub("prob_|_prevalence", "", rownames(tmp)),
                  share = tmp[,1] / nrow(inv_dat)) %>%
  mutate(type = "Ethnic Origin Prevalence Across Inventors") %>%
  rbind(origin_dist)
origin_dist<- plot_df %>% 
  filter(rank_1 >= THRES_PMAX[2], dist_second >= THRES_DIST2ND[2]) %>%
  group_by(origin) %>%
  summarise(count = n()) %>%
  mutate(share = count /sum(count), 
         type = "Inventor Origin Share With Threshold Conditions") %>%
  select(-count) %>%
  rbind(origin_dist)

ggplot(origin_dist, aes(x = origin, y = share, fill = type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = as.vector(glasbey(3)), name = "Approach") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.35)) +
  guides(fill = guide_legend(nrow = 3)) +
  labs(y = "Inventor Origin Shares and \n Ethnic Origin Prevalence", x = "Ethnic Origin") +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(),
        axis.title = element_text(face="bold",size=10))
# ggsave("/scicore/home/weder/nigmat01/paper_plots/joeg_revision_plots/class_prev.png")


