#######################################################################
# Description:    Script to determine optimal probability thresholds  #
#                 for xgb model                                       #
# Authors:        Matthias Niggli/CIEB UniBasel                       #
# Date:           11.03.2020                                          #
#######################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")
library("plotly")
library("reshape2")

# directories  -----------------------------------------------------------------
path = "..." # your path to the repository directory
setwd(path)
print("Directories specified")

###################################
#### Load and process data ########
###################################

eval_metrics <- read.csv("/01_create_training_dataset/df0_evaluation_indicator_thresholds.csv")[,-1]
eval_metrics <- eval_metrics %>% mutate(id = paste0(min_proba, min_distance, max_entropy))
eval_group_shares <- read.csv("/01_create_training_dataset/df1_evaluation_indicator_groupsize.csv")[,-1]
eval_group_shares <- eval_group_shares %>% mutate(id = paste0(min_proba, min_distance, max_entropy))

# calculate variance of the ethnic origin shares depending on the threshold
tmp <- eval_group_shares %>% group_by(id) %>% summarize(var_group_shares = var(sample_share))

# calculate the combined sample share of the smalles two ethnic origins
min_shares <- eval_group_shares %>% group_by(id) %>% arrange(sample_share) %>% 
  slice(6) %>% summarize(min_shares = mean(sample_share))
tmp <- merge(tmp, min_shares, by = "id")
eval_metrics <- merge(eval_metrics, tmp, by = "id")

# normalize overall sample size, acc and var_group_shares for total score:
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
eval_metrics <- eval_metrics %>% 
  mutate(f1_norm = min_max_norm(f1), sample_fraction_norm = min_max_norm(sample_fraction), 
         inverse_var_group_shares_norm = min_max_norm(1/var_group_shares),
         min_shares_norm = min_max_norm(min_shares))

#################################################
#### Choose weights and calculate scores ########
#################################################

(weight_f1 = 0.5)

sample_importance = 1 - weight_f1
size_weight = 0.5

(weight_sample_size = sample_importance * size_weight)
(weight_group_shares = sample_importance * (1 - size_weight) / 2)
(weight_min_shares = weight_group_shares)

eval_metrics <- eval_metrics %>% 
  mutate(score = f1_norm * weight_f1 +
           sample_fraction_norm * weight_sample_size +
           inverse_var_group_shares_norm * weight_group_shares +
           min_shares_norm * weight_min_shares) %>%
  arrange(-score)
best_thresholds <- eval_metrics[1,]
print(best_thresholds)

################################################################
#### check actual ethnic origin shares for best_thresholds #####
################################################################

tmp <- eval_group_shares %>% filter(min_proba == best_thresholds$min_proba, 
                             min_distance == best_thresholds$min_distance,
                             max_entropy == best_thresholds$max_entropy)
tmp <- tmp %>% mutate(change = sample_share - baseline_share) %>%
  select(ethnic.origin, baseline_share, sample_share, change) %>% 
  arrange(change)
tmp

##################################
#### Check weight robustness #####
##################################

weight_f1 <- seq(0.3, 0.8, 0.02)
weight_sample_size <- weight_f1 * sample_importance * size_weight
weight_group_shares <- ((1 - (weight_f1 + weight_sample_size)) / 2)
weight_min_shares <- weight_group_shares

best_thresholds_id <- best_thresholds$id
grid <- cbind(weight_f1, weight_sample_size, weight_group_shares, weight_min_shares)
if(sum(rowSums(grid)) != nrow(grid)){warning("Weights incorrectly specified")}

count <- 0
N_top <- 5

for(i in 1:nrow(grid)){
  params <- grid[i, ]
  
  res <- eval_metrics %>% 
    mutate(score = f1_norm * params["weight_f1"] + 
             sample_fraction_norm * params["weight_sample_size"] +
             inverse_var_group_shares_norm * params["weight_group_shares"] + 
             min_shares_norm * params["weight_min_shares"]) %>%
    arrange(-score) %>% slice(1:N_top)

  if(best_thresholds_id %in% res$id){count <- count + 1}
  }

paste0("Best threshold is in the top ", N_top, " threshold combinations in ", 
       100 * round(count / nrow(grid), 3),
       "% of cases.")

###################################
#### Plot threshold and scores ####
###################################

plot_grid <- eval_metrics %>% 
  select(min_proba, min_distance, score) %>%
  distinct(min_proba, min_distance, .keep_all = TRUE)
plot_grid <- acast(plot_grid, min_proba ~ min_distance, value.var = "score")

p <- plot_ly(x = as.numeric(colnames(plot_grid)), 
        y = as.numeric(rownames(plot_grid)),
        z = plot_grid, type = "surface") %>%
  layout(scene = list(xaxis = list(title = "\u0394"),
                      yaxis = list(title = "P<sub>h</sub>"),
                      zaxis = list(title = "score")))
p