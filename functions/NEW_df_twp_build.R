library(nflreadr)
library(nflfastR)
library(sqldf)
library(openxlsx)
library(tidyr)
library(plyr)
library(dplyr)
library(tidyverse)
library(readxl)
library(flexclust)
library(factoextra)
library(NbClust)
library(xgboost)
library(caret)
library(caTools)
library(Metrics)
library(cvms)
library(ParBayesianOptimization)
library(doParallel)
library(stringr)
library(pROC)
library(olsrr)
library(stats)
library(ROCR)
library(gridExtra)
library(mgcv)

options(scipen = 999)

setwd("C:/Users/vflre/Downloads/NFL Models")


df_tip <- read.xlsx("df_time_in_pocket_all.xlsx")

df_twp_agg <- df_tip %>% group_by(team_name, Week, Season) %>% 
  dplyr::summarise(less_rate = sum(less_passing_snaps, na.rm = T) / (sum(less_passing_snaps, na.rm = T) + sum(more_passing_snaps, na.rm = T)),
                   less_twp_rate = sum(less_passing_snaps * less_twp_rate, na.rm = TRUE) / sum(less_passing_snaps, na.rm = TRUE) / 100,
                   more_twp_rate = sum(more_passing_snaps * more_twp_rate, na.rm = TRUE) / sum(more_passing_snaps, na.rm = TRUE) / 100,
                   less_int_rate = sum(less_interceptions, na.rm = T) / sum(less_passing_snaps, na.rm = T),
                   more_int_rate = sum(more_interceptions, na.rm = T) / sum(more_passing_snaps, na.rm = T))


df_twp_agg_stats <- df_twp_agg %>% 
  mutate(twp_rate = less_rate * less_twp_rate + (1 - less_rate) * more_twp_rate,
         int_rate = less_rate * less_int_rate + (1 - less_rate) * more_int_rate) %>% 
  select(team_name, Week, Season, twp_rate, int_rate)
